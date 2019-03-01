#*********************************************
#*********************************************
#' Writes a Simrad raw file.
#'
#' @param data  is a list of the data to write to the Simrad raw file.
#' @param con  is the path to the raw file.
#' @param header  is a list of the header to write to the Simrad raw file.
#' @param t is a vector of the time steps to write. NA
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#' @param msg  is TRUE to print a time bar during reading.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD dim_all interpret.mtim strff
#'
#' @export
#' @rdname writeEKRaw
#'
writeEKRaw <- function(data, con, header=NULL, t=1, endian="little", msg=TRUE){
	
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	
	if(is.list(data) && all(c("data", "header") %in% names(data))){
		header = data$header
		data = data$data
	}
	
	DataTypeChar = c("NME0", "TAG0", "CON1", "SVP0")
	DataName = c("NMEA", "annotations", "conf", "svp")
	AcousticType = c("RAW0", "RAW1")
	raw = 0
	if(length(data$pings$gaintx)>0){
		raw = 1
	}
	validDatagramNames = c(DataTypeChar, AcousticType[raw+1])
	validDataNames = c(DataName, "pings")
	#validRawDatagrams = c("RAW0", "RAW1")
	nBytesDgHeader = 12
	nBytesConfigHeader = 516
	nBytesTransceiverCount = 320
	nBytesHeader = 8
	nBytesSampledataInfo = c(72, 132)[raw+1]
	
	nNMEA = 0
	nTAG = 0
	# Ping index number:
	pind = 0
	fileSize = file.info(con)$size
	previousTime = -Inf
	
	# 't' may be given as "all", indicating Inf (all time steps):
	if(TSD::strff("all", t)){
		t = Inf
	}
	# Very large values of 't' are interpreted as 
	if(any(nchar(t)>4)){
		mtim = interpret.mtim(t)
		t = c(0, Inf)
	}
	else{
		mtim = c(0, Inf)
	}
	
	# Open the raw file:
	fid = file(con, "wb")
	
	
	# Write the header:
	headerLen = nBytesDgHeader + nBytesConfigHeader + (header$transceivercount * nBytesTransceiverCount)
	
	#  Write the header:
	# > > > > > > > > > > #
	writeEKRaw_WriteHeader(fid=fid, config=list(header=header, transceiver=data$config), dgLen=headerLen, dgType="CON0", dgTime=header$time, endian=endian)
	
	# < < < < < < < < < < #
	# Detect whether the acoustic data are given in arrays with dimension [lenb, numb, numt], where lenb = 1 for beam meta data:
	#if(length(data$pings[[1]])>0 && !is.list(data$pings[[1]])){
	if(length(data$pings[[1]])>0){
		
		# Get the channel indices:
		data$pings$channel <- row(data$pings$frequency)
		
		# Change made by Holmin at 2017-02-08:
		# Split the data into lists of datagrams, one for each channel of each ping:
		# Funciton used for splitting data into singe datagrams:
		applyLastDimAsChannel = function(x){
			# Split by last dimension, assumed to be channels:
			splitOnePingByLastDimension <- function(x){
				if(length(dim(x))<2){
					dim(x) = c(1, length(x))
				}
				# If three or more dimensions, collapse to a matrix:
				else{
					dim(x) = c(dim(x)[1], prod(dim(x)[-1]))
				}
				split(x, rep(seq_len(dim(x)[2]), each=dim(x)[1]))
			}
			# Split each list element and flatten the list:
			if(is.list(x)){
				x <- lapply(x, splitOnePingByLastDimension)
				unlist(x, recursive=FALSE)
			}
			# If vector or matrix, assume one element per channel:
			else{
				splitOnePingByLastDimension(x)
			}
		}
		
		# Split into datagrams in list:
		data$pings = lapply(data$pings, applyLastDimAsChannel)
		# Discard variables that do not have the correct number of channels:
		data$pings = data$pings[sapply(data$pings, function(xx) length(xx) == length(data$pings$channel))]
	}
	
	# Update data$pings$count (the lengths of the beams) by discarding missing values:
	acousticAndAngleDataNames = c("power", "athwartship", "alongship", "pings$data")
	dataPresent = sapply(data$pings[acousticAndAngleDataNames], length)
	dataPresent = which(dataPresent>0)
	if(length(dataPresent) == 0){
		warning("No acoustic data or angle data included in 'data'")
		data$pings$count = 0
	}
	else{
		data$pings$count = lapply(data$pings[[acousticAndAngleDataNames[dataPresent[1]]]], function(x) sum(!is.na(x)))
	}
	
	
	# Generate the writing order, based on the time points of each datagram:
	allmtim = list(unlist(data[[validDataNames[1]]]$time), unlist(data[[validDataNames[2]]]$time), unlist(data[[validDataNames[3]]]$time), unlist(data[[validDataNames[4]]]$time), data[[validDataNames[5]]]$time)
	allmtim_length = sapply(allmtim, length)
	# Create a matrix with the index in the vector 'validDatagramNames' in the first column, the position in each data gram type in the second column, and the MATLAB time in the third column:
	allmtim_ind = cbind(rep(seq_along(allmtim), allmtim_length), sequence(allmtim_length), unlist(allmtim))
	o = order(allmtim_ind[, 3])
	allmtim_ind = allmtim_ind[o, ]
	
	
	##### Write the file, processing individual datagrams: #####
	if(msg){
		# Plotting of time bar:
		infostring = "Writing the SIMRAD raw file:"
		cat(infostring, "\n", sep="")
		totalsteps = nrow(allmtim_ind)
		stepfact = nchar(infostring)/totalsteps
		oldvalue = 0
	}
	
	for(i in seq_len(nrow(allmtim_ind))){
	#	while(nBytesRead<fileSize){
	
		if(msg){
			# Print a dot if the floor of the new value exceeds the old value:
			thisvalue = floor(i*stepfact)
			if(thisvalue > oldvalue){
				cat(rep(".", thisvalue-oldvalue), if(i == totalsteps) "\n", sep="")
				oldvalue = thisvalue
			}
		}
		
		# If writing subsets - check if we're done:
		if(allmtim_ind[i, 3] > mtim[2]){
			break
		}
	
		# Extract the currently processed datagram:
		thisdatagramName = validDatagramNames[allmtim_ind[i, 1]]
		thisdataname = validDataNames[allmtim_ind[i, 1]]
		thismtim = allmtim_ind[i, 3]
	
		# Extract the data to write at the current step:
		if(thisdatagramName %in% DataTypeChar){
			if(length(data[[thisdataname]]$string)>0){
				thisdata = data[[thisdataname]]$string[allmtim_ind[i, 2]]
			}
			else if(length(data[[thisdataname]]$text)>0){
				thisdata = data[[thisdataname]]$text[allmtim_ind[i, 2]]
			}
			else{
				warning(paste("Missing 'text' or 'string' field in the list data$", thisdataname, sep=""))
			}
		}
		else{
			thisdata = lapply(data[[thisdataname]], function(xx) xx[[allmtim_ind[i, 2]]])
			#thisdata = sapply(data[[thisdataname]], writeEKRaw_extractNumericDatagram, allmtim_ind[i, 2])
		}
		thislength = getDgLen(thisdata, thisdatagramName, nBytesDgHeader, nBytesSampledataInfo)
		
		# Write the datagram length:
		# > > > > > > > > > > #
		writeBin(as.integer(thislength), con=fid, size=4, endian="little")
		# < < < < < < < < < < #
		
		# Write the datagram header:
		# > > > > > > > > > > #
		writeEKRaw_WriteDgHeader(fid, dgType=thisdatagramName, dgTime=thismtim, endian="little", tz="UTC")
		# < < < < < < < < < < #
		
		##### Process datagrams by type: #####
		# Process NMEA datagram:
		if(thisdatagramName %in% DataTypeChar){
			text = writeChar(thisdata, fid, nchars=thislength - nBytesDgHeader, eos=NULL)
		}
		# Process RAW0 datagram:
		else if(thisdatagramName == "RAW0"){
			writeEKRaw_WriteSampledata_RAW0(fid, thisdata, endian="little")
		}
		# Process RAW1 datagram:		
		else if(thisdatagramName == "RAW1"){
			writeEKRaw_WriteSampledata_RAW1(fid, thisdata, endian="little")
		}
		# Repeat the datagram length:
		# > > > > > > > > > > #
		writeBin(as.integer(thislength), con=fid, size=4, endian="little")
		# < < < < < < < < < < #
	}
	close(fid)
}
