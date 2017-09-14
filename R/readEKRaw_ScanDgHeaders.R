#*********************************************
#*********************************************
#' Modify parts of EK80/WBAT .raw files
#'
#' Provides the facility to modify parts of EK80/WBAT .raw files so that these files can be read by the Large Scale Survey System (LSSS) software.
#'
#' @param filename The path to directory of raw files or a vector of the paths to the raw files.
#' @param newDir The directory in which to put the modified files.
#' @param addMRU logical; if TRUE the funciton inserts a dummy MRU0 datagram prior to RAW3 datagrams.
#' @param fixBeamType logical; if TRUE the BeamType is fixed to 1.
#' @param fixSlope logical; if TRUE the slope is set to a default.
#' @param addTransducerSeralNumber A string giving the transducer serial number.
#' @param endian the endianness of the file, defaulted to "little".
#' @param msg logical: if TRUE print a time bar during file conversion.
#'
#' @return A vector of file names of the modified files.
#'
#' @importFrom TSD is.TSD
#' @importFrom tools file_ext
#'
#' @export
#' @rdname readEKRaw_ScanDgHeaders
#' 
readEKRaw_ScanDgHeaders<-function(filename, newDir, t="all", addMRU=TRUE, fixBeamType=TRUE, fixSlope=TRUE, endian="little", msg=TRUE){
	
	##### Preparation #####
	# Chech the name of the file and whether it is a TSD file:
	if(tolower(tools::file_ext(filename))!="raw"){
		warnings(paste0("The file ", filename, " does not have file extension \"raw\""))
		}
	if(TSD::is.TSD(filename)){
		stop(paste0("The file ", filename, " is a TSD file. Only SIMRAD raw files accepted"))
		}
	
	nBytesDgHeader = 12
	nBytesConfigHeader = 516
	nBytesTransceiverCount = 320
	nBytesHeader = 8
	nBytesSampledataInfo = 72
	nNMEA = 0
	nTAG = 0
	# Ping index number:
	pind = 0
	pingsnames = NULL
	fileSize=file.info(filename)$size
	previousTime=-Inf
	
	# Open the raw file:
	fid=file(filename, "rb")
	
	
	nBytesRead = 0
	
	##### Read the file, processing individual datagrams: #####
	totalsteps=file.info(filename)$size
	if(msg){
		# Plotting of time bar:
		infostring="Reading datagram headers from a Simrad raw file:"
		cat(infostring,"\n",sep="")
		stepfact=nchar(infostring)/totalsteps
		oldvalue=0
		}
	
	
	##### Execution and output #####
	out = NULL
	while(nBytesRead<fileSize){
		if(msg){
			# Print a dot if the floor of the new value exceeds the old value:
			thisvalue=floor(seek(fid)*stepfact)
			if(thisvalue > oldvalue){
				cat(rep(".",thisvalue-oldvalue))
				oldvalue=thisvalue
				}
			}
			
		# > > > > > > > > > > #
		len = readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
		# If the length of the datagram is 0, abort the reading:
		alreadyread = seek(fid)
		if(sum(len)==0 && alreadyread<totalsteps){
			warning(paste0("File ", filename, " was only partially read.", if(msg) paste0(" Number of bytes read: ", alreadyread, " of ", totalsteps, " (",  format(100*alreadyread/totalsteps,digits=1), " percent read).")))
			break
			}
		# < < < < < < < < < < #
		
		# Read the datagram header:
		# > > > > > > > > > > #
		dgHeader = readEKRaw_ReadDgHeader(fid, timeOffset=0, endian=endian)
		raw = seek(fid, where=len-nBytesDgHeader, origin="current")
		# < < < < < < < < < < #
		
		out = rbind(out, c(len, unlist(dgHeader)))
		
		nBytesRead = nBytesRead + len + 8 # 4 bytes each from reading the 'len' and 'lastlen', in total 8 bytes
		
		# Datagram length is repeated:
		# > > > > > > > > > > #
		lastLen = readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
		# < < < < < < < < < < #
		}
	
	close(fid)
	if(msg){
		cat("\n", sep="")
		}
	
	# Return the header and data:
	colnames(out) = c("Nbytes", "dgName", "dgTime")
	out
	##################################################
	##################################################
	}


