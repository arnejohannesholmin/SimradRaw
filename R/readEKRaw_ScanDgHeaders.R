#*********************************************
#*********************************************
#' Read all datagram headers of a Simrad raw file.
#'
#' @param filename The path to a raw file.
#' @param endian the endianness of the file, defaulted to "little".
#' @param msg logical: if TRUE print a time bar during file reading.
#'
#' @return A data frame of datagram info.
#'
#' @importFrom TSD is.TSD
#' @importFrom tools file_ext
#'
#' @export
#' @rdname readEKRaw_ScanDgHeaders
#' 
readEKRaw_ScanDgHeaders <- function(filename, endian="little", msg=TRUE){
	
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


