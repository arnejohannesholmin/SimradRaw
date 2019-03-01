#*********************************************
#*********************************************
#' Modify parts of EK80/WBAT .raw files
#'
#' Provides the facility to modify parts of EK80/WBAT .raw files so that these files can be read by the Large Scale Survey System (LSSS) software.
#'
#' @param filename The path to directory of raw files or a vector of the paths to the raw files.
#' @param newDir The directory in which to put the modified files.
#' @param t The pings to process.
#' @param addMRU logical; if TRUE the funciton inserts a dummy MRU0 datagram prior to RAW3 datagrams.
#' @param fixBeamType logical; if TRUE the BeamType is fixed to 1.
#' @param fixSlope logical; if TRUE the slope is set to a default.
#' @param addTransducerSerialNumber  Logical; if TRUE add transducer serial number to the xml datagram.
#' @param endian the endianness of the file, defaulted to "little".
#' @param msg logical: if TRUE print a time bar during file conversion.
#'
#' @return A vector of file names of the modified files.
#'
#' @importFrom TSD interpret.mtim is.TSD strff
#' @importFrom XML xml xmlTreeParse
#' @importFrom tools file_ext
#'
#' @export
#' @rdname readEKRaw_EK80modifyFile
#' 
readEKRaw_EK80modifyFile<-function(filename, newDir, t="all", addMRU=TRUE, fixBeamType=TRUE, fixSlope=TRUE, addTransducerSerialNumber="114", endian="little", msg=TRUE){
	
	# Functions for writing different kinds of data:
	writeDatagramRaw <- function(newfid, dgData, dgHeader, endian="little", nBytesDgHeader=12){
		# Write the datagram length:
		len = nBytesDgHeader + length(dgData)
		writeBin(as.integer(len), con=newfid, size=4, endian=endian)
		writeEKRaw_WriteDgHeader(newfid, dgType=dgHeader$dgType, dgTime=dgHeader$dgTime, endian=endian, tz="UTC")
		writeBin(dgData, newfid)
		writeBin(as.integer(len), con=newfid, size=4, endian=endian)
		}
	writeDatagramChar <- function(newfid, dgData, dgHeader, endian="little", nBytesDgHeader=12){
		# Write the datagram length:
		len = nBytesDgHeader + nchar(dgData)
		writeBin(as.integer(len), con=newfid, size=4, endian=endian)
		writeEKRaw_WriteDgHeader(newfid, dgType=dgHeader$dgType, dgTime=dgHeader$dgTime, endian=endian, tz="UTC")
		writeChar(dgData, newfid, eos=NULL)
		writeBin(as.integer(len), con=newfid, size=4, endian=endian)
		}
	writeDatagramMRU0 <- function(newfid, Heave, Roll, Pitch, Heading, dgHeader, endian="little", nBytesDgHeader=12){
		# Write the datagram length:
		len = nBytesDgHeader + 4*4
		writeBin(as.integer(len), con=newfid, size=4, endian=endian)
		writeEKRaw_WriteDgHeader(newfid, dgType=dgHeader$dgType, dgTime=dgHeader$dgTime, endian=endian, tz="UTC")
		writeBin(as.double(Heave), con=newfid, size=4, endian=endian)
		writeBin(as.double(Roll), con=newfid, size=4, endian=endian)
		writeBin(as.double(Pitch), con=newfid, size=4, endian=endian)
		writeBin(as.double(Heading), con=newfid, size=4, endian=endian)
		writeBin(as.integer(len), con=newfid, size=4, endian=endian)
		}
	readXML0 <- function(fid, len, nBytesDgHeader, fixBeamType, fixSlope){
		xml = readBin(fid, what="raw", len-nBytesDgHeader)
		xml = rawToChar(xml, multiple=FALSE)
		if(fixBeamType || fixSlope){
			xmlData = xmlTreeParse(xml)
			#xml <- xmlToList(xml)[[1]]
			if(length(xmlData$doc$children$Configuration)>0){
				if(fixBeamType){
					xml = gsub("BeamType=\"SPLIT\"", "BeamType=\"1\"", xml)
					}
				if(nchar(addTransducerSerialNumber)>0){
					str = paste0("<Transducer SerialNumber=\"", addTransducerSerialNumber, "\" ")
					xml = gsub("<Transducer ", str, xml)
					}
				}
			if(length(xmlData$doc$children$Parameter)>0 && fixSlope){
				#prexml = xml
				xml = gsub("Slope=\"Fast\"", "Slope=\"0.0\"", xml)
				xml = gsub("Slope=\"Slow\"", "Slope=\"0.50\"", xml) # As per email from Ivar
			}
			}
		xml
		}
	readXML0_old <- function(fid, len, nBytesDgHeader, fixBeamType, fixSlope){
		xml = readBin(fid, what="raw", len-nBytesDgHeader)
		xml = rawToChar(xml, multiple=FALSE)
		if(fixBeamType || fixSlope){
			xmlData = xmlTreeParse(xml)
			#xml <- xmlToList(xml)[[1]]
			if(length(xmlData$doc$children$Configuration)>0){
				if(fixBeamType){
					xml = gsub("BeamType=\"SPLIT\"", "BeamType=1", xml)
					}
				if(nchar(addTransducerSerialNumber)>0){
					str = paste0("<Transducer SerialNumber=", addTransducerSerialNumber, " ")
					xml = gsub("<Transducer ", str, xml)
					}
				}
			if(length(xmlData$doc$children$Parameter)>0 && fixSlope){
				#prexml = xml
				xml = gsub("Slope=\"Fast\"", "Slope=0.0", xml)
				xml = gsub("Slope=\"Slow\"", "Slope=0.50", xml) # As per email from Ivar
			}
			}
		xml
		}
	
	##### Preparation #####
	# Chech the name of the file and whether it is a raw file:
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
	
	# 't' may be given as "all", indicating Inf (all time steps):
	if(TSD::strff("all",t)){
		t=Inf
		}
	
	# Very large values of 't' are interpreted as Inf:
	if(any(nchar(t)>6)){
		mtim=interpret.mtim(t)
		t=c(0,Inf)
		}
	else{
		mtim=c(0,Inf)
		}
	
	# Open the raw file:
	fid=file(filename, "rb")
	suppressWarnings(dir.create(newDir))
	newfilename = file.path(newDir, basename(filename))
	newfid=file(newfilename,"wb")
	
	# Read configuration datagram 'CON0':
	# > > > > > > > > > > #
	# (DG1) Skip bytes:
	len = readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	# Read the first datagram header:
	# (DG2):
	dgHeader = readEKRaw_ReadDgHeader(fid, timeOffset=0, endian=endian)
	# (DG3):
	if(dgHeader$dgType == "CON0"){
		config = readEKRaw_ReadHeader(fid, dgHeader, endian=endian)
		numb = config$header$transceivercount
		writeEKRaw_WriteHeader(newfid, config=config, dgLen=len, dgType=dgHeader$dgType, dgTime=dgHeader$dgTime, endian=endian)
		}
	else if(dgHeader$dgType == "XML0"){
		xml = readXML0(fid=fid, len=len, nBytesDgHeader=nBytesDgHeader, fixBeamType=fixBeamType, fixSlope=fixSlope)
		config = list()
		numb = 1
		writeDatagramChar(newfid, dgData=xml, dgHeader=dgHeader, endian=endian)
		}
	else{
		warning("No CON0 or XML0 datagram at the beginning of the file.")
		raw = readBin(fid, what="raw", len-nBytesDgHeader)
		config = list()
		numb = 1
		writeDatagramRaw(newfid, dgData=raw, dgHeader=dgHeader, endian=endian)
		}
		
	# (DG4) Skip bytes:
	len = readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	# < < < < < < < < < < #
	
	nBytesRead = nBytesHeader + nBytesDgHeader + nBytesConfigHeader + if(length(config)) config$header$transceivercount * nBytesTransceiverCount else 0
	
	##### Read the file, processing individual datagrams: #####
	totalsteps=file.info(filename)$size
	if(msg){
		# Plotting of time bar:
		infostring="Modifying the WBAT raw file for reading in LSSS:"
		cat(infostring,"\n",sep="")
		stepfact=nchar(infostring)/totalsteps
		oldvalue=0
		}

	
	##### Execution and output #####
	clean = TRUE
	while(nBytesRead<=2*fileSize){
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
			clean = FALSE
			break
			}
		# < < < < < < < < < < #
		
		# Read the datagram header:
		# > > > > > > > > > > #
		dgHeader = readEKRaw_ReadDgHeader(fid, timeOffset=0, endian=endian)
		# < < < < < < < < < < #
		
		nBytesRead = nBytesRead + len + 8 # 4 bytes each from reading the 'len' and 'lastlen', in total 8 bytes
		
		# If reading subsets - check if we're done:
		#if(length(dgHeader$dgType)==0 || nchar(dgHeader$dgType)==0 || (length(dgHeader$dgTime) && dgHeader$dgTime > mtim[2])){
		if(length(dgHeader$dgType)==0 || (length(dgHeader$dgTime) && dgHeader$dgTime > mtim[2])){
			# Move file pointer back to beginning of the current datagram:
			#seek(fid, where=-(nBytesDgHeader+4), origin="current")
			break
			}
		
		##### Process datagrams by type: #####
		
		# Process XML0 datagram:
		if(dgHeader$dgType=="XML0"){
			xml = readXML0(fid=fid, len=len, nBytesDgHeader=nBytesDgHeader, fixBeamType=fixBeamType, fixSlope=fixSlope)	
			writeDatagramChar(newfid, dgData=xml, dgHeader=dgHeader, endian=endian)
			}
		else{
			raw = readBin(fid, what="raw", len-nBytesDgHeader)
			# Add a dummy MRU0 datagram prior to the RAW3 datagram:
			if(addMRU && dgHeader$dgType=="RAW3"){
				# The MRU0 datagram has four floats (16 bytes):
				MRUdgHeader = dgHeader
				MRUdgHeader$dgType = "MRU0"
				writeDatagramMRU0(newfid, Heave=0, Roll=0, Pitch=0, Heading=0, dgHeader=MRUdgHeader, endian=endian)		
				}
			writeDatagramRaw(newfid, dgData=raw, dgHeader=dgHeader, endian=endian)
			}
		
		
		# Datagram length is repeated:
		# > > > > > > > > > > #
		lastLen = readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
		# < < < < < < < < < < #
		}
	
	close(fid)
	close(newfid)
	if(msg){
		cat("\n", sep="")
		}
	
	# Return the header and data:
	newfilename
	##################################################
	##################################################
	}
