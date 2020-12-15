#*********************************************
#*********************************************
#' Reads a Simrad raw file.
#'
#' @param f				The path to the raw file.
#' @param t				A vector of the time steps to read.
#' @param endian		The endian of the file.
#' @param timeOffset	The time offset of the datagram.
#' @param drop.out		Logical: If TRUE drop dimensions of the data.
#' @param msg			Logical: If TRUE print a time bar during reading.
#' @param splitByPings	Logical: If TRUE split the acousic data by pings, which can avoid many NAs at the ends of the beams when the range shifts inside a file.
#' @param complex.out	Logical: If TRUE output the real and imaginary part usd to extract power (for fishery sonars).
#' @param skipRaw		Logical: If TRUE skip reading the RAW data (power). This reduces processing time to about 20 percent, useful if the acoustic data are not needed.
#' @param ...			Used for robustness.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD is.TSD NAs papply arr.ind2ind
#' @importFrom XML xml xmlParse
#' @importFrom tools file_ext
#' @importFrom data.table as.data.table data.table
#'
#' @export
#' @rdname readEKRaw
#'
readEKRaw <- function(f, t=1, endian="little", timeOffset=0, drop.out=FALSE, msg=TRUE, splitByPings=FALSE, complex.out=FALSE, skipRaw=FALSE, ...){
	
	ppp <- proc.time()
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	# Update: 2015-04-23 - Changed to return pings separately as elements of lists, and filling in NAs to form arrays in each ping, but keeping variable lengths between pings, in order to save space particularly when one ping has long beams and the others have short beams, in which case a lot of NAs will be saved.
	# Update: 2015-10-08 - Added support for reading non-UTF-8 characters.
	# Update: 2015-11-16 - Fixed bug when reading files with long difference in time between beams. Now trying to associate new pings by either that the idx reached numb, or that idx==1.
	# Last: 2016-10-23 - Changed to writing multiple temporary TSD files, speeding up for high number of pings.
	
	# Function for appending NAs:
	appendNA <- function(x, len){
		c(x, TSD::NAs(len - length(x)))[seq_len(len)]
	}
	
	# Function for removing trailing NAs for all beams in a ping of acoustic data:
	stripNAsAtBeamEnd <- function(x){
		allNA <- apply(is.na(x), 1, all)
		if(any(allNA)){
			# Only strip from the end of the beam:
			if(allNA[length(allNA)]){
				# Get the position where the reversed allNA moves fromTRUE to FALSE (diff -1):
				posFromEnd <- which(diff(rev(allNA)) == -1)
				# Return NULL in th unlikely event that all values are NA:
				if(length(posFromEnd)==0){
					return(NULL)
				}
				posFromEnd <- min(posFromEnd)
				# Get the sequence to extract samples along the beam by:
				valid <- seq_len(length(allNA) - posFromEnd)
				x <- x[valid, , drop=FALSE]
			}
		}
		x
	}
	
	# Function for splitting an array by its last dimension (e.g. representing time), and keeping the first dimensions:
	splitByLastDim <- function(x, ind){
		dimx <- dim(x)
		firstdim <- dimx[-length(dimx)]
		lastdim <- dimx[length(dimx)]
		if(length(firstdim) == 0){
			out <- as.list(x)
		}
		else{
			# Create a split vector and split by the last dimension:
			ind <- rep(seq_len(lastdim), each=prod(firstdim))
			out <- split(x, ind)
			# Convert all pings to matrices:
			out <- lapply(out, array, dim=firstdim)
			# Strip off NAs:
			out <- lapply(out, stripNAsAtBeamEnd)
		}
	
		out
	}
	
	# Function that returns an array filled with NAs where there is no data:
	fillNA <- function(x, numb, numt, pingID){
		# Get the lengths of the data:
		lens <- unlist(lapply(x, length), use.names=FALSE)
		maxlens <- max(lens)
		
		# For those with length shorter than the maximum, append with NAs:
		if(!all(lens == lens[1])){
			x[lens != maxlens] <- lapply(x[lens != maxlens], appendNA, len=maxlens)
			
		}
		
		# Unlist and set dimension:
		x <- unlist(x, use.names=FALSE)
		if(lens[1] == 1){
			dim(x) <- c(numb, numt)
		}
		else{
			dim(x) <- c(maxlens, numb, numt)
		}
		#numb <- max(channelID)
		numt <- max(pingID)
		
		x
	}

	# Function for converting a list of RAW* datagrams to a list of arrays filled with NAs for unequal lengths (of the beams):
	#list2arrayAddNA_OneDatagram <- function(rawDatagram, data){
	list2arrayAddNA <- function(raw, numb, splitByPings=FALSE, drop.out=FALSE, transpose=TRUE){
		
		# Transpose the list from one element per channel to one element per variable:
		if(transpose){
			numraw <- length(raw)
			prenames <- names(raw[[1]])
			raw = data.table::as.data.table(raw)
			raw = as.list(data.table::data.table(t(raw)))
			names(raw) <- prenames
		}
		
		# Get ping indices as incemeneting when the 'channel' variable resets:
		# Support for channel not present:
		if(length(raw$channel)==0){
			raw$channel <- as.list(rep(1, numraw))
		}
		
		# Support for only one channel per ping:
		channelID <- unlist(raw$channel)
		if(all(unlist(raw$channel) == raw$channel[[1]])){
			pingID <- seq_along(raw$channel)
		}
		else{
			pingID <- c(1, diff(channelID))
			pingID <- pingID < 0
			pingID <- cumsum(pingID) + 1
		}
		#numb <- max(channelID)
		numt <- max(pingID)
		
		# Report a warning if there are pings with missing beams:
		numbPerPing <- table(pingID)
		if(!all(numbPerPing == numb)){
			differingPings <- which(numbPerPing != numb)
			warning("Unequal number of beams in file ", f, ": (ping ID ", paste(differingPings, collapse=", "), "). These beams are filled in with NAs")
			raw <- lapply(raw, fillInMissingChannels, numb=numb, channelID=channelID, pingID=pingID)
			# Reset the pingID and channelID to reflect data in all beams of all pings:
			pingID <- rep(seq_len(numb), each=numt)
			channelID <- rep(seq_len(numb), numt)
		}
		
		# Fill with NAs:
		raw <- lapply(raw, fillNA, numb=numb, numt=numt, pingID=pingID)
		
		# If required, split into single pings for the acosutic data (for compatibility with older versions):
		if(splitByPings){
			acosutic <- intersect(c("data", "power", "alongship_e", "athwartship_e"), names(raw))
			if(length(acosutic)){
				raw[acosutic] <- lapply(raw[acosutic], splitByLastDim)
			}
		}
		
		# Drop the empty dimensions if required:
		if(drop.out){
			raw <- lapply(raw, drop)
		}
	
		raw
	}
	
	
				#seq_acceptNA_expand <- function(start, end, pos=0){
				#	if(is.na(start) || is.na(end)){
				#		return(NULL)
				#	}
				#	else{
				#		out <- seq(start, end)
				#	}
                #
				#	out <- outer(out, pos, "+")
                #
				#	return(out)
				#}
	
	# Function to read one variable of a datagram given by an index 'i':
	#readOneVarOfDatagram <- function(i, x, schema){
	#	readBin(x[seq(schema$start[i], schema$end[i])], what=schema$what[i], n=schema$n[[i]], size=schema$size[i])
	#}
	
			## Function to get mode and count from a RAW0 datagram:
			#get_mode_count <- function(x){
			#	# Get the schema for only mode and count:
			#	schema <- readEKRaw_GetSchema(dgName="RAW0", var=c("mode_low", "mode_high", "count"))
		    #
			#	temp <- list(
			#		mode_low = readOneVarOfDatagram(1, x=x, schema=schema), 
			#		mode_high = readOneVarOfDatagram(2, x=x, schema=schema)
			#	)
			#	temp <- readEKRaw_getMode(temp)
		    #
			#	count = readOneVarOfDatagram(3, x=x, schema=schema)
		    #
			#	list(mode=temp$mode, count=count)
			#}
			#get_datatype_ncomplexpersample_count <- function(x){
			#	# Get the schema for only mode and count:
			#	schema <- readEKRaw_GetSchema(dgName="RAW1", var=c("datatype", "ncomplexpersample", "count"))
		    #
			#	datatype = readOneVarOfDatagram(1, x=x, schema=schema)
			#	ncomplexpersample = readOneVarOfDatagram(2, x=x, schema=schema)
			#	count = readOneVarOfDatagram(3, x=x, schema=schema)
		    #
			#	list(datatype=datatype, ncomplexpersample=ncomplexpersample, count=count)
			#}
	        #
			## Function for converting a list of datagrams given as raw vectors to a list of variables with one raw matrix per variable:
			#transposeDatagrams <- function(x, dgName="RAW0", info=list()){
			#	schema <- readEKRaw_GetSchema(dgName=dgName, x=info)
		    #
			#	# Funciton for extracting the raw matrix of one variable:
			#	extractRaw <- function(ind, x){
			#		if(length(ind) == 0){
			#			return(ind)
			#		}
			#
			#		array(x[ind], dim=dim(ind))
			#	}
		    #
			#	# Get the sequence of raw values for each variable for one datagram:
			#	pos <- c(0, cumsum(lengths(x))[-length(x)])
			#	out <- mapply(seq_acceptNA_expand, schema$start, schema$end, MoreArgs=list(pos=pos))
		    #
			#	out <- lapply(out, extractRaw, x=x)
		    #
			#	return(out)
			#}
	
	# Function for filling inn data for missing channels as empty list elements, which are then later filled with NAs:
	fillInMissingChannels <- function(x, numb, channelID, pingID){
		numt <- max(pingID)
		out <- vector("list", numb * numt)
		out[TSD::arr.ind2ind(cbind(channelID, pingID), shape=c(numb, numt))] <- x
		out
	}
	
	# Function for converting a raw vector to character and returning it in a list along with the dgTime attribute:
	readEKRaw_GetText <- function(x, ...){
		text <- rawToCharDotDotDot(x)
		x <- list(
			time = attr(x, "dgTime"), 
			text = text
		)
		x
	}
	# Function for converting a raw vector to an xml list:
	readEKRaw_GetXML <- function(x, ...){
		out <- rawToCharDotDotDot(x)
		out <- XML::xmlParse(out)
		out <- XML::xmlToList(out)
		if(!is.list(out)){
			out <- as.list(out)
		}
		out$time <- attr(x, "dgTime")
		out
	}
	# Function for converting a raw vector to an xml list:
	readEKRaw_GetMRU <- function(x, ...){
		out <- convertRaw(x, dgName="MRU0", endian=endian)
		out$time <- attr(x, "dgTime")
		out
	}
	# Function for converting the raw vector given datagram name:
	readEKRaw_getDatagram <- function(x, skipRaw=FALSE, ...){
		# Get the 'dgName' from the attributes of the first element:
		dgName <- attr(x[[1]], "dgName")
		
		if(!skipRaw && dgName[1] %in% c("RAW0", "RAW1")){
			schema <- readEKRaw_GetSchema(dgName[1], group=FALSE)
		}
		
		
		if(dgName %in% c("NME0", "TAG0", "SVP0", "CON1")){
			fun <- readEKRaw_GetText
		}
		else if(dgName %in% "XML0"){
			fun <- readEKRaw_GetXML
		}
		else if(dgName %in% "DEP0"){
			fun <- function(x, endian="little", ...){
				convertRaw(x, dgName="DEP0", endian=endian)
			}
		}
		#else if(dgName %in% "CDS0"){
		#	fun <- readEKRaw_GetXML
		#}
		else if(dgName %in% "RAW0"){
			if(skipRaw){
				fun <- function(x, ...) x
			}
			else{
				fun <- function(x, endian="little", ...){
					# readEKRaw_GetRAW(x, dgName="RAW0", endian=endian, ...)
					#convertRaw(x, dgName="RAW0", endian=endian, ...)
					convertRaw(x, dgName="RAW0", endian=endian, schema=schema, ...)
				}
			}	
		}
		else if(dgName %in% "RAW1"){
			if(skipRaw){
				fun <- function(x, ...) x
			}
			else{
				fun <- function(x, endian="little", complex.out=FALSE, ...){
					# readEKRaw_GetRAW(x, dgName="RAW1", endian=endian, complex.out=complex.out, ...)
					#convertRaw(x, dgName="RAW1", endian=endian, complex.out=complex.out, ...)
					convertRaw(x, dgName="RAW1", endian=endian, complex.out=complex.out, schema=schema, ...)
				}
			}
		}
		else if(dgName %in% "FIL1"){
			fun <- function(x, endian="little", ...){
				convertRaw(x, dgName="FIL1", endian=endian, ...)
			}
		}
		else if(dgName %in% "MRU0"){
			fun <- readEKRaw_GetMRU
		}
		else if(dgName %in% "CON0"){
			fun <- readEKRaw_GetFileHeader
		}
		else{
			warning("Unknown datagram name ", dgName, ". Read as text.")
			fun <- readEKRaw_GetText
		}
		
		# Apply the funciton over all elements:
		#out <- TSD::papply(x, fun, ...)
		out <- lapply(x, fun, ...)
	}
	
	
	## Function for converting the raw vector given datagram name:
	#readEKRaw_getDatagram_attemptOnTransposingListOfRawDatagramsToListOfVariables <- function(x, ...){
	#	# Get the 'dgName' from the attributes of the first element:
	#	dgName <- attr(x[[1]], "dgName")
	#	if(dgName %in% c("NME0", "TAG0", "SVP0", "CON1")){
	#		fun <- readEKRaw_GetText
	#	}
	#	else if(dgName %in% "XML0"){
	#		fun <- readEKRaw_GetXML
	#	}
	#	else if(dgName %in% "DEP0"){
	#		fun <- function(x, endian="little", ...){
	#			convertRaw(x, dgName="DEP0", endian=endian)
	#		}
	#	}
	#	#else if(dgName %in% "CDS0"){
	#	#	fun <- readEKRaw_GetXML
	#	#}
	#	else if(dgName %in% "RAW0"){
	#		fun <- function(x, endian="little", ...){
	#			# readEKRaw_GetRAW(x, dgName="RAW0", endian=endian, ...)
	#			convertRaw(x, dgName="RAW0", endian=endian, ...)
	#		}
	#		
	#		# Extract all time steps for each variable:
	#		info <- get_mode_count(x[[1]])
	#		x <- transposeDatagrams(x, dgName="RAW0", info=info)
	#	}
	#	else if(dgName %in% "RAW1"){
	#		fun <- function(x, endian="little", complex.out=FALSE, ...){
	#			# readEKRaw_GetRAW(x, dgName="RAW1", endian=endian, complex.out=complex.out, ...)
	#			convertRaw(x, dgName="RAW1", endian=endian, complex.out=complex.out, ...)
	#		}
	#		
	#		# Extract all time steps for each variable:
	#		info <- get_datatype_ncomplexpersample_count(x[[1]])
	#		x <- transposeDatagrams(x, dgName="RAW1", info=info)
	#	}
	#	else if(dgName %in% "FIL1"){
	#		fun <- function(x, endian="little", ...){
	#			convertRaw(x, dgName="FIL1", endian=endian, ...)
	#		}
	#	}
	#	else if(dgName %in% "MRU0"){
	#		fun <- readEKRaw_GetMRU
	#	}
	#	else if(dgName %in% "CON0"){
	#		fun <- readEKRaw_GetFileHeader
	#	}
	#	else{
	#		warning("Unknown datagram name ", dgName, ". Read as text.")
	#		fun <- readEKRaw_GetText
	#	}
	#	
	#	# Apply the funciton over all elements:
	#	out <- lapply(x, fun, ...)
	#	
	#	return(out)
	#}
	#
	
	
	
	
	
	
	
	
	########## File: ##########
	# Chech the name of the file and whether it is a TSD file:
	if(tolower(tools::file_ext(f))!="raw"){
		warnings(paste0("The file ",f, " does not have file extension \"raw\""))
	}
	if(TSD::is.TSD(f)){
		stop(paste0("The file ",f, " is a TSD file. Only SIMRAD raw files accepted"))
	}
	####################
	
	# Read the raw file as raw:
	temp <- readEKRaw_ScanDgHeaders(f, endian=endian, msg=msg)
	
	rawFileFormat <- readEKRaw_GetRawFileFormat(temp$dg)
	
	
	
	########## First: ##########
	# Extract the first datagram as either CON0 (rAW0 and RAW1) or XML0 (RAW3):
	if(any(temp$dg$dgName == "CON0")){
		atCON0 <- temp$dg$dgName == "CON0"
		atCON0 <- seq(temp$dg$starts[atCON0], temp$dg$end[atCON0])
		config <- readEKRaw_GetFileHeader(temp$raw[atCON0])
		# Get the number of beams:
		numb <- config$header$transceivercount
	}
	else if(any(temp$dg$dgName == "XML0")){
		atXML0 <- which(temp$dg$dgName == "XML0")[1]
		atXML0 <- seq(temp$dg$starts[atXML0], temp$dg$end[atXML0])
		suppressWarnings(config <- readEKRaw_GetXML(temp$raw[atXML0]))
		# Get the number of beams:
		numb <- length(config$Transceivers[names(config$Transceivers) == "Transceiver"])
	}
	else{
		stop("No CON0 or XML0 datagram at the beginning of the file.")
	}
	####################
	
	
	########## Time: ##########
	# First perform a check for incomplete pings:
	pind <- TSD::NAs(nrow(temp$dg))
	atRaw <- startsWith(temp$dg$dgName, "RAW")
	numRaw <- sum(atRaw)
	if(numRaw %% numb != 0){
		warning("There may be incomplete pings in the file ", f, " resulting in possibly inconsistency in the number of pings read compared to the value of 't'.")
	}
	numt <- numRaw / numb
	
	# Identify ping indices by the sequence of RAW datagrams:
	pind[atRaw] <- rep(seq_len(numt), each=numb)
	pind <- approx(x=seq_along(pind), y=pind, xout=seq_along(pind), method="constant", rule=2)$y
	
	# Treat the specified time steps:
	if(is.character(t) && startsWith(t, "all")){
		# Read all datagrams if t="all":
		ind <- seq_len(nrow(temp$dg))
	}
	# Very large values of 't' are interpreted as mtim, utim or ftim:
	else if(any(nchar(t)>6)){
		mtim <- TSD::interpret.mtim(t)
		ind <- which(temp$dg$dgTime >= min(mtim) & temp$dg$dgTime <= max(mtim))
	}
	else{
		ind <- which(pind %in% t)
	}
	####################
	
	
	########## Raw vectors: ##########
	# Get the raw vectors of each requested datagram:
	data <- readEKRaw_SplitRawVector(temp, ind=ind)
	
	# Add the Matlab times as names to the list of raw vectors for convenience:
	names(data$raw) <- data$dg$dgName
	for(i in seq_along(data$raw)){
		attr(data$raw[[i]], "dgTime") <- data$dg$dgTime[i]
		attr(data$raw[[i]], "dgName") <- data$dg$dgName[i]
	}
	
	# Split the list of raw vectors	by datagram name:
	data <- split(data$raw, data$dg$dgName)
	####################
	
	# Exclude the first datagram, which has already been read:
	#data <- data[-1]
	
	# Apply the conversion given datagram name:
	data <- TSD::papply(data, readEKRaw_getDatagram, endian="little", timeOffset=0, xBase=-11644473600, complex.out=complex.out, skipRaw=skipRaw)
	
	
	########## Header: ##########
	## Interpret the CON0 datagram:
	#if(any(names(data) == "CON0")){
	#	# Move the CON0 datagram to a 'config' list:
	#	config <- data$CON0[[1]]
	#	#data$CON0 <- NULL
	#	# Get the number of beams:
	#	numb <- config$header$transceivercount
	#}
	#else if(any(names(data) == "XML0")){
	#	# Move the CON0 datagram to a 'config' list:
	#	config <- data$XML0[[1]]
	#	#data$CON0 <- NULL
	#	# Get the number of beams:
	#	numb <- length(config$Transceivers)
	#}
	#else{
	#	stop("No CON0 or XML0 datagram at the beginning of the file.")
	#	config <- list()
	#	numb <- 1
	#}
	
	# Add the transceiver configuration to the data:
	data$config <- readEKRaw_getDataConfig(config=config, rawFileFormat=rawFileFormat)
	#data$config <- config$transceiver
	header <- readEKRaw_getHeader(config=config, data=data, rawFileFormat=rawFileFormat)
	
	# Get environment data from an XML0 datagram and remove this datagram:
	data <- readEKRaw_getEnvironment(data)
	
	# Exclude the first datagram, which has already been read:
	if(any(names(data) == "CON0")){
		data <- readEKRaw_removeFirst(data, "CON0")
	}
	else if(any(names(data) == "XML0")){
		#data <- readEKRaw_removeFirst(data, "XML0")
		data <- readEKRaw_UsedXML0(data)
	}
	####################
	
	
	# Give a warning if there are more than one type of raw data:
	atRawDatagram <- startsWith(names(data), "RAW")
	rawDatagram <- names(data)[atRawDatagram]
	if(length(rawDatagram) == 0){
		warning("The file ", f, " does not contain any RAW0 or RAW1 datagram, which are the only RAW-datagrams currently supported by readEKRaw()")
	}
	else if(length(rawDatagram) > 1){
		warning("Only one RAW-datagram supported")
	}
	
	
	# Convert to a list of variables for the RAW* datagrams, and rename to "pings" according to the original Matlab structure by dr. Rick Towler, NOAA Alaska Fisheries Science Center:
	# data[[rawDatagram]] <- list2arrayAddNA(data[[rawDatagram]], numb=numb, splitByPings=splitByPings, drop.out=drop.out, transpose=FALSE) # Not a successful attempt to used transpose=FALSE, sinfe there are bugs with this in list2arrayAddNA():
	if(!skipRaw){
		data[[rawDatagram]] <- list2arrayAddNA(data[[rawDatagram]], numb=numb, splitByPings=splitByPings, drop.out=drop.out, transpose=TRUE)
		names(data)[atRawDatagram] <- "pings"
	}
	
	# Reset the ping index 'number' to the ping indices:
	data$pings$number <- if(length(dim(data$pings$number))==0) seq_along(data$pings$number) else col(data$pings$number)
	numt <- max(data$pings$number)
	
	if(length(data$DEP0)){
		data$DEP0 <- list2arrayAddNA(data$DEP0, numb=1, splitByPings=splitByPings, drop.out=drop.out, transpose=TRUE)
		data$DEP0 <- as.data.frame(data$DEP0, stringsAsFactors=FALSE)
	}
	if(length(data$MRU0)){
		data$MRU0 <- list2arrayAddNA(data$MRU0, numb=1, splitByPings=splitByPings, drop.out=drop.out, transpose=TRUE)
		data$MRU0 <- as.data.frame(data$MRU0, stringsAsFactors=FALSE)
	}
	
	
	
	# Unlist NMEA:
	if(any(names(data) == "NME0")){
		# Add time and string:
		data$NMEA$time <- sapply(data$NME0, "[[", "time", USE.NAMES=FALSE)
		data$NMEA$string <- sapply(data$NME0, "[[", "text", USE.NAMES=FALSE)
		data$NME0 <- NULL
	}
	# Unlist TAG0:
	if(any(names(data) == "TAG0")){
		# Add time and string:
		data$annotations$time <- sapply(data$TAG0, "[[", "time", USE.NAMES=FALSE)
		data$annotations$text <- sapply(data$TAG0, "[[", "string", USE.NAMES=FALSE)
		data$TAG0 <- NULL
	}
	
	
	# Order the data by name to resemble the old version:
	data <- data[order(names(data))]
	
	
	# Return the header and data:
	list(header=header, data=data, numt=numt, numb=numb, dg=temp$dg)
	##################################################
	##################################################
}
#'
#' @export
#'
readEKRaw_GetRawFileFormat <- function(x){
	# The RAW0-fileformat contains both CON0 and RAW0:
	if(all(c("CON0", "RAW0") %in% x$dgName)){
		rawFileFormat <- 0
	}
	else if(all(c("CON0", "RAW1") %in% x$dgName)){
		rawFileFormat <- 1
	}
	else if(all(c("RAW2") %in% x$dgName)){
		rawFileFormat <- 2
	}
	else if("XML0" %in% x$dgName && any(c("RAW0", "RAW3") %in% x$dgName)){
		rawFileFormat <- 3
	}
	else{
		warning("Unsupported rawFileFormat")
	}
	rawFileFormat
}


#*********************************************
#*********************************************
#' Read only vessel data from a Simrad raw file.
#'
#' @param f				The path to the raw file.
#' @param NMEA.out		logiacl: If TRUE retun a list of the NMEA strings and the vessel info.
#'
#' @export
#'
readAllNMEA <- function(f, NMEA.out = FALSE) {
	# Scan the file:
	raw <- readEKRaw_ScanDgHeaders(f)
	# Convert all NMEA to string:
	NMEA <- sapply(which(raw$dg$dgName == "NME0"), function(at) rawToChar(raw$raw[seq(raw$dg[at, "starts"], raw$dg[at, "ends"])]))
	
	vessel <- NMEA2vessel(NMEA)
	vessel$ftim <- TSD::mtim2ftim(vessel$imtm)
	
	if(NMEA.out) {
		return(
			list(
				NMEA = NMEA, 
				vessel = vessel
			)
		)
	}
	else {
		return(vessel)
	}
	
	
}


# Function for converting mode_high and mode_low to mode:
readEKRaw_getMode <- function(x, ...){
	if(length(x$mode_high) && length(x$mode_high)){
		x$mode <- 256 * x$mode_high + x$mode_low
		# Remove the mode components:
		x$mode_high <- NULL
		x$mode_low <- NULL
	}
	x
}
# Function for converting fishery sonar data in complex form to power:
readEKRaw_complex2power <- function(x, complex.out=FALSE, ...){
	if(length(x$data)){
		dim(x$data) <- c(2, x$ncomplexpersample, x$count)
		# Save as complex type:
		x$data <- complex(real=x$data[1,,], imaginary=x$data[2,,])
		dim(x$data) <- c(x$ncomplexpersample, x$count)
		# Sum over the number of complex values per sample, and take the square of the abs, which is the same as the sum of the real part squared and the imaginary part squared:
		x$power <- 10*log10(abs(colSums(x$data))^2)
		
		# Remove the complex
		if(!complex.out){
			x$data <- NULL
		}
	}
	x
}
# Function for converting getting athwartship and alongship angles:
readEKRaw_getAngles <- function(x, ...){
	if(length(x$angle)){
		x$angle <- matrix(x$angle, nrow=x$count, ncol=2, byrow=TRUE)
		x$athwartship <- x$angle[,1]
		x$alongship <- x$angle[,2]
		# Remove the angle
		x$angle <- NULL
	}
	x
}
# Function for converting getting athwartship and alongship angles:
readEKRaw_getPower <- function(x, ...){
	if(length(x$power0)){
		# Power * 10 * log10(2) / 256
		x$power <- x$power0 * 0.011758984205624
		# Remove the power0
		x$power0 <- NULL
	}
	x
}

readEKRaw_getEnvironment <- function(x){
	if(length(x$XML0)){
		atEnvironment <- which(sapply(x$XML0, function(x) length(x$.attrs["SoundSpeed"])>0))
		x$environment <- as.list(x$XML0[[atEnvironment]]$.attrs)
		x$environment <- lapply(x$environment, as.numeric)
		x$environment <- as.data.frame(x$environment)
		# Remove the envitonment form the list of XML0 datagrams:
		#x$XML0[[atEnvironment]] <- NULL
	}
	x
}

readEKRaw_removeFirst <- function(data, name){
	if(length(data[[name]])==1){
		data[[name]] <- NULL
	}
	else{
		data[[name]] <- data[[name]][-1]
	}
	data
}

readEKRaw_UsedXML0 <- function(data){
	atTransceivers <- which(sapply(data$XML0, function(x) "Transceivers" %in% names(x)))[1]
	atEnvironment <- which(sapply(data$XML0, function(x) "SoundSpeed" %in% names(x$.attrs)))[1]
	atVersion <- head(which(sapply(data$XML0, function(x) "version" %in% names(x))), 1)
	
	data$XML0 <- data$XML0[-c(atTransceivers, atEnvironment, atVersion)]
	
	data
}



readEKRaw_getDataConfig <- function(config, rawFileFormat){
	#if(length(config$transceiver)){
	#	out <- config$transceiver
	#}
	#else if(length(config$Transceivers)){
	#	out <- getDataConfigRaw3(config)
	#}
	if(rawFileFormat %in% 0:1){
		out <- config$transceiver
	}
	else if(rawFileFormat == 3){
		out <- getDataConfigRaw3(config)
	}
	else{
		warning("Unsupported rawFileFormat")
	}
	out
}

readEKRaw_getHeader <- function(config, data, rawFileFormat){
	if(rawFileFormat %in% 0:1){
		header <- config$header
	}
	else if(rawFileFormat == 3){
		header <- getHeaderRaw3(config=config, data=data)
	}
	else{
		warning("Unsupported rawFileFormat")
	}
	header	
}


splitByChar <- function(x, char=";", numeric=TRUE){
	if(is.character(x)){
		x <- strsplit(x, char)[[1]]
		if(numeric){
			x <- as.numeric(x)
		}
	}
	x
}
getTransducerVarOne <- function(x, var){
	if(is.list(x)){
		out <- x$.attrs[var]
	}
	else{
		out <- x[var]
	}
	out <- splitByChar(unname(out))
	out
}
getTransducerVar <- function(config, var){
	out <- lapply(config$Transceivers[names(config$Transceivers) == "Transceiver"], function(x) getTransducerVarOne(x$Channels$Channel$Transducer, var))
	if(length(out[[1]]) == 1){
		out <- unname(unlist(out))
	}
	else{
		out <- do.call(rbind, out)
	}
	out
}
getFrequencyPar <- function(config){
	getFrequencyParOne <- function(x){
		out <- x$Channels$Channel$Transducer
		out <- out[names(out) %in% "FrequencyPar"]
		if(length(out)){
			out <- lapply(out, function(x) {storage.mode(x) <- "numeric"; x})
			out <- do.call(rbind, out)
			rownames(out) <- NULL
		}
		else{
			out <- NA
		}
		out
	}
	out <- lapply(config$Transceivers[names(config$Transceivers) == "Transceiver"], getFrequencyParOne)
	out <- unname(out)
	out
}
getDataConfigRaw3 <- function(config){
	out <- list(
		channelid                   = sapply(config$Transceivers[names(config$Transceivers) == "Transceiver"], function(x) x$Channels$Channel$.attrs["ChannelID"]),
		beamtype                    = getTransducerVar(config, "BeamType"),
		frequency                   = getTransducerVar(config, "Frequency"),
		gain                        = getTransducerVar(config, "Gain"),
		equivalentbeamangle         = getTransducerVar(config, "EquivalentBeamAngle"),
		beamwidthalongship          = getTransducerVar(config, "BeamWidthAlongship"),
		beamwidthathwartship        = getTransducerVar(config, "BeamWidthAthwartship"),
		anglesensitivityalongship   = getTransducerVar(config, "AngleSensitivityAlongship"),
		anglesensitivityathwartship = getTransducerVar(config, "AngleSensitivityAthwartship"),
		anglesoffsetalongship       = getTransducerVar(config, "AngleOffsetAlongship"),
		angleoffsetathwartship      = getTransducerVar(config, "AngleOffsetAthwartship"),
		posx                        = NA,
		posy                        = NA,
		posz                        = NA,
		dirx                        = NA,
		diry                        = NA,
		dirz                        = NA,
		pulselengthtable            = do.call(rbind, lapply(config$Transceivers[names(config$Transceivers) == "Transceiver"], function(x) splitByChar(x$Channels$Channel$.attrs["PulseDuration"]))),
		spare2                      = NA,
		gaintable                   = getTransducerVar(config, "Gain"),
		spare3                      = NA,
		sacorrectiontable           = getTransducerVar(config, "SaCorrection"),
		spare4                      = NA
	)
	# Add the FrequencyPar:
	out$FrequencyPar <- getFrequencyPar(config)
	out
}
getHeaderRaw3 <- function(data, config){
	# Combine the header of the Transceivers element in the (should be first) XML0 datagram, and the version (usually third) XML0 datagram:
	atTransceivers <- which(sapply(data$XML0, function(x) "Transceivers" %in% names(x)))[1]
	atversion <- which(sapply(data$XML0, function(x) "version" %in% names(x)))[1]
	out <- c(
		data$XML0[[atTransceivers]]$Header, 
		data$XML0[[atversion]] 
	)
	
	# Remove the time stamp:
	out <- out[tolower(names(out)) != "time"]
	out
}


#*********************************************
#*********************************************
#' (Internal) Strips the input from NAs, used to remove missing pings in EKRaw2TSD_oneFile().
#'
#' @param x a matrix or array of acoustic data.
#' @param NAind indices of the NAs.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_stripNA
#' 
readEKRaw_stripNA <- function(x, NAind=NULL){
	if(length(NAind)){
		if(is.logical(NAind)){
			NAind <- which(NAind)
		}
		if(length(dim(x))==0){
			x[-NAind]
		}
		else if(length(dim(x))==2){
			x[,-NAind]
		}
		else if(length(dim(x))==3){
			x[,,-NAind]
		}
	}
	else{
		x
	}
}

#*********************************************
#*********************************************
#' Splits Simrad raw files by beams.
#'
#' @param x The path to a Simrad raw file or a directory of Simrad raw files.
#' @param t The time steps to extract the data for (use the default t="all" to keep all time steps)
#' @param beamnr A vector or list of beam indices. If empty (default) split into one file for each beam. If several beams should be in one file, use a list such as list(1, 2:4) for a four beam file, resulting in one file for the first beam and one file for the last three beams.
#' @param write Logical: If TRUE, write the splitted data into separate files (in separate directories as default).
#' @param newdir The path to the directory in which to put the splitted files, defaulted to one directory per beam in the same directory as the original raw files.
#' @param nameadd A string to be appended to the file names of the splitted files, defaulted to "Beams_1", "Beams_2,3,4" if splitting a four beam file into the first beam and the last three beams.
#' @param msg Logical: If TURE print a time bar for reading and writing of each individual raw file.
#' @param msgbar Logical: If TURE print a time bar showing the progression of splitting multiple files.
#' @param ext The file extension of the raw files.
#' @param ... Further arguments passed on to list.files(), such as 'recursive'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_split
#' 
readEKRaw_split <- function(x, t="all", beamnr=NULL, write=TRUE, newdir=NULL, nameadd=NULL, msg=FALSE, msgbar=TRUE, ext="raw", ...){
	# Function used for getting new file names, given 'newdir', 'nameadd' and the beamnumber(s). Returns only one file name!!!:
	newFileName <- function(x, beamnr, nameadd, newdir){
		# Get the new directory and new names of the files:
		if(length(nameadd)==0){
			nameadd <- paste0("Beams_", paste(beamnr, collapse=","))
		}
		if(length(newdir)==0){
			newdir <- file.path(dirname(x[1]), nameadd)
		}
		suppressWarnings(dir.create(newdir))
		newfilename <- basename(x)
		newfilename <- paste0(substr(newfilename, 1, nchar(newfilename)-nchar(ext)-1), "_", nameadd, ".", ext)
		file.path(newdir, newfilename)
	}
	
	# Get the paths to the raw files if given as a directory:
	if(isTRUE(file.info(x)$isdir)){
		x <- list.files(x, full.names=TRUE, paste0("^.*[:.:](", ext, ")$"), ...)
	}
	
	if(msgbar){
		infostring = "Extracting beams from Simrad raw files:"
		cat(infostring,"\n",sep="")
		totalsteps = length(x)
		stepfact = nchar(infostring)/totalsteps
		oldvalue = 0
	}
	for(i in seq_along(x)){
		if(msgbar){
			thisvalue = floor(i*stepfact)
			if(thisvalue > oldvalue){
				cat(rep(".",thisvalue-oldvalue),if(i == totalsteps) "\n", sep="")
				oldvalue = thisvalue
				}
			}
		
		# Read the raw file:
		data <- readEKRaw(x[i], t=t, msg=msg)
		data <- readEKRaw_extractBeams(data, beamnr=beamnr)
		thisnewdir <- rep(newdir, length.out=length(data$beamnr))
		
		#data <- readEKRaw_extractBeams(data, )
		if(write){
			for(j in seq_along(data$beamnr)){
				# The file name is set based on the available beams in the file:
				f <- newFileName(x[i], data$beamnr[[j]], nameadd, thisnewdir[j])
				writeEKRaw(data$data[[j]], con=f, msg=msg)
			}
		}
	}
	# Return the last data:
	#invisible(data)
	# Return the last file name:
	invisible(f)
}


#*********************************************
#*********************************************
#' Read all datagram headers of a Simrad raw file.
#'
#' Provides the facility to modify parts of EK80/WBAT .raw files so that these files can be read by the Large Scale Survey System (LSSS) software.
#'
#' @param raw			The path to directory of raw files or a vector of the paths to the raw files.
#' @param endian		The endianness of the file, defaulted to "little".
#' @param msg			Logical: if TRUE print a time bar during file reading.
#' @param rawsplit.out	Logical: should the raw vector be split by datagrams and returned.
#'
#' @return A data frame of datagram info.
#'
#' @importFrom TSD is.TSD
#' @importFrom tools file_ext
#' @importFrom data.table rbindlist
#'
#' @export
#' @rdname readEKRaw_ScanDgHeaders
#' 
readEKRaw_ScanDgHeaders <- function(raw, endian="little", msg=TRUE, rawsplit.out=FALSE){
	
	#  Read the entire file if not previously read:
	if(is.character(raw) && file.exists(raw)){
		fid <- file(raw, "rb")
		on.exit(close(fid))
		raw <- readBin(fid, "raw", file.info(raw)$size)
	}
	
	nbytesDgLen <- 4
	nBytesDgHeader <- 12
	totalsteps <- length(raw)
	
	# Define the output 'out', the datagram indices 'i', and the position in the raw vector 'at':
	out <- list()
	at <- 0
	i <- 0
	
	while(at < totalsteps){
		# Increment datagram index:
		i <- i + 1
		# Get the datagram length in bytes:
		dgLen <- readBin(raw[at + seq_len(nbytesDgLen)], what="int", n=1, size=nbytesDgLen, endian=endian, signed=TRUE)
		if(dgLen == 0){
			warning("The datagram at position ", at, " is empty.")
		}
	
		# If the length of the datagram is 0, abort the reading:
		if(sum(dgLen)==0 && at < totalsteps){
			break
		}
		
		# Read the datagram header:
		dgHeader <- readEKRaw_GetDgHeader(raw[at + nbytesDgLen + seq_len(nBytesDgHeader)], timeOffset=0, endian=endian)
		at <- at + dgLen + 8 # 4 bytes each from reading the 'dgLen' at the start and end of the datagram, in total 8 bytes
		
		# Add the number of bytes used for the datagram length before and anfter the datagram:
		Nbytes <- dgLen + 2 * nbytesDgLen
		out[[i]] <- c(list(Nbytes=Nbytes, dgLen=dgLen), dgHeader)
	}
	
	if(msg){
		cat("\n", sep="")
	}
	
	# Return the header and data:
	dg <- as.data.frame(data.table::rbindlist(out), stringsAsFactors=FALSE)
	colnames(dg) <- c("Nbytes", "dgLen", "dgName", "dgTime")
	
	# Warning if there is no CON0 datagram:
	if(!dg$dgName[1] %in% c("CON0", "XML0")){
		warning("No CON0 or XML0 datagram at the beginning of the file.")
	}
	
	# Output also the raw vector for each datagram, where the leading and trailing length info (4 bytes on each side) is removed, as well as the 12 bytes of the datagram header:
	dg$starts <- cumsum(dg$Nbytes)
	dg$ends <- dg$starts - nbytesDgLen
	dg$starts <- c(0, dg$starts[-length(dg$starts)]) + nbytesDgLen + nBytesDgHeader + 1
	
	if(rawsplit.out){
		rawsplit <- readEKRaw_SplitRawVector(list(raw=raw, dg=dg))$raw
		list(dg=dg, raw=raw, rawsplit=rawsplit)
	}
	else{
		list(dg=dg, raw=raw)
	}
}
# Function for splitting the raw vector into vectors for each datagram (excluding the header and length info at the start of the vector and the length info at the end):
readEKRaw_SplitRawVector <- function(x, dg=NULL, ind=NULL){
	if(length(ind) == 0){
		ind <- seq_along(x$dg$starts)
	}
	x$dg <- x$dg[ind, ]
	# Run thorugh the rows of the datagram info matrix x$dg, and extract the datagrams as raw vectors:
	vec <- seq_len(nrow(x$dg))
	x$raw <- lapply(vec, function(i) x$raw[seq(x$dg$starts[i], x$dg$ends[i])])
	names(x$raw) <- x$dg$dgName
	x
}


#*********************************************
#*********************************************
#' Reads the transceiver configuration from a Simrad raw file.
#'
#' @param fid  is the path to the raw file.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_ReadTransceiverConfig
#'
readEKRaw_ReadTransceiverConfig<-function(fid, endian="little"){
	
	# Based on code by Lars Nonboe Andersen, Simrad.
	configXcvr = list()
	configXcvr[["channelid"]] =						readChar(con=fid, nchars=128, useBytes=TRUE)
	configXcvr[["beamtype"]] =						readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["frequency"]] =						readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["gain"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["equivalentbeamangle"]] =			readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["beamwidthalongship"]] =			readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["beamwidthathwartship"]] =			readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["anglesensitivityalongship"]] =		readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["anglesensitivityathwartship"]] =	readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["anglesoffsetalongship"]] =			readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["angleoffsetathwartship"]] =		readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["posx"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["posy"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["posz"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["dirx"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["diry"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["dirz"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["pulselengthtable"]] =				readBin(fid, what="double", n=5, size=4, endian=endian, signed=TRUE)
	configXcvr[["spare2"]] =						readChar(con=fid, nchars=8, useBytes=TRUE)
	configXcvr[["gaintable"]] =						readBin(fid, what="double", n=5, size=4, endian=endian, signed=TRUE)
	configXcvr[["spare3"]] =						readChar(con=fid, nchars=8, useBytes=TRUE)
	configXcvr[["sacorrectiontable"]] =				readBin(fid, what="double", n=5, size=4, endian=endian, signed=TRUE)
	configXcvr[["spare4"]] =						readChar(con=fid, nchars=52, useBytes=TRUE)
	
	# Return the transceiver configuration:
	configXcvr
}

#*********************************************
#*********************************************
#' Read all times of a raw file.
#'
#' @param f			A vector of raw file names or a directory containing raw files.
#' @param endian	The endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#' @param msg		Logical: If TRUE print a time bar during reading.
#' @param cores		An integer giving the number of cores to run in parallel.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD mtim2utim papply
#' @export
#'
readEKRaw_ReadTimes <- function(f, endian="little", msg=TRUE, cores=1){
	
	readEKRaw_ReadTimesOneFile <- function(f, endian="little", msg=TRUE){
		# Scan all datagram headers:
		temp <- SimradRaw::readEKRaw_ScanDgHeaders(f, endian=endian, msg=msg)
		# Get the raw format:
		rawFileFormat <- paste0("RAW", SimradRaw::readEKRaw_GetRawFileFormat(temp$dg))
		# Keep only the raw datagrams:
		raw <- temp$dg[temp$dg$dgName==rawFileFormat, ]
		# Get the unix times:
		utim <- TSD::mtim2utim(raw$dgTime)
		# Select only start times of the pings (the times are repeated over all beams)
		utim <- unique(utim)
	
		return(utim)
	}
	
	if(isTRUE(file.info(f)$isdir)){
		f <- list.files(f, pattern="*.raw", full.names=TRUE)
	}
	
	utim <- TSD::papply(f, readEKRaw_ReadTimesOneFile, endian=endian, msg=msg, cores=cores)
	return(utim)
}

#*********************************************
#*********************************************
#' Reads the sample data from a Simrad raw1 file.
#'
#' @param fid  is the path to the raw file.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_ReadSampledata_RAW1
#'
readEKRaw_ReadSampledata_RAW1<-function(fid, endian="little"){
	
	# Based on code by Lars Nonboe Andersen, Simrad.
	sampledata=list()
	sampledata$channel = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$datatype = readBin(con=fid, what="int", n=1, size=1, endian=endian, signed=TRUE)
	sampledata$ncomplexpersample = readBin(con=fid, what="int", n=1, size=1, endian=endian, signed=TRUE)
	temp = readBin(con=fid, what="double", n=13, size=4, endian=endian, signed=TRUE)
		sampledata$gaintx = temp[1]
		sampledata$frequency = temp[2]
		sampledata$transmitpower = temp[3]
		sampledata$pulselength = temp[4]
		sampledata$bandwidth = temp[5]
		sampledata$sampleinterval = temp[6]
		sampledata$soundvelocity = temp[7]
		sampledata$absorptioncoefficient = temp[8]
		sampledata$heave = temp[9]
		sampledata$roll = temp[10]
		sampledata$pitch = temp[11]
		sampledata$temperature = temp[12]
		sampledata$heading = temp[13]
	sampledata$transmitmode = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$pulseform = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	temp = readBin(con=fid, what="double", n=12, size=4, endian=endian, signed=TRUE)
		sampledata$dirx = temp[1]
		sampledata$diry = temp[2]
		sampledata$dirz = temp[3]
		sampledata$gainrx = temp[4]
		sampledata$sacorrection = temp[5]
		sampledata$equivalentbeamangle = temp[6]
		sampledata$beamwidthalongshiprx = temp[7]
		sampledata$beamwidthathwartshiprx = temp[8]
		sampledata$anglesensitivityalongship = temp[9]
		sampledata$anglesensitivityathwartship = temp[10]
		sampledata$angleoffsetalongship = temp[11]
		sampledata$angleoffsetathwartship = temp[12]
	sampledata$spare = readChar(con=fid, nchars=2, useBytes=TRUE)
	sampledata$noisefilter = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$beamwidthmode = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$beammode = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$beamwidthhorizontaltx = readBin(con=fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$beamwidthverticaltx = readBin(con=fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$offset = readBin(con=fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$count = readBin(con=fid, what="int", n=1, size=4, endian=endian, signed=TRUE) # 132 bytes in total
	# Read acoustic data:
	if(integer.base.b(sampledata$datatype, endian=endian)[4]==1){
		
		# From the matlab code of Simrad:
		# sdata = fread(fid,[2*sampledata.ncomplexpersample,sampledata.count],'float32');
		# sdata = reshape(sdata,[2 sampledata.ncomplexpersample sampledata.count]);
			# % trx32multiratefilterdelay = 16; %samples
		# trx32multiratefilterdelay = 1; %samples
			# % txpulsesamples = round(sampledata.pulselength/sampledata.sampleinterval);
			# % sdata = sdata(:,:,txpulsesamples+1:end);
		# sdata = sdata(:,:,trx32multiratefilterdelay:end);
		# sampledata.count = size(sdata,3); % Update sampledata.count by the result of trx32multiratefilterdelay.
		# sampledata.data = squeeze(complex(sdata(1,:,:),sdata(2,:,:)));
		# % Sum over complex samples: sum(sdata,2), then square, the sum the real and imaginary part, and finally take the 10*log10 of the result:
		# sampledata.power = 10*log10(squeeze(sum((sum(sdata,2)).^2)));
	
		sdata = readBin(con=fid, what="double", n=2 * sampledata$ncomplexpersample * sampledata$count, size=4, endian=endian, signed=TRUE)
		dim(sdata)=c(2, sampledata$ncomplexpersample, sampledata$count)
		#trx32multiratefilterdelay = 1
		#sdata = sdata[,,seq(trx32multiratefilterdelay,sampledata$count), drop=FALSE]
		# Update the count of the current channel:
		#sampledata$count = dim(sdata)[3]
		# Save as complex type:
		sampledata$data = complex(real=sdata[1,,], imaginary=sdata[2,,])
		dim(sampledata$data)=c(sampledata$ncomplexpersample, sampledata$count)
		# Sum over the number of complex values per sample, and take the square of the abs, which is the same as the sum of the real part squared and the imaginary part squared:
		sampledata$power = 10*log10(abs(colSums(sampledata$data))^2)
	}
	sampledata
}

#*********************************************
#*********************************************
#' Reads the sample data from a Simrad raw0 file.
#'
#' @param fid  is the path to the raw file.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_ReadSampledata_RAW0
#'
readEKRaw_ReadSampledata_RAW0<-function(fid, endian="little"){
	
	# Based on code by Lars Nonboe Andersen, Simrad.
	sampledata=list()
	sampledata$channel = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	mode_low = readBin(con=fid, what="int", n=1, size=1, endian=endian, signed=TRUE)
	mode_high = readBin(con=fid, what="int", n=1, size=1, endian=endian, signed=TRUE)
	sampledata$mode = 256 * mode_high + mode_low;
	temp = readBin(con=fid, what="double", n=12, size=4, endian=endian, signed=TRUE)
		sampledata$transducerdepth = temp[1]
		sampledata$frequency = temp[2]
		sampledata$transmitpower = temp[3]
		sampledata$pulselength = temp[4]
		sampledata$bandwidth = temp[5]
		sampledata$sampleinterval = temp[6]
		sampledata$soundvelocity = temp[7]
		sampledata$absorptioncoefficient = temp[8]
		sampledata$heave = temp[9]
		sampledata$roll = temp[10]
		sampledata$pitch = temp[11]
		sampledata$temperature = temp[12]
	sampledata$trawlupperdepthvalid = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$trawlopeningvalid = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$trawlupperdepth = readBin(con=fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$trawlopening = readBin(con=fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$offset = readBin(con=fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$count = readBin(con=fid, what="int", n=1, size=4, endian=endian, signed=TRUE) # 72 bytes in total
	# Read acoustic data and angle data:
	if(sampledata$count > 0){
		if(sampledata$mode != 2){
			power = readBin(con=fid, what="int", n=sampledata$count, size=2, endian=endian, signed=TRUE)
			# Power * 10 * log10(2) / 256
			sampledata$power = (power * 0.011758984205624)
		}
		if(sampledata$mode > 1){
			angle = readBin(con=fid, what="int", n=2*sampledata$count, size=1, endian=endian, signed=TRUE)
			angle=matrix(angle,nrow = sampledata$count,ncol=2,byrow=TRUE)
			sampledata$athwartship = angle[,1]
			sampledata$alongship = angle[,2]
		}
	}
	sampledata
}

#*********************************************
#*********************************************
#' Read the header from a Simrad raw file.
#'
#' @param fid  is the path to the raw file.
#' @param dgHeader A list containing the dgTime.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_ReadHeader
#'
readEKRaw_ReadHeader<-function(fid, dgHeader, endian="little"){
	
	# Read configuration header:
	configheader = readEKRaw_ReadConfigHeader(fid, endian=endian)
	configheader$time = dgHeader$dgTime
	
	# Extract individual xcvr configurations:
	configXcvr=list()
	if(configheader$transceivercount>10000 || configheader$transceivercount<0){
		warning(paste0("Data not properly read (transceivercount higher than 10000 or negative (", configheader$transceivercount, ") . Try to change endian)"))
		return(list())
	}
	for(i in seq_len(configheader$transceivercount)){
		thisconfigXcvr = readEKRaw_ReadTransceiverConfig(fid, endian=endian)
		for(j in seq_along(thisconfigXcvr)){
			configXcvr[[names(thisconfigXcvr )[j]]][[i]] = thisconfigXcvr[[j]]
		}
	}
	# Collapse the tables into matrices:
	for(i in seq_along(configXcvr)){
		if(length(configXcvr[[i]][[1]])>1){
			configXcvr[[i]] = matrix(unlist(configXcvr[[i]]),byrow=TRUE,nrow=configheader$transceivercount)
		}
	}
	if(length(configXcvr)>0){
		names(configXcvr)=names(thisconfigXcvr)
	}
	
	# Return the header and tranceiver configurations:
	list(header=configheader, transceiver=configXcvr)
}

#*********************************************
#*********************************************
#' Reads the a datagram header stored in a Simrad raw file.
#'
#' @param fid  is the path to the raw file.
#' @param timeOffset  is the time offset of the datagram.
#' @param endian is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31). NA
#' @param xBase  is the base of Windows FILETIME: xBase=unclass(as.POSIXct('1601-1-1', tz="UTC"))[1].
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD FILETIME2mtim
#'
#' @export
#' @rdname readEKRaw_ReadDgHeader
#'
readEKRaw_ReadDgHeader<-function(fid, timeOffset=0, endian="little", xBase=-11644473600){
	
	# Read datagram type:
	dgType <- readChar(con=fid, nchars=4, useBytes=TRUE)
	# If no data was read, end the function:
	if(length(dgType)==0){
		return(list())
	}
	
	#  Read datagram time (NT Time - number of 100-nanosecond intervals since January 1, 1601):
	lowdatetime <- readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	if(length(lowdatetime)>0 && lowdatetime<0){
		lowdatetime <- lowdatetime + 2^32
	}
	highdatetime <- readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	# Convert to unsingned integer, which is not supported in R:
	# Convert NT time to MATLAB serial time:
	FILETIME <- (highdatetime * 2 ^ 32 + lowdatetime) + timeOffset/86400
	
	dgTime <- TSD::FILETIME2mtim(FILETIME, xBase=xBase)
	
	# Return:
	list(dgType=dgType, dgTime=dgTime)
}

#*********************************************
#*********************************************
#' Reads the configuration header from a Simrad raw0 file.
#'
#' @param fid  is the path to the raw file.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_ReadConfigHeader
#'
readEKRaw_ReadConfigHeader<-function(fid, endian="little"){
	
	# Based on code by Lars Nonboe Andersen, Simrad.
	configheader=list()
	configheader[["surveyname"]] =              readChar(con=fid, nchars=128, useBytes=TRUE)
	configheader[["transectname"]] =            readChar(con=fid, nchars=128, useBytes=TRUE)
	configheader[["soundername"]] =             readChar(con=fid, nchars=128, useBytes=TRUE)
	configheader[["version"]] =                 readChar(con=fid, nchars=30, useBytes=TRUE)
	configheader[["multiplexing"]] =            readBin(fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	configheader[["timebias"]] =                readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["soundvelocityaverage"]] =    readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["soundvelocitytransducer"]] = readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["mruoffsetx"]] =              readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["mruoffsety"]] =              readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["mruoffsetz"]] =              readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["mrualphax"]] =               readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["mrualphay"]] =               readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["mrualphaz"]] =               readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["gpsoffsetx"]] =              readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["gpsoffsety"]] =              readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["gpsoffsetz"]] =              readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configheader[["spare"]] =                   readChar(con=fid, nchars=48, useBytes=TRUE)
	configheader[["transceivercount"]] =        readBin(fid, what="int", n=1, size=4, endian=endian,  signed=TRUE)
	
	# Return the configuration header:
	configheader
}

#*********************************************
#*********************************************
#' Converts power to volume backscattering coefficient from data in the TSD format.
#'
#' @param x  is a list of the data (one element for each time step!!!).
#' @param beams  is a list of the beam configuration of the sonar or echosounder.
#' @param cali  optinal calibration information containing gain and Sa_correction values, possibly given pulse length values and frequencies.
#' @param list.out  is TRUE to return the data as a list of acoustic values and calibration values.
#' @param tiltcorr  is TRUE to apply the tilt correction used for fishery sonars.
#' @param toTS  is TRUE to apply the TS calibration instead for the Sv calibration.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD dim_all listOfEqual2array
#'
#' @export
#'
readEKRaw_power2sv.TSD <- function(x, beams=list(), cali=NULL, list.out=FALSE, tiltcorr=0, toTS=FALSE){
	
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	# Update: 2015-04-27 - Fixing bugs.
	# Update: 2015-12-03 - Using full dimension for all elements to ensure that nothing wrong is happening.
	# Last: 2015-12-21 - Added the calibration data as a list returned from a calibration xml file.
	
	# Function used for expanding the dimenstions of the beams variables:
	expandBeamsVars <- function(beams){
		expandVar <- function(x){
			d <- dim(x)
			l <- length(x)
			if(length(d)==0){
				x <- matrix(x, nrow=beams$numb, ncol=beams$numt, byrow=l==beams$numt)
			}
			x
		}
		tobeexpanded <- !names(beams) %in% c("numb", "numt")
		beams[tobeexpanded] <- lapply(beams[tobeexpanded], expandVar)
		beams
	}
	# Convenient function for conversion from log to linear values:
	exp10 <- function(x){
		10^(x/10)
	}
	
	
	# Detect the type of raw file:
	if(length(beams$gai1)>0 || (is.list(x) && length(x$data$pings$gaintx)>0)){
		raw=1
	}
	else{
		raw=0
	}
	
	# If an object as read directly using readEKRaw() is used, extract the beams information here:
	if(is.list(x) && length(x$data)>0){
		# Beams variables with different location in raw0 and raw1:
		# Raw1:
		if(raw==1){
			beams <- list(
				# 1. Sa-correction: sacr:
				sacr <- x$data$pings$sacorrection, 
				# 2. Gain: gai1, gai2:
				gai1 <- x$data$pings$gaintx, 
				gai2 <- x$data$pings$gainrx, 
				# 3. Equivalent beam angle: eqba:
				eqba <- x$data$pings$equivalentbeamangle, 
				# 4. Elevation angle (tilt): dirx:
				dirx <- x$data$pings$dirx
			)
		}
		# Raw0:
		else{
			beams <- list(
				# 1. Sa-correction, sacr:
				sacr <- x$data$config$sacorrectiontable[,1], 
				# 2. Gain: gain:
				gain <- x$data$config$gain, 
				# 3. Equivalent beam angle: eqba:
				eqba <- x$data$config$equivalentbeamangle, 
				# 4. Elevation angle (tilt): dirx:
				dirx <- x$data$config$dirx
			)
		}	
		
		# Beams variables with the same location in raw0 and raw1:
		# 5. Transmit power: tpow:
		beams$tpow <- x$data$pings$transmitpower
		# 6. Pulse length: plsl:
		beams$plsl <- x$data$pings$pulselength[1,]
		# 7. average speed of sound: asps:
		beams$asps <- x$data$pings$soundvelocity[1,]
		# 8. frequency: freq:
		beams$freq <- x$data$pings$frequency
	}
	else{
		x <- list(data=list(pings=list(power=x)))
	}
	
	# Expand all beams variables to a matrix with dimension c(numb , numt):
	beams$numt <- ncol(beams$freq)
	beams$numb <- nrow(beams$freq)
	beams <- expandBeamsVars(beams)
	
	# Apply input calibration:
	if(length(cali)>0 && is.list(cali)){
		# Old raw1-calibration data containing sa-correction:
		if(length(cali$sacr)){
			# Disregard frequency but pick out the closest pulse lengths:
			closestplsl <- apply(outer(cali$gain[,"pl"]*1e-6, beams$plsl), 2, which.min)
			beams$gain <- cali$gain[closestplsl,1]
			closestplsl <- apply(outer(cali$sacr[,"pl"]*1e-6, beams$plsl), 2, which.min)
			beams$sacr <- cali$sacr[closestplsl,1]
		}
		# New raw1-calibration data containing effective pulse length (plse):
		else if(length(cali$plse)){
			beams$gain <- cali$gain
			beams$plse <- cali$plse
		}
		# Only gain given, no effective pulselength or sa-correction:
		else if(length(cali$gain)){
			beams$gain <- cali$gain
		}
		# Otherwise do not apply any calibration:
		else{
			cali <- NULL
		}
	}
	# Apply internal gain:
	if(length(cali)==0){
		if(raw==1){
			beams$gain <- beams$gai1 + beams$gai2
		}
		else{
			beams$gain <- 2 * beams$gain # Multiply by 2 to correspond to the summation of two gains when raw==1
		}
	}
	
	# Special treatment of gain in fishery sonar (Macaulay et al., 2016, Practical calibration of ship-mounted omni-directional fisheries sonars):
	if(raw==1 && length(cali)){
		gainRaw1 <- function(x){
			3.69E3 / x$freq[1] - 53.9E6 / (x$freq[1]^2) - 6.49E-3 / x$plsl[1] - 43.2
		}
		beams$gain <- beams$gain + gainRaw1(beams) - gainRaw1(cali)
	}
	
	
	# Get the effective pulse duration:
	if(length(beams$plse)==0){
		beams$plse <- beams$plsl * 10^(2*beams$sacr/10)
	}
	
	# Calculate wavelength:
	beams$lmbd <-  beams$asps / beams$freq
	
	# Elevation angle correction. Adopted from Gavin's code on 2015-05-19. Note that the tilt is negative below horizontal, thus the minus sign. However, this has no effect since cos(x) = cos(-x):
	if(isTRUE(tiltcorr[[1]])){
		cat("Tilt-correction applied for fishery sonar\n")
		beams$tiltcorr <- 40 * log10(cos(-beams$dirx * pi / 180))
	}
	else{
		beams$tiltcorr <- tiltcorr
	}
	
	# Set correct dimensions:
	beams[c("tpow", "lmbd", "asps", "plsl", "gain", "tiltcorr", "sacr", "eqba")] <- lapply(beams[c("tpow", "lmbd", "asps", "plsl", "gain", "tiltcorr", "sacr", "eqba")], array, dim=c(beams$numb, beams$numt))
	if(toTS){
		beams$calf <- 10 * log10( (beams$tpow * beams$lmbd^2) / (16*pi^2) )  +  beams$gain + beams$tiltcorr
	}
	else{
		beams$calf <- 10 * log10( (beams$tpow * beams$lmbd^2 * beams$asps * beams$plse) / (32*pi^2) )  +  beams$gain + beams$tiltcorr + beams$eqba
	}
	
	# Drop to an array if all time steps have identical dimensions:
	# No longer used, since the new readEKRaw() outputs an array [#samples, #beams, #pings] padded with NAs.
	#x$data$pings$power <- TSD::listOfEqual2array(x$data$pings$power)
	if(is.list(x$data$pings$power)){
		for(i in seq_len(beams$numt)){
			thisdim <- TSD::dim_all(x$data$pings$power[[i]])
			# The power field in the raw files is already in dB, so we need to linearize it here:
			x$data$pings$power[[i]] <- exp10(x$data$pings$power[[i]] - rep(beams$calf[,i], each=thisdim[1]))
		}
	}
	else{
		x$data$pings$power <- exp10(x$data$pings$power - rep(beams$calf, each=dim(x$data$pings$power)[1]))
	}
	
	# Return:
	if(list.out){
		list(vbsc=x$data$pings$power, Cgai=beams$gain, Csac=beams$sacr, Ctcr=beams$tiltcorr, Ccal=beams$calf)
	}
	else{
		x$data$pings$power
	}
}

#*********************************************
#*********************************************
#' Reads the sample data from a Simrad raw1 file.
#'
#' @param dgName	The type of schema, such as a datagram name.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#'
readEKRaw_GetSchemaOld <- function(dgName=c("RAW0", "RAW1", "ConfigHeader", "TransceiverConfig"), x=list(), var="all"){
	
	# Read power from RAW0 if mode != 2:
	nPowerRAW0 <- function(x){
		x$count * as.numeric(x$mode != 2)
	}
	# Read angle from RAW0 if mode 2, 3, ..., so if mode==2 only read electrical angles:
	nAngleRAW0 <- function(x){
		2 * x$count * as.numeric(x$mode > 1)
	}
	# Read acoustic data from RAW1 if the fourth bit of datatype is 1:
	nIfBitpos4 <- function(x){
		bitpos <- which(as.integer(intToBits(x$datatype))==1L)
		if(identical(bitpos, 4L)){
			2 * x$ncomplexpersample * x$count
		}
		else{
			0
		}
	}
	# Read coefficients of filter:
	nFIL1 <- function(x){
		2 * x$noofcoefficients
	}
	
	# Define the type and length of the variables, except the acoustic data, which has length depending on these variables:
	schema_RAW0 <- list(
		list(var="channel",						what="int",    n=1, size=2), 
		
		list(var="mode_low",					what="int",    n=1, size=1), 
		list(var="mode_high",					what="int",    n=1, size=1), 
		                                	
		list(var="transducerdepth",				what="double", n=1, size=4), 
		list(var="frequency",					what="double", n=1, size=4), 
		list(var="transmitpower",				what="double", n=1, size=4), 
		list(var="pulselength",					what="double", n=1, size=4), 
		list(var="bandwidth",					what="double", n=1, size=4), 
		list(var="sampleinterval",				what="double", n=1, size=4), 
		list(var="soundvelocity",				what="double", n=1, size=4), 
		list(var="absorptioncoefficient",		what="double", n=1, size=4), 
		list(var="heave", 						what="double", n=1, size=4), 
		list(var="roll", 						what="double", n=1, size=4), 
		list(var="pitch", 						what="double", n=1, size=4), 
		list(var="temperature", 				what="double", n=1, size=4), 
		                                	
		list(var="trawlupperdepthvalid", 		what="int",    n=1, size=2), 
		list(var="trawlopeningvalid", 			what="int",    n=1, size=2), 
		                                	
		list(var="trawlupperdepth",				what="double", n=1, size=4), 
		list(var="trawlopening",				what="double", n=1, size=4), 
		
		list(var="offset",						what="int",    n=1, size=4), 
		list(var="count",						what="int",    n=1, size=4), 
		
		list(var="power0",						what="int",    n=if(length(x)) nPowerRAW0(x) else nPowerRAW0, size=2), 
		list(var="angle",						what="int",    n=if(length(x)) nAngleRAW0(x) else nAngleRAW0, size=1)
	)
	schema_RAW1 <- list(
		list(var="channel",						what="int",    n=1, size=2), 
		list(var="datatype",					what="int",    n=1, size=1), 
		list(var="ncomplexpersample",			what="int",    n=1, size=1), 
		                                	
		list(var="gaintx",						what="double", n=1, size=4), 
		list(var="frequency",					what="double", n=1, size=4), 
		list(var="transmitpower",				what="double", n=1, size=4), 
		list(var="pulselength",					what="double", n=1, size=4), 
		list(var="bandwidth",					what="double", n=1, size=4), 
		list(var="sampleinterval",				what="double", n=1, size=4), 
		list(var="soundvelocity",				what="double", n=1, size=4), 
		list(var="absorptioncoefficient",		what="double", n=1, size=4), 
		list(var="heave", 						what="double", n=1, size=4), 
		list(var="roll", 						what="double", n=1, size=4), 
		list(var="pitch", 						what="double", n=1, size=4), 
		list(var="temperature", 				what="double", n=1, size=4), 
		list(var="heading", 					what="double", n=1, size=4), 
		                                	
		list(var="transmitmode", 				what="int",    n=1, size=2), 
		list(var="pulseform", 					what="int",    n=1, size=2), 
		                                	
		list(var="dirx",						what="double", n=1, size=4), 
		list(var="diry",						what="double", n=1, size=4), 
		list(var="dirz",						what="double", n=1, size=4), 
		list(var="gainrx",						what="double", n=1, size=4), 
		list(var="sacorrection",				what="double", n=1, size=4), 
		list(var="equivalentbeamangle",			what="double", n=1, size=4), 
		list(var="beamwidthalongshiprx",		what="double", n=1, size=4), 
		list(var="beamwidthathwartshiprx",		what="double", n=1, size=4), 
		list(var="anglesensitivityalongship", 	what="double", n=1, size=4), 
		list(var="anglesensitivityathwartship",	what="double", n=1, size=4), 
		list(var="angleoffsetalongship", 		what="double", n=1, size=4), 
		list(var="angleoffsetathwartship", 		what="double", n=1, size=4), 
		
		list(var="spare",						what="char",   n=2, size=1), 
		
		list(var="noisefilter",					what="int",    n=1, size=2), 
		list(var="beamwidthmode",				what="int",    n=1, size=2), 
		list(var="beammode",					what="int",    n=1, size=2), 
		
		list(var="beamwidthhorizontaltx",		what="double", n=1, size=4), 
		list(var="beamwidthverticaltx",			what="double", n=1, size=4), 
		
		list(var="offset",						what="int",    n=1, size=4), 
		list(var="count",						what="int",    n=1, size=4), 
		
		list(var="data",						what="double", n=if(length(x)) nIfBitpos4(x) else nIfBitpos4, size=4)
	)
	schema_ConfigHeader <- list(
		list(var="surveyname",					what="char",   n=128, size=1), 
		list(var="transectname",				what="char",   n=128, size=1), 
		list(var="soundername",					what="char",   n=128, size=1), 
		list(var="version",						what="char",   n=30,  size=1), 
		
		list(var="multiplexing",				what="int",    n=1,   size=2), 
		
		list(var="timebias",					what="int",    n=1,   size=4), 
		                                	
		list(var="soundvelocityaverage",		what="double", n=1,   size=4), 
		list(var="soundvelocitytransducer",		what="double", n=1,   size=4), 
		list(var="mruoffsetx",					what="double", n=1,   size=4), 
		list(var="mruoffsety",					what="double", n=1,   size=4), 
		list(var="mruoffsetz",					what="double", n=1,   size=4), 
		list(var="mrualphax",					what="double", n=1,   size=4), 
		list(var="mrualphay",					what="double", n=1,   size=4), 
		list(var="mrualphaz",					what="double", n=1,   size=4), 
		list(var="gpsoffsetx", 					what="double", n=1,   size=4), 
		list(var="gpsoffsety", 					what="double", n=1,   size=4), 
		list(var="gpsoffsetz", 					what="double", n=1,   size=4), 
		                                	
		list(var="spare",						what="char",   n=48,  size=1), 
		
		list(var="transceivercount", 			what="int",    n=1, size=4)
	)
	schema_TransceiverConfig <- list(
		list(var="channelid",					what="char",   n=128, size=1), 
		
		list(var="beamtype",					what="int",    n=1,   size=4), 
		
		list(var="frequency",					what="double", n=1,   size=4), 
		list(var="gain",						what="double", n=1,   size=4), 
		list(var="equivalentbeamangle",			what="double", n=1,   size=4), 
		list(var="beamwidthalongship",			what="double", n=1,   size=4), 
		list(var="beamwidthathwartship",		what="double", n=1,   size=4), 
		list(var="anglesensitivityalongship",	what="double", n=1,   size=4), 
		list(var="anglesensitivityathwartship",	what="double", n=1,   size=4), 
		list(var="anglesoffsetalongship",		what="double", n=1,   size=4), 
		list(var="angleoffsetathwartship",		what="double", n=1,   size=4), 
		list(var="posx",						what="double", n=1,   size=4), 
		list(var="posy",						what="double", n=1,   size=4), 
		list(var="posz",						what="double", n=1,   size=4), 
		list(var="dirx",						what="double", n=1,   size=4), 
		list(var="diry",						what="double", n=1,   size=4), 
		list(var="dirz",						what="double", n=1,   size=4), 
		list(var="pulselengthtable",			what="double", n=5,   size=4), 
		                                	
		list(var="spare2",						what="char",   n=8,   size=1), 
		
		list(var="gaintable",					what="double", n=5,   size=4), 
	
		list(var="spare3",						what="char",   n=8,   size=1), 
		
		list(var="sacorrectiontable",			what="double", n=5,   size=4), 
		
		list(var="spare4",						what="char",   n=52,  size=1)
	)
	schema_FIL1 <- list(
		list(var="stage",						what="int",    n=1,   size=2), 
		
		list(var="spare",						what="char",   n=2,   size=1), 
		list(var="channelID",					what="char",   n=128, size=1), 
		
		list(var="noofcoefficients",			what="int",    n=1,   size=2), 
		list(var="decimationfactor",			what="int",    n=1,   size=2), 
		
		list(var="coefficients",				what="double", n=nFIL1,	size=4)
	)
	schema_MRU0 <- list(
		list(var="heave",						what="double", n=1,	  size=4), 
		list(var="roll",						what="double", n=1,	  size=4), 
		list(var="pitch",						what="double", n=1,	  size=4), 
		list(var="heading",						what="double", n=1,	  size=4)
	)
	schema_DEP0 <- list(
		list(var="depth",						what="double", n=1,	  size=4), 
		list(var="parameter1",					what="double", n=1,	  size=4), 
		list(var="parameter2",					what="double", n=1,	  size=4)
	)
	
	# Get the datagram schema by type:
	schema <- get(paste("schema", dgName[1], sep="_"))
	
	# Rbind into a data frame:
	#schema <- data.table::rbindlist(schema)
	
	# Transpose the list:
	names_schema <- names(schema[[1]])
	schema <- lapply(seq_along(schema[[1]]), function(i) sapply(schema, "[[", i))
	names(schema) <- names_schema
	
	# Get the positions of the data in the input raw vector:
	
	# The n0 is the n inserted0 at the functions, which are assumed to cone last,and should thus not change the position in the raw vector:
	funs <- sapply(schema$n, is.function)
	n0 <- schema$n
	n0[funs] <- 0
	n0 <- unlist(n0)
	
	schema$len <- schema$size * n0
	schema$end <- cumsum(schema$len)
	schema$pos <- c(0, schema$end[-length(schema$end)])
	schema$start <- schema$pos + 1
	# Insert NAs in ends for the functions:
	schema$end[funs | n0==0] <- NA
	
	# Create a funciton that accepts ..., since the parameters of readBin are input to the funciton in readVar():
	schema$fun <- c("readBin", "rawToCharDotDotDot")[as.numeric(schema$what == "char") + 1]
	
	### schema$fun <- c(readBin, function(x, ...) rawToChar(x))[as.numeric(schema$what == "char") + 1]
	
	# Convert to a list:
	schema <- as.list(schema)
	# Set the dgName as attribute:
	attr(schema, "dgName") <- dgName[1]
	
	if(length(var) && is.character(var) && !identical(tolower(var), "all")){
		schema <- lapply(schema, "[", schema$var %in% var)
	}
	
	schema
}

readEKRaw_GetSchema <- function(dgName=c("RAW0", "RAW1", "ConfigHeader", "TransceiverConfig"), x=list(), var="all", group=FALSE){
	
	# Read power from RAW0 if mode != 2:
	nPowerRAW0 <- function(x){
		x$count * as.numeric(x$mode != 2)
	}
	# Read angle from RAW0 if mode 2, 3, ..., so if mode==2 only read electrical angles:
	nAngleRAW0 <- function(x){
		2 * x$count * as.numeric(x$mode > 1)
	}
	# Read acoustic data from RAW1 if the fourth bit of datatype is 1:
	nIfBitpos4 <- function(x){
		bitpos <- which(as.integer(intToBits(x$datatype))==1L)
		if(identical(bitpos, 4L)){
			2 * x$ncomplexpersample * x$count
		}
		else{
			0
		}
	}
	# Read coefficients of filter:
	nFIL1 <- function(x){
		2 * x$noofcoefficients
	}
	
	# Define the type and length of the variables, except the acoustic data, which has length depending on these variables:
	schema_RAW0 <- list(
		list(var="channel",						what="int",    n=1, size=2), 
		
		list(var="mode_low",					what="int",    n=1, size=1), 
		list(var="mode_high",					what="int",    n=1, size=1), 
		                                	
		list(var="transducerdepth",				what="double", n=1, size=4), 
		list(var="frequency",					what="double", n=1, size=4), 
		list(var="transmitpower",				what="double", n=1, size=4), 
		list(var="pulselength",					what="double", n=1, size=4), 
		list(var="bandwidth",					what="double", n=1, size=4), 
		list(var="sampleinterval",				what="double", n=1, size=4), 
		list(var="soundvelocity",				what="double", n=1, size=4), 
		list(var="absorptioncoefficient",		what="double", n=1, size=4), 
		list(var="heave", 						what="double", n=1, size=4), 
		list(var="roll", 						what="double", n=1, size=4), 
		list(var="pitch", 						what="double", n=1, size=4), 
		list(var="temperature", 				what="double", n=1, size=4), 
		                                	
		list(var="trawlupperdepthvalid", 		what="int",    n=1, size=2), 
		list(var="trawlopeningvalid", 			what="int",    n=1, size=2), 
		                                	
		list(var="trawlupperdepth",				what="double", n=1, size=4), 
		list(var="trawlopening",				what="double", n=1, size=4), 
		
		list(var="offset",						what="int",    n=1, size=4), 
		list(var="count",						what="int",    n=1, size=4), 
		
		list(var="power0",						what="int",    n=if(length(x)) nPowerRAW0(x) else nPowerRAW0, size=2), 
		list(var="angle",						what="int",    n=if(length(x)) nAngleRAW0(x) else nAngleRAW0, size=1)
	)
	schema_RAW1 <- list(
		list(var="channel",						what="int",    n=1, size=2), 
		list(var="datatype",					what="int",    n=1, size=1), 
		list(var="ncomplexpersample",			what="int",    n=1, size=1), 
		                                	
		list(var="gaintx",						what="double", n=1, size=4), 
		list(var="frequency",					what="double", n=1, size=4), 
		list(var="transmitpower",				what="double", n=1, size=4), 
		list(var="pulselength",					what="double", n=1, size=4), 
		list(var="bandwidth",					what="double", n=1, size=4), 
		list(var="sampleinterval",				what="double", n=1, size=4), 
		list(var="soundvelocity",				what="double", n=1, size=4), 
		list(var="absorptioncoefficient",		what="double", n=1, size=4), 
		list(var="heave", 						what="double", n=1, size=4), 
		list(var="roll", 						what="double", n=1, size=4), 
		list(var="pitch", 						what="double", n=1, size=4), 
		list(var="temperature", 				what="double", n=1, size=4), 
		list(var="heading", 					what="double", n=1, size=4), 
		                                	
		list(var="transmitmode", 				what="int",    n=1, size=2), 
		list(var="pulseform", 					what="int",    n=1, size=2), 
		                                	
		list(var="dirx",						what="double", n=1, size=4), 
		list(var="diry",						what="double", n=1, size=4), 
		list(var="dirz",						what="double", n=1, size=4), 
		list(var="gainrx",						what="double", n=1, size=4), 
		list(var="sacorrection",				what="double", n=1, size=4), 
		list(var="equivalentbeamangle",			what="double", n=1, size=4), 
		list(var="beamwidthalongshiprx",		what="double", n=1, size=4), 
		list(var="beamwidthathwartshiprx",		what="double", n=1, size=4), 
		list(var="anglesensitivityalongship", 	what="double", n=1, size=4), 
		list(var="anglesensitivityathwartship",	what="double", n=1, size=4), 
		list(var="angleoffsetalongship", 		what="double", n=1, size=4), 
		list(var="angleoffsetathwartship", 		what="double", n=1, size=4), 
		
		list(var="spare",						what="char",   n=2, size=1), 
		
		list(var="noisefilter",					what="int",    n=1, size=2), 
		list(var="beamwidthmode",				what="int",    n=1, size=2), 
		list(var="beammode",					what="int",    n=1, size=2), 
		
		list(var="beamwidthhorizontaltx",		what="double", n=1, size=4), 
		list(var="beamwidthverticaltx",			what="double", n=1, size=4), 
		
		list(var="offset",						what="int",    n=1, size=4), 
		list(var="count",						what="int",    n=1, size=4), 
		
		list(var="data",						what="double", n=if(length(x)) nIfBitpos4(x) else nIfBitpos4, size=4)
	)
	schema_ConfigHeader <- list(
		list(var="surveyname",					what="char",   n=128, size=1), 
		list(var="transectname",				what="char",   n=128, size=1), 
		list(var="soundername",					what="char",   n=128, size=1), 
		list(var="version",						what="char",   n=30,  size=1), 
		
		list(var="multiplexing",				what="int",    n=1,   size=2), 
		
		list(var="timebias",					what="int",    n=1,   size=4), 
		                                	
		list(var="soundvelocityaverage",		what="double", n=1,   size=4), 
		list(var="soundvelocitytransducer",		what="double", n=1,   size=4), 
		list(var="mruoffsetx",					what="double", n=1,   size=4), 
		list(var="mruoffsety",					what="double", n=1,   size=4), 
		list(var="mruoffsetz",					what="double", n=1,   size=4), 
		list(var="mrualphax",					what="double", n=1,   size=4), 
		list(var="mrualphay",					what="double", n=1,   size=4), 
		list(var="mrualphaz",					what="double", n=1,   size=4), 
		list(var="gpsoffsetx", 					what="double", n=1,   size=4), 
		list(var="gpsoffsety", 					what="double", n=1,   size=4), 
		list(var="gpsoffsetz", 					what="double", n=1,   size=4), 
		                                	
		list(var="spare",						what="char",   n=48,  size=1), 
		
		list(var="transceivercount", 			what="int",    n=1, size=4)
	)
	schema_TransceiverConfig <- list(
		list(var="channelid",					what="char",   n=128, size=1), 
		
		list(var="beamtype",					what="int",    n=1,   size=4), 
		
		list(var="frequency",					what="double", n=1,   size=4), 
		list(var="gain",						what="double", n=1,   size=4), 
		list(var="equivalentbeamangle",			what="double", n=1,   size=4), 
		list(var="beamwidthalongship",			what="double", n=1,   size=4), 
		list(var="beamwidthathwartship",		what="double", n=1,   size=4), 
		list(var="anglesensitivityalongship",	what="double", n=1,   size=4), 
		list(var="anglesensitivityathwartship",	what="double", n=1,   size=4), 
		list(var="anglesoffsetalongship",		what="double", n=1,   size=4), 
		list(var="angleoffsetathwartship",		what="double", n=1,   size=4), 
		list(var="posx",						what="double", n=1,   size=4), 
		list(var="posy",						what="double", n=1,   size=4), 
		list(var="posz",						what="double", n=1,   size=4), 
		list(var="dirx",						what="double", n=1,   size=4), 
		list(var="diry",						what="double", n=1,   size=4), 
		list(var="dirz",						what="double", n=1,   size=4), 
		list(var="pulselengthtable",			what="double", n=5,   size=4), 
		                                	
		list(var="spare2",						what="char",   n=8,   size=1), 
		
		list(var="gaintable",					what="double", n=5,   size=4), 
	
		list(var="spare3",						what="char",   n=8,   size=1), 
		
		list(var="sacorrectiontable",			what="double", n=5,   size=4), 
		
		list(var="spare4",						what="char",   n=52,  size=1)
	)
	schema_FIL1 <- list(
		list(var="stage",						what="int",    n=1,   size=2), 
		
		list(var="spare",						what="char",   n=2,   size=1), 
		list(var="channelID",					what="char",   n=128, size=1), 
		
		list(var="noofcoefficients",			what="int",    n=1,   size=2), 
		list(var="decimationfactor",			what="int",    n=1,   size=2), 
		
		list(var="coefficients",				what="double", n=nFIL1,	size=4)
	)
	schema_MRU0 <- list(
		list(var="heave",						what="double", n=1,	  size=4), 
		list(var="roll",						what="double", n=1,	  size=4), 
		list(var="pitch",						what="double", n=1,	  size=4), 
		list(var="heading",						what="double", n=1,	  size=4)
	)
	schema_DEP0 <- list(
		list(var="depth",						what="double", n=1,	  size=4), 
		list(var="parameter1",					what="double", n=1,	  size=4), 
		list(var="parameter2",					what="double", n=1,	  size=4)
	)
	
	# Get the datagram schema by type:
	schema <- get(paste("schema", dgName[1], sep="_"))
	
	# Rbind into a data frame:
	#schema <- data.table::rbindlist(schema)
	
	# Transpose the list:
	names_schema <- names(schema[[1]])
	schema <- lapply(seq_along(schema[[1]]), function(i) sapply(schema, "[[", i))
	names(schema) <- names_schema
	
	# Get the positions of the data in the input raw vector:
	
	# The n0 is the n inserted0 at the functions, which are assumed to cone last,and should thus not change the position in the raw vector:
	funs <- sapply(schema$n, is.function)
	n0 <- schema$n
	n0[funs] <- 0
	n0 <- unlist(n0)
	
	schema$len <- schema$size * n0
	schema$end <- cumsum(schema$len)
	schema$pos <- c(0, schema$end[-length(schema$end)])
	schema$start <- schema$pos + 1
	schema$ind <- mapply(seq_acceptNA, schema$start, schema$end, SIMPLIFY=FALSE)
	# Insert NAs in ends for the functions:
	dynamic <- funs | n0==0
	static <- !dynamic
	schema$end[dynamic] <- NA
	
	# Create a funciton that accepts ..., since the parameters of readBin are input to the funciton in readVar():
	schema$fun <- c("readBin", "rawToCharDotDotDot")[as.numeric(schema$what == "char") + 1]
	
	### schema$fun <- c(readBin, function(x, ...) rawToChar(x))[as.numeric(schema$what == "char") + 1]
	
	# Convert to a list:
	schema <- as.list(schema)
	
	if(length(var) && is.character(var) && !identical(tolower(var), "all")){
		schema <- lapply(schema, "[", schema$var %in% var)
	}
	
	## Group the variables and return also the positions:
	#if(group){
	#	# Get unique combiations of what, and size:
	#	what_size <- paste(schema$what[static], schema$size[static])
	#	staticgroup_varind <- match(what_size, unique(what_size))
	#	
	#	staticgroup <- lapply(schema, subset, static)
	#	staticgroup <- lapply(staticgroup, split, staticgroup_varind)
	#	
	#	#staticgroup$ind <- lapply(seq_along(staticgroup$start), function(i) unlist(mapply(seq_acceptNA, staticgroup$start[[i]], staticgroup$end[[i]], SIMPLIFY=FALSE)))
	#	
	#	schema <- staticgroup
	#}
	
	if(group){
		static <- which(!sapply(schema$n, is.function))
		schema$staticgroup <- readEKRaw_GroupSchemaStatic(schema, static=static)
	}
	
	
	# Set the dgName as attribute:
	attr(schema, "dgName") <- dgName[1]
	
	schema
}


# Group the variables and return also the positions:
readEKRaw_GroupSchemaStatic <- function(schema, static){
	# Get unique combiations of what, and size:
	what_size <- paste(schema$what[static], schema$size[static])
	staticgroup_varind <- match(what_size, unique(what_size))
	
	staticgroup <- lapply(schema, "[", static)
	staticgroup <- lapply(staticgroup, split, staticgroup_varind)
	
	attr(staticgroup, "dgName") <- attr(schema, "dgName") 
	
	return(staticgroup)
}

seq_acceptNA <- function(start, end){
	if(is.na(start) || is.na(end)){
		return(NULL)
	}
	else{
		out <- seq(start, end)
	}

	return(out)
}
# A small function that supports ... in rawToChar, and accepting embedded null:
rawToCharDotDotDot <- function(x, ...){
	paste(rawToChar(x, multiple=TRUE), collapse="")
}

# A function that converts a vector of raw data as given by the schema:
convertRawOne <- function(i, x, schema, endian="little", offset=0){
	thisind <- offset + schema$pos[i] + seq_len(schema$len[i])
	out <- do.call(schema$fun[[i]], list(x[thisind], what=schema$what[i], n=schema$n[[i]], size=schema$size[i], endian=endian, signed=TRUE))
	out
}

convertRaw_old <- function(x, schema=NULL, dgName=c("RAW0", "RAW1", "ConfigHeader", "TransceiverConfig"), endian="little", offset=0){
	# Get the schema if missing:
	if(length(schema)==0){
		schema <- readEKRaw_GetSchema(dgName[1])
	}
	# Use apply, even though this is not suited for dataframes:
	out <- lapply(seq_along(schema[[1]]), convertRawOne, x=x, schema=schema, endian=endian, offset=offset)
	names(out) <- schema$var
	out
}


convertRawOneVar <- function(i, x, schema, endian="little", offset=0){
	# Return immediately if no data are present for the current variable:
	if(length(x) == 0){
		return()
	}
	
	# The length of the raw vector 'x' can be divided with the size of the data points to get the number of elements:
	n <- length(x) / schema$size[i]
	out <- do.call(schema$fun[[i]], list(x, what=schema$what[i], n=n, size=schema$size[i], endian=endian, signed=TRUE))
	out
}



convertRawOneStaticSingle <- function(i, x, schema, endian="little", offset=0){
	# Get the section of the raw vector to read, given by the schema by the :
	thisind <- offset + schema$pos[i] + seq_len(schema$len[i])
	out <- do.call(schema$fun[[i]], list(x[thisind], what=schema$what[i], n=schema$n[[i]], size=schema$size[i], endian=endian, signed=TRUE))
	return(out)
}



convertRawOneStaticGroup <- function(i, x, schemaGrouped, endian="little", offset=0){
	# Get the section of the raw vector to read, given by the 'ind' element of the grouped schema:
	thisind <- offset + unlist(schemaGrouped$ind[[i]], use.names=FALSE)
	# Read the raw vector:
	thisn <- sum(unlist(schemaGrouped$n[[i]], use.names=FALSE))
	out <- do.call(schemaGrouped$fun[[i]][1], list(x[thisind], what=schemaGrouped$what[[i]][1], n=thisn, size=schemaGrouped$size[[i]][1], endian=endian, signed=TRUE))
	
	# Split the output into a list given the number of data of each variable:
	splitInd <- rep(seq_along(schemaGrouped$n[[i]]), unlist(schemaGrouped$n[[i]]))
	out <- split(out, splitInd)
	# Add names:
	names(out) <- schemaGrouped$var[[i]]
	
	return(out)
}

convertRawOneDependent <- function(i, x, schema, data, endian="little", offset=0){
	# Convert only if n > 0:
	thisn <- schema$n[[i]](data)
	# Get the length of the current data variable (which is set to 0 for convenience in readEKRaw_GetSchema, but can be conputed as the product of n and size):
	thislen <- schema$size[i] * thisn
	thisind <- offset + schema$pos[i] + seq_len(thislen)
	if(thisn > 0){
		out <- do.call(schema$fun[[i]], list(x[thisind], what=schema$what[i], n=thisn, size=schema$size[i], endian=endian, signed=TRUE))
	}
	else{
		out <- NULL
	}
	out
}

convertRaw <- function(x, schema=NULL, dgName=c("RAW0", "RAW1", "ConfigHeader", "TransceiverConfig"), afterStatic=c("readEKRaw_getMode"), afterDependent=c("readEKRaw_complex2power", "readEKRaw_getAngles", "readEKRaw_getPower"), endian="little", offset=0, ...){
	
	# Get the schema if missing:
	if(length(schema)==0){
		schema <- readEKRaw_GetSchema(dgName[1])
	}
	
	# Get the static and dependent schema (dependent meaning with length dependent on the variables in the static schema):
	dependent <- sapply(schema$n, is.function)
	static <- which(!dependent)
	dependent <- which(dependent)
	
	# Group the schema by unique combinations of size and type, and convert for each group, hopefully saving time:
	#ppp <- proc.time()[2:3]
	#schemaGrouped <- readEKRaw_GroupSchemaStatic(schema=schema, static=static)
	#out <- lapply(seq_along(schema$staticgroup$n), convertRawOneStaticGroup, x=x, schemaGrouped=schema$staticgroup, endian=endian, offset=offset)
	#out <- unlist(out, recursive=FALSE)
	#print(proc.time()[2:3] - ppp)
	
	
	##ppp <- proc.time()[2:3]
	out <- lapply(static, convertRawOneStaticSingle, x=x, schema=schema, endian=endian, offset=offset)
	names(out) <- schema$var[static]
	##print(proc.time()[2:3] - ppp)
	
	
	#out <- lapply(static, convertRawOneStaticGroup, x=x, schema=schema, endian=endian, offset=offset)
	#names(out) <- schema$var[static]
	
	# Run various processing of the static data prior to extracting the data with dynamic length:
	for(fun in afterStatic){
		out <- do.call(fun, list(x=out, ...))
	}
	
	# Then the dependent:
	#ppp <- proc.time()[3]
	if(length(dependent)){
		temp <- lapply(dependent, convertRawOneDependent, x=x, schema=schema, data=out, endian=endian, offset=offset)
		names(temp) <- schema$var[dependent]
		out <- c(out, temp)
	}
	#print(proc.time()[3] - ppp)
	# Remove empty elements:
	out <- out[lengths(out) > 0]
	
	# Run various processing of the dependent data:
	for(fun in afterDependent){
		out <- do.call(fun, list(x=out, ...))
	}
	
	# Add certain variables for RAW datagrams:
	if(startsWith(attr(schema, "dgName"), "RAW")){
		# Set the 'number', which is the ping index of the file (set to NA here and then reset in readEKRawAll()), and the datagram time:
		out$number <- NA
		out$time <- attr(x, "dgTime")
	}
	
	out
}

#*********************************************
#*********************************************
#' Read the header from a Simrad raw file.
#'
#' @param x			The raw vector.
#' @param endian	The endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#' @param ...		Used for robustness.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#'
readEKRaw_GetFileHeader <- function(x, endian="little", ...){
	
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.

	# Read configuration header:
	offset <- 0
	schema <- readEKRaw_GetSchema("ConfigHeader", group=FALSE)
	
	# Extract the ConfigHeader from the raw vector:
	configheader <- convertRaw(x, schema=schema, endian=endian, offset=offset)
	configheader$time <- attr(x, "dgTime")
	# Set the offset used in the following reading of each transceiver:
	offset <- offset + max(schema$end)
	
	# Extract individual transceiver configurations:
	configXcvr <- list()
	if(configheader$transceivercount>10000 || configheader$transceivercount<0){
		warning(paste0("Data not properly read (transceivercount higher than 10000 or negative (", configheader$transceivercount, ") . Try to change endian)"))
		return(list())
	}
	for(i in seq_len(configheader$transceivercount)){
		# Read transceiver configuration:
		schema <- readEKRaw_GetSchema("TransceiverConfig", group=FALSE)
		# Extract the TransceiverConfig from the raw vector:
		thisconfigXcvr <- convertRaw(x, schema=schema, endian=endian, offset=offset)
		# Set the offset used in the next reading of each transceiver:
		offset <- offset + max(schema$end)
		
		for(j in seq_along(thisconfigXcvr)){
			configXcvr[[names(thisconfigXcvr )[j]]][[i]] <- thisconfigXcvr[[j]]
		}
	}
	# Collapse the tables into matrices:
	for(i in seq_along(configXcvr)){
		if(length(configXcvr[[i]][[1]])>1){
			configXcvr[[i]] <- matrix(unlist(configXcvr[[i]]), byrow=TRUE, nrow=configheader$transceivercount)
		}
	}
	if(length(configXcvr)>0){
		names(configXcvr) <- names(thisconfigXcvr)
	}
	
	# Return the header and tranceiver configurations:
	list(header=configheader, transceiver=configXcvr)
}

#*********************************************
#*********************************************
#' Reads the a datagram header stored in a Simrad raw file.
#'
#' @param x	The raw vector.
#' @param timeOffset  is the time offset of the datagram.
#' @param endian is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31). NA
#' @param xBase  is the base of Windows FILETIME: xBase=unclass(as.POSIXct('1601-1-1', tz="UTC"))[1].
#' @param offset an offset in bytes.
#' @param ...	Used for robustness.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD FILETIME2mtim
#'
#' @export
#'
readEKRaw_GetDgHeader <- function(x, timeOffset=0, endian="little", xBase=-11644473600, offset=0, ...){
	
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	
	# Read datagram type:
	s <- seq_len(4) + offset
	
	dgType <- rawToChar(x[s])
	# If no data was read, end the function:
	if(length(dgType)==0){
		return(list())
	}
	
	#  Read datagram time (NT Time - number of 100-nanosecond intervals since January 1, 1601):
	lowdatetime <- readBin(x[4 + s], what="int", n=1, size=4, endian=endian, signed=TRUE)
	if(length(lowdatetime)>0 && lowdatetime<0){
		lowdatetime <- lowdatetime + 2^32
	}
	highdatetime <- readBin(x[8 + s], what="int", n=1, size=4, endian=endian, signed=TRUE)
	# Convert to unsingned integer, which is not supported in R:
	# Convert NT time to MATLAB serial time:
	FILETIME <- (highdatetime * 2 ^ 32 + lowdatetime) + timeOffset/86400
	
	dgTime <- TSD::FILETIME2mtim(FILETIME, xBase=xBase)
	
	# Return:
	list(dgType=dgType, dgTime=dgTime)
}

#*********************************************
#*********************************************
#' Modify parts of EK80/WBAT .raw files
#'
#' Provides the facility to modify parts of EK80/WBAT .raw files so that these files can be read by the Large Scale Survey System (LSSS) software.
#'
#' @param filename The path to directory of raw files or a vector of the paths to the raw files.
#' @param newDir The directory in which to put the modified files.
#' @param t The time steps to extract the data for (use the default t="all" to keep all time steps)
#' @param addMRU logical; if TRUE the funciton inserts a dummy MRU0 datagram prior to RAW3 datagrams.
#' @param fixBeamType logical; if TRUE the BeamType is fixed to 1.
#' @param fixSlope logical; if TRUE the slope is set to a default.
#' @param addTransducerSerialNumber A string giving the transducer serial number.
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
#' @rdname readEKRaw_extractPings
#' 
readEKRaw_extractPings <- function(filename, newDir, t="all", addMRU=TRUE, fixBeamType=TRUE, fixSlope=TRUE, addTransducerSerialNumber="114", endian="little", msg=TRUE){
	
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
	
	# 't' may be given as "all", indicating Inf (all time steps):
	if(TSD::strff("all",t)){
		t=Inf
	}
	
	# Very large values of 't' are interpreted as Inf:
	if(any(nchar(t)>4)){
		mtim=TSD::interpret.mtim(t)
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
	# (DG2) Skip bytes:
	dgHeader = readEKRaw_ReadDgHeader(fid, timeOffset=0, endian=endian)
	if(dgHeader$dgType == "CON0"){
		# (DG3) Skip bytes:
		config = readEKRaw_ReadHeader(fid, dgHeader, endian=endian)
		# Skip bytes:
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
		warning("No CON0 datagram at the beginning of the file.")
		# (DG3) Skip bytes:
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

#*********************************************
#*********************************************
#' Extracts one or more beams from raw data read by readEKRaw().
#' 
#' Used in \code{\link\{readEKRaw_split}}.
#'
#' @param x			A list of data read by readEKRaw().
#' @param beamnr	A vector of indices of the beams to extract.
#' @param drop		Logical: if TRUE drop the output when only one beam is extracted.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' 
readEKRaw_extractBeams <- function(x, beamnr=NULL, drop=FALSE){
	# Remove variables with missing names (left overs from the reading function, but should be removed in the future):
	x$data$config <- x$data$config[!is.na(names(x$data$config))]
	x$data$pings <- x$data$pings[!is.na(names(x$data$pings))]
	
	# 'beamnr' can be NULL, a list of vectors, or a vector that will be converted to a list:
	nbeams <- length(x$data$config[[1]])
	if(length(beamnr)==0){
		beamnr <- as.list(seq_len(nbeams))
	}
	else{
		if(!is.list(beamnr)){
			beamnr <- as.list(beamnr)
			}
		beamnr <- lapply(beamnr, intersect, seq_len(nbeams))
		}
	
	out = vector("list", length(beamnr))
	for(i in seq_along(out)){
		out[[i]] <- x
		out[[i]]$header$transceivercount <- length(beamnr[[i]])
		# Extract the beams from the config:
		out[[i]]$data$config <- lapply(out[[i]]$data$config, function(xx) if(length(dim(xx))==2) xx[beamnr[[i]], , drop=FALSE] else xx[beamnr[[i]]])
		# Extract the beams from the pings:
		out[[i]]$data$pings <- lapply(out[[i]]$data$pings, function(xx) if(is.list(xx)) lapply(xx, function(xxx) xxx[, beamnr[[i]], drop=FALSE]) else xx[beamnr[[i]], , drop=FALSE])
		}
	if(drop && length(out)==1){
		out <- out[[1]]
		beamnr <- beamnr[[1]]
	}
	invisible(list(data=out, beamnr=beamnr))
}

