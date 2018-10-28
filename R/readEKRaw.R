#*********************************************
#*********************************************
#' Reads a Simrad raw file.
#'
#' @param f  is the path to the raw file.
#' @param t  is a vector of the time steps to read.
#' @param endian' is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31). NA
#' @param timeOffset  is the time offset of the datagram.
#' @param drop.out  is TRUE to drop dimensions of the data.
#' @param msg  is TRUE to print a time bar during reading.
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
readEKRaw <- function(f, t=1, endian="little", timeOffset=0, drop.out=FALSE, msg=TRUE, splitByPings=FALSE, complex.out=FALSE, ...){
	
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	# Update: 2015-04-23 - Changed to return pings separately as elements of lists, and filling in NAs to form arrays in each ping, but keeping variable lengths between pings, in order to save space particularly when one ping has long beams and the others have short beams, in which case a lot of NAs will be saved.
	# Update: 2015-10-08 - Added support for reading non-UTF-8 characters such as "Æ".
	# Update: 2015-11-16 - Fixed bug when reading files with long difference in time between beams. Now trying to associate new pings by either that the idx reached numb, or that idx==1.
	# Last: 2016-10-23 - Changed to writing multiple temporary TSD files, speeding up for high number of pings.
	
	# Function for appending NAs:
	appendNA <- function(x, len){
		c(x, TSD::NAs(len - length(x)))[seq_len(len)]
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
			ind <- rep(seq_len(lastdim), each=prod(firstdim))
			out <- split(x, ind)
			out <- lapply(out, array, dim=firstdim)
		}
	
		out
	}
	
	# Function that returns an array filled with NAs where there is no data:
	fillNA <- function(x, numb, numt){
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
		
		x
	}

	# Function for converting a list of RAW* datagrams to a list of arrays filled with NAs for unequal lengths (of the beams):
	#list2arrayAddNA_OneDatagram <- function(rawDatagram, data){
	list2arrayAddNA <- function(raw, numb, splitByPings=FALSE){
		
		# Transpose the list from one element per channel to one element per variable:
		prenames <- names(raw[[1]])
		raw = data.table::as.data.table(raw)
		raw = as.list(data.table::data.table(t(raw)))
		names(raw) <- prenames
		
		# Get ping indices as incemeneting when the 'channel' variable resets:
		channelID <- unlist(raw$channel)
		pingID <- c(1, diff(channelID))
		pingID <- pingID < 0
		pingID <- cumsum(pingID) + 1
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
		raw <- lapply(raw, fillNA, numb, numt)
		
		# If required, split into single pings for the acosutic data (for compatibility with older versions):
		if(splitByPings){
			acosutic <- intersect(c("data", "power", "alongship_e", "athwartship_e"), names(raw))
			if(length(acosutic)){
				raw[acosutic] <- lapply(raw[acosutic], splitByLastDim)
			}
		}
		
		raw
	}
	
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
		x <- rawToCharDotDotDot(x)
		x <- XML::xmlParse(x)
		x <- XML::xmlToList(x)
		x
	}
	# Function for converting the raw vector given datagram name:
	readEKRaw_getDatagram <- function(x, ...){
		# Get the 'dgName' from the attributes of the first element:
		dgName <- attr(x[[1]], "dgName")
		
		if(dgName %in% c("NME0", "TAG0", "SVP0", "CON1")){
			fun <- readEKRaw_GetText
		}
		else if(dgName %in% "XML0"){
			fun <- readEKRaw_GetXML
		}
		else if(dgName %in% "RAW0"){
			fun <- function(x, endian="little", ...){
				# readEKRaw_GetRAW(x, dgName="RAW0", endian=endian, ...)
				convertRaw(x, dgName="RAW0", endian=endian, ...)
			}
		}
		else if(dgName %in% "RAW1"){
			fun <- function(x, endian="little", complex.out=FALSE, ...){
				# readEKRaw_GetRAW(x, dgName="RAW1", endian=endian, complex.out=complex.out, ...)
				convertRaw(x, dgName="RAW1", endian=endian, complex.out=complex.out, ...)
			}
		}
		else if(dgName %in% "FIL1"){
			fun <- function(x, endian="little", ...){
				convertRaw(x, dgName="FIL1", endian=endian, ...)
			}
		}
		else if(dgName %in% "CON0"){
			fun <- readEKRaw_GetHeader
		}
		else{
			warning("Unknown datagram name ", dgName, ". Read as text.")
			fun <- readEKRaw_GetText
		}
		
		# Apply the funciton over all elements:
		out <- lapply(x, fun, ...)
	}
	
	
	
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
	
	
	########## First: ##########
	# Extract the first datagram as either CON0 (rAW0 and RAW1) or XML0 (RAW3):
	if(any(temp$dg$dgName == "CON0")){
		atCON0 <- temp$dg$dgName == "CON0"
		atCON0 <- seq(temp$dg$starts[atCON0], temp$dg$end[atCON0])
		config <- readEKRaw_GetHeader(temp$raw[atCON0])
		# Get the number of beams:
		numb <- config$header$transceivercount
	}
	else if(any(temp$dg$dgName == "XML0")){
		atXML0 <- which(temp$dg$dgName == "XML0")[1]
		atXML0 <- seq(temp$dg$starts[atXML0], temp$dg$end[atXML0])
		config <- readEKRaw_GetXML(temp$raw[atXML0])
		# Get the number of beams:
		numb <- length(config$Transceivers)
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
	
	
	# Apply the conversion given datagram name:
	data <- TSD::papply(data, readEKRaw_getDatagram, endian="little", timeOffset=0, xBase=-11644473600, complex.out=complex.out)
	
	
	########## Header: ##########
	# Interpret the CON0 datagram:
	if(any(names(data) == "CON0")){
		# Move the CON0 datagram to a 'config' list:
		config <- data$CON0[[1]]
		#data$CON0 <- NULL
		# Get the number of beams:
		numb <- config$header$transceivercount
	}
	else if(any(names(data) == "XML0")){
		# Move the CON0 datagram to a 'config' list:
		config <- data$XML0[[1]]
		#data$CON0 <- NULL
		# Get the number of beams:
		numb <- length(config$Transceivers)
	}
	else{
		warning("No CON0 datagram at the beginning of the file.")
		config <- list()
		numb <- 1
	}
	
	# Add the transceiver configuration to the data:
	data$config <- config$transceiver
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
	
	browser()
	
	# Convert to a list of variables for the RAW* datagrams, and rename to "pings" according to the original Matlab structure by dr. Rick Towler, NOAA Alaska Fisheries Science Center:
	data[[rawDatagram]] <- list2arrayAddNA(data[[rawDatagram]], numb=numb, splitByPings=splitByPings)
	names(data)[atRawDatagram] <- "pings"
	# Reset the 'number' to the ping indices:
	data$pings$number <- col(data$pings$number)
	
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
	
	# Drop the empty dimensions if required:
	if(drop.out){
		data$pings <- lapply(data$pings, drop)
	}
	numt <- ncol(data$pings$channel)
	
	
	# Order the data by name to resemble the old version:
	data <- data[order(names(data))]
	
	
	# Return the header and data:
	list(header=config$header, data=data, numt=numt, numb=numb, datagramLengths=temp$dg$dgLen)
	##################################################
	##################################################
}
