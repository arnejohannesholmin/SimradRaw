#*********************************************
#*********************************************
#' Reads a Simrad raw file.
#'
#' @param f				The path to the raw file.
#' @param t				A vector of the time steps to read.
#' @param endian'		The endian of the file, defaulted to "little".
#' @param timeOffset	The time offset of the datagram.
#' @param drop.out		Logical: If TRUE drop dimensions of the output data.
#' @param msg,pb		See ?TSD::papply.
#' @param splitByPings  Logical: If TRUE split the acoustic data into a list of single pings and strip off ranges with all NAs at the end of the beams (corresponding to the output from \code{\link{readEKRawOld}}).
#' @param complex.out	Logical: If TRUE output also the complex values stored in the raw file for the RAW1 file format (fishery sonar).
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
readEKRaw <- function(f, t=1, endian="little", timeOffset=0, drop.out=FALSE, msg=TRUE, pb=TRUE, splitByPings=FALSE, complex.out=FALSE, ...){
	
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
	list2arrayAddNA <- function(raw, numb, splitByPings=FALSE, drop.out=FALSE){
		
		# Transpose the list from one element per channel to one element per variable:
		numraw <- length(raw)
		prenames <- names(raw[[1]])
		raw = data.table::as.data.table(raw)
		raw = as.list(data.table::data.table(t(raw)))
		names(raw) <- prenames
		
		# Get ping indices as incemeneting when the 'channel' variable resets:
		# Support for channel not present:
		if(length(raw$channel)==0){
			raw$channel <- as.list(rep(1, numraw))
		}
		
		# Support for only one channel per ping:
		if(all(unlist(raw$channel) == raw$channel[[1]])){
			pingID <- seq_along(raw$channel)
		}
		else{
			channelID <- unlist(raw$channel)
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
		raw <- lapply(raw, fillNA, numb, numt)
		
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
	readEKRaw_getDatagram <- function(x, ...){
		# Get the 'dgName' from the attributes of the first element:
		dgName <- attr(x[[1]], "dgName")
		
		if(dgName %in% c("NME0", "TAG0", "SVP0")){
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
			fun <- function(x, endian="little", ...){
				# readEKRaw_GetRAW(x, dgName="RAW0", endian=endian, ...)
				browser()
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
		else if(dgName %in% "MRU0"){
			fun <- readEKRaw_GetMRU
		}
		else if(dgName %in% "CON0"){
			fun <- readEKRaw_GetFileHeader
		}
		else if(dgName %in% "CON1"){
			fun <- readEKRaw_GetXML
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
	
	# Exclude the first datagram, which has already been read:
	#data <- data[-1]
	
	# Apply the conversion given datagram name:
	data <- TSD::papply(data, readEKRaw_getDatagram, endian="little", timeOffset=0, xBase=-11644473600, complex.out=complex.out, msg = NULL, pb = TRUE)
	
	
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
	data[[rawDatagram]] <- list2arrayAddNA(data[[rawDatagram]], numb=numb, splitByPings=splitByPings, drop.out=drop.out)
	names(data)[atRawDatagram] <- "pings"
	
	# Reset the ping index 'number' to the ping indices:
	data$pings$number <- if(length(dim(data$pings$number))==0) seq_along(data$pings$number) else col(data$pings$number)
	numt <- max(data$pings$number)
	
	if(length(data$DEP0)){
		data$DEP0 <- list2arrayAddNA(data$DEP0, numb=1, splitByPings=splitByPings, drop.out=drop.out)
		data$DEP0 <- as.data.frame(data$DEP0, stringsAsFactors=FALSE)
	}
	if(length(data$MRU0)){
		data$MRU0 <- list2arrayAddNA(data$MRU0, numb=1, splitByPings=splitByPings, drop.out=drop.out)
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
		x$angle <- matrix(angle, nrow=x$count, ncol=2, byrow=TRUE)
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
	atVersion <- which(sapply(data$XML0, function(x) "version" %in% names(x)))[1]
	
	data$XML0 <- data$XML0[-c(atTransceivers, atEnvironment, atVersion)]
	
	data
}


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
	out <- lapply(config$Transceivers, function(x) getTransducerVarOne(x$Channels$Channel$Transducer, var))
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
	out <- lapply(config$Transceivers, getFrequencyParOne)
	out <- unname(out)
	out
}
getDataConfigRaw3 <- function(config){
	out <- list(
		channelid                   = sapply(config$Transceivers, function(x) x$Channels$Channel$.attrs["ChannelID"]),
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
		pulselengthtable            = do.call(rbind, lapply(config$Transceivers, function(x) splitByChar(x$Channels$Channel$.attrs["PulseDuration"]))),
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
