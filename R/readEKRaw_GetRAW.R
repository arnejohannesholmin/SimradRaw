#*********************************************
#*********************************************
#' Reads the sample data from a Simrad raw1 file.
#'
#' @param x  is a raw vector read from a Simrad raw file.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#'
### readEKRaw_GetRAW_old <- function(x, dgName=c("RAW0", "RAW1"), endian="little", complex.out=FALSE, ...){
### 	
### 	# Get the raw datagram schema:
### 	schema <- readEKRaw_GetSchema(dgName[1])
### 	pos <- max(schema$end)
### 	
### 	# Convert the data up until the acoustic data:
### 	sampledata <- convertRaw(x, schema=schema, endian=endian)
### 	
### 	# Set the 'number', which is the ping index of the file (set to NA here and then reset in readEKRawAll()), and the datagram time:
### 	sampledata$number <- NA
### 	sampledata$time <- attr(x, "dgTime")
### 	
### 	# Get the mode (for RAW0):
### 	sampledata <- getMode(sampledata)
### 	
### 	# Get acoustic data:
### 	sampledata <- readEKRaw_GetAcosuticData(x, sampledata=sampledata, pos=pos, dgName=dgName, endian=endian, complex.out=complex.out)
### 
### 	sampledata
### }
### readEKRaw_GetRAW <- function(x, dgName=c("RAW0", "RAW1"), afterStatic=c("getMode"), afterDependent=c("complex2power", "getAngles", "getPower"), endian="little", complex.out=FALSE, ...){
### 	
### 	out <- convertRaw(x, dgName=dgName, afterStatic=afterStatic, afterDependent=afterDependent, endian=endian, offset=offset, ...)
### 		
### 	out
### }
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







all.equal2 <- function(x, y){
	namesx <- names(x)
	namesy <- names(y)
	inames <- intersect(namesx, namesy)
	diffnamesx <- setdiff(namesx, namesy)
	diffnamesy <- setdiff(namesy, namesx)
	out <- lapply(inames, function(this) all.equal(x[[this]], y[[this]]))
	names(out) <- inames
	if(length(diffnamesx)){
		cat("Objects in x but not in y: ", paste(diffnamesx, collapse=", "), "\n")
	}
	if(length(diffnamesy)){
		cat("Objects in y but not in x: ", paste(diffnamesy, collapse=", "), "\n")
	}
	out[!sapply(out, isTRUE)]
}













