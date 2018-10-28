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
getMode <- function(x, ...){
	if(length(x$mode_high) && length(x$mode_high)){
		x$mode <- 256 * x$mode_high + x$mode_low
		# Remove the mode components:
		x$mode_high <- NULL
		x$mode_low <- NULL
	}
	x
}
# Function for converting fishery sonar data in complex form to power:
complex2power <- function(x, complex.out=FALSE, ...){
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
getAngles <- function(x, ...){
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
getPower <- function(x, ...){
	if(length(x$power0)){
		# Power * 10 * log10(2) / 256
		x$power <- x$power0 * 0.011758984205624
		# Remove the power0
		x$power0 <- NULL
	}
	x
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

