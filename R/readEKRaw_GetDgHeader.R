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
