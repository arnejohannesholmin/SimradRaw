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
