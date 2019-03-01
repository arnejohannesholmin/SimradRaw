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
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Reads the a datagram header stored in a Simrad raw file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---fid--- is the path to the raw file.
	# ---timeOffset--- is the time offset of the datagram.
	# ---endian' is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	# ---xBase--- is the base of Windows FILETIME: xBase=unclass(as.POSIXct('1601-1-1', tz="UTC"))[1].
	

	##################################################
	##################################################
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
	##################################################
	##################################################
}
