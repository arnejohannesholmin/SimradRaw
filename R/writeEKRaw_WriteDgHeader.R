#*********************************************
#*********************************************
#' Writes the sample data to a Simrad raw0 file.
#'
#' @param fid  is the path to the raw file.
#' @param dgType  is the datagram type.
#' @param dgTime  is the time stamp of the datagram.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#' @param tz  is the the time zone. Overrides 'xBase' in mtim2FILETIME() if not given as tz="UTC". See as.POSIXlt().
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD mtim2FILETIME
#'
#' @export
#' @rdname writeEKRaw_WriteDgHeader
#'
writeEKRaw_WriteDgHeader<-function(fid, dgType, dgTime, endian="little", tz="UTC"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Writes the sample data to a Simrad raw0 file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---fid--- is the path to the raw file.
	# ---dgType--- is the datagram type.
	# ---dgTime--- is the time stamp of the datagram.
	# ---endian--- is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	# ---tz--- is the the time zone. Overrides 'xBase' in mtim2FILETIME() if not given as tz="UTC". See as.POSIXlt().
	

	##################################################
	##################################################
	# Write datagram type:
	writeChar(dgType, con=fid, nchars=4, eos=NULL)
	
	# Convert to the time information stored in the raw files:
	FILETIME = mtim2FILETIME(dgTime, tz=tz)
	#highdatetime = floor(FILETIME/(2^32)) * 2^32
	base = 2^32
	highdatetime = floor(FILETIME/base)
	#lowdatetime = FILETIME - highdatetime
	#lowdatetime = lowdatetime - 2^32
	lowdatetime = FILETIME %% base
	# There is no unsigned integer in R, so we simply write it as signed. This will lead to reduced precision:
	# Convert to unsigned:
	if(length(lowdatetime)>0 && lowdatetime>2^31){
		lowdatetime = lowdatetime - 2^32
		}
	
	#lowdatetime = lowdatetime - base

	#  Write datagram time (NT Time - number of 100-nanosecond intervals since January 1, 1601):
	writeBin(as.integer(lowdatetime), fid, size=4, endian=endian)
	writeBin(as.integer(highdatetime), fid, size=4, endian=endian)
	##################################################
	##################################################
	}
