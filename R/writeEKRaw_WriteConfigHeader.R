#*********************************************
#*********************************************
#' Writes the configuration header to a Simrad raw0 file.
#'
#' @param configheader  is a list of the configuration header.
#' @param fid  is the path to the raw file.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname writeEKRaw_WriteConfigHeader
#'
writeEKRaw_WriteConfigHeader<-function(fid, configheader, endian="little"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Writes the configuration header to a Simrad raw0 file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---configheader--- is a list of the configuration header.
	# ---fid--- is the path to the raw file.
	# ---endian--- is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	

	##################################################
	##################################################
	# Write the elements of the 'configheader' list:
	suppressWarnings(writeChar(configheader$surveyname, con=fid, nchars=128, eos=NULL))
	suppressWarnings(writeChar(configheader$transectname, con=fid, nchars=128, eos=NULL))
	suppressWarnings(writeChar(configheader$soundername, con=fid, nchars=128, eos=NULL))
	suppressWarnings(writeChar(configheader$version, con=fid, nchars=30, eos=NULL))
	writeBin(as.integer(configheader$multiplexing), con=fid, size=2, endian=endian)
	writeBin(as.integer(configheader$timebias), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$soundvelocityaverage), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$soundvelocitytransducer), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$mruoffsetx), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$mruoffsety), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$mruoffsetz), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$mrualphax), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$mrualphay), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$mrualphaz), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$gpsoffsetx), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$gpsoffsety), con=fid, size=4, endian=endian)
	writeBin(as.double(configheader$gpsoffsetz), con=fid, size=4, endian=endian)
	suppressWarnings(writeChar(configheader$spare, con=fid, nchars=48, eos=NULL))
	writeBin(as.integer(configheader$transceivercount), con=fid, size=4, endian=endian)
	##################################################
	##################################################
	}
