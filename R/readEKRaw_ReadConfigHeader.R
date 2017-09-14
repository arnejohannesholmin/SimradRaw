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
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Reads the configuration header from a Simrad raw0 file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---fid--- is the path to the raw file.
	# ---endian--- is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	

	##################################################
	##################################################
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
	##################################################
	##################################################
	}
