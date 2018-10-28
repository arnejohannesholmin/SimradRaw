#*********************************************
#*********************************************
#' Reads the transceiver configuration from a Simrad raw file.
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
#' @rdname readEKRaw_ReadTransceiverConfig
#'
readEKRaw_ReadTransceiverConfig<-function(fid, endian="little"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Reads the transceiver configuration from a Simrad raw file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---fid--- is the path to the raw file.
	# ---endian--- is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	

	##################################################
	##################################################
	# Based on code by Lars Nonboe Andersen, Simrad.
	configXcvr = list()
	configXcvr[["channelid"]] =						readChar(con=fid, nchars=128, useBytes=TRUE)
	configXcvr[["beamtype"]] =						readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["frequency"]] =						readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["gain"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["equivalentbeamangle"]] =			readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["beamwidthalongship"]] =			readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["beamwidthathwartship"]] =			readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["anglesensitivityalongship"]] =		readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["anglesensitivityathwartship"]] =	readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["anglesoffsetalongship"]] =			readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["angleoffsetathwartship"]] =		readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["posx"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["posy"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["posz"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["dirx"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["diry"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["dirz"]] =							readBin(fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	configXcvr[["pulselengthtable"]] =				readBin(fid, what="double", n=5, size=4, endian=endian, signed=TRUE)
	configXcvr[["spare2"]] =						readChar(con=fid, nchars=8, useBytes=TRUE)
	configXcvr[["gaintable"]] =						readBin(fid, what="double", n=5, size=4, endian=endian, signed=TRUE)
	configXcvr[["spare3"]] =						readChar(con=fid, nchars=8, useBytes=TRUE)
	configXcvr[["sacorrectiontable"]] =				readBin(fid, what="double", n=5, size=4, endian=endian, signed=TRUE)
	configXcvr[["spare4"]] =						readChar(con=fid, nchars=52, useBytes=TRUE)
	
	# Return the transceiver configuration:
	configXcvr
	##################################################
	##################################################
	}
