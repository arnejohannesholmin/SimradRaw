#*********************************************
#*********************************************
#' Writes the transceiver configuration to a Simrad raw file.
#'
#' @param fid  is the path to the raw file.
#' @param configXcvr  is a list of the transceiver configuration.
#' @param xBase  is the index of the time step.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname writeEKRaw_WriteTransceiverConfig
#'
writeEKRaw_WriteTransceiverConfig<-function(fid, configXcvr, i, endian="little"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Writes the transceiver configuration to a Simrad raw file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---fid--- is the path to the raw file.
	# ---configXcvr--- is a list of the transceiver configuration.
	# ---xBase--- is the index of the time step.
	# ---endian--- is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	

	##################################################
	##################################################
	# Write the elements of the 'configXcvr' list:
	suppressWarnings(writeChar(configXcvr$channelid[i], con=fid, nchars=128, eos=NULL))
	writeBin(as.integer(configXcvr$beamtype[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$frequency[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$gain[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$equivalentbeamangle[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$beamwidthalongship[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$beamwidthathwartship[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$anglesensitivityalongship[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$anglesensitivityathwartship[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$anglesoffsetalongship[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$angleoffsetathwartship[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$posx[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$posy[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$posz[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$dirx[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$diry[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$dirz[i]), con=fid, size=4, endian=endian)
	writeBin(as.double(configXcvr$pulselengthtable[i,]), con=fid, size=4, endian=endian)
	suppressWarnings(writeChar(configXcvr$spare2[i], con=fid, nchars=8, eos=NULL))
	writeBin(as.double(configXcvr$gaintable[i,]), con=fid, size=4, endian=endian)
	suppressWarnings(writeChar(configXcvr$spare3[i], con=fid, nchars=8, eos=NULL))
	writeBin(as.double(configXcvr$sacorrectiontable[i,]), con=fid, size=4, endian=endian)
	suppressWarnings(writeChar(configXcvr$spare4[i], con=fid, nchars=52, eos=NULL))
	##################################################
	##################################################
	}
