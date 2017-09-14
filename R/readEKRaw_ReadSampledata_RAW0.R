#*********************************************
#*********************************************
#' Reads the sample data from a Simrad raw0 file.
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
#' @rdname readEKRaw_ReadSampledata_RAW0
#'
readEKRaw_ReadSampledata_RAW0<-function(fid, endian="little"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Reads the sample data from a Simrad raw0 file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---fid--- is the path to the raw file.
	# ---endian--- is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	

	##################################################
	##################################################
	# Based on code by Lars Nonboe Andersen, Simrad.
	sampledata=list()
	sampledata$channel = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	mode_low = readBin(con=fid, what="int", n=1, size=1, endian=endian, signed=TRUE)
	mode_high = readBin(con=fid, what="int", n=1, size=1, endian=endian, signed=TRUE)
	sampledata$mode = 256 * mode_high + mode_low;
	temp = readBin(con=fid, what="double", n=12, size=4, endian=endian, signed=TRUE)
		sampledata$transducerdepth = temp[1]
		sampledata$frequency = temp[2]
		sampledata$transmitpower = temp[3]
		sampledata$pulselength = temp[4]
		sampledata$bandwidth = temp[5]
		sampledata$sampleinterval = temp[6]
		sampledata$soundvelocity = temp[7]
		sampledata$absorptioncoefficient = temp[8]
		sampledata$heave = temp[9]
		sampledata$roll = temp[10]
		sampledata$pitch = temp[11]
		sampledata$temperature = temp[12]
	sampledata$trawlupperdepthvalid = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$trawlopeningvalid = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$trawlupperdepth = readBin(con=fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$trawlopening = readBin(con=fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$offset = readBin(con=fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$count = readBin(con=fid, what="int", n=1, size=4, endian=endian, signed=TRUE) # 72 bytes in total
	# Read acoustic data and angle data:
	if(sampledata$count > 0){
		if(sampledata$mode != 2){
			power = readBin(con=fid, what="int", n=sampledata$count, size=2, endian=endian, signed=TRUE)
			# Power * 10 * log10(2) / 256
			sampledata$power = (power * 0.011758984205624)
			}
		if(sampledata$mode > 1){
			angle = readBin(con=fid, what="int", n=2*sampledata$count, size=1, endian=endian, signed=TRUE)
			angle=matrix(angle,nrow = sampledata$count,ncol=2,byrow=TRUE)
			sampledata$athwartship = angle[,1]
			sampledata$alongship = angle[,2]
			}
		}
	sampledata
	##################################################
	##################################################
	}
