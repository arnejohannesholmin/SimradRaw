#*********************************************
#*********************************************
#' Writes the sample data to a Simrad raw0 file.
#'
#' @param fid  is the path to the raw file.
#' @param data  is a list of sample data to write to the Simrad raw file.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD rm.na
#'
#' @export
#' @rdname writeEKRaw_WriteSampledata_RAW0
#'
writeEKRaw_WriteSampledata_RAW0<-function(fid, data, endian="little"){
	
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
	# ---data--- is a list of sample data to write to the Simrad raw file.
	# ---endian--- is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	

	##################################################
	##################################################
	# Write the elements of the 'data':
	writeBin(as.integer(data$channel), con=fid, size=2, endian=endian)
	mode_high <- floor(data$mode/256) * 256
	mode_low <- data$mode - mode_high
	mode_high <- mode_high / 256
	writeBin(as.integer(mode_low), con=fid, size=1, endian=endian)
	writeBin(as.integer(mode_high), con=fid, size=1, endian=endian)
	writeBin(as.double(data$transducerdepth), con=fid, size=4, endian=endian)
	writeBin(as.double(data$frequency), con=fid, size=4, endian=endian)
	writeBin(as.double(data$transmitpower), con=fid, size=4, endian=endian)
	writeBin(as.double(data$pulselength), con=fid, size=4, endian=endian)
	writeBin(as.double(data$bandwidth), con=fid, size=4, endian=endian)
	writeBin(as.double(data$sampleinterval), con=fid, size=4, endian=endian)
	writeBin(as.double(data$soundvelocity), con=fid, size=4, endian=endian)
	writeBin(as.double(data$absorptioncoefficient), con=fid, size=4, endian=endian)
	writeBin(as.double(data$heave), con=fid, size=4, endian=endian)
	writeBin(as.double(data$roll), con=fid, size=4, endian=endian)
	writeBin(as.double(data$pitch), con=fid, size=4, endian=endian)
	writeBin(as.double(data$temperature), con=fid, size=4, endian=endian)
	writeBin(as.integer(data$trawlupperdepthvalid), con=fid, size=2, endian=endian)
	writeBin(as.integer(data$trawlopeningvalid), con=fid, size=2, endian=endian)
	writeBin(as.double(data$trawlupperdepth), con=fid, size=4, endian=endian)
	writeBin(as.double(data$trawlopening), con=fid, size=4, endian=endian)
	writeBin(as.integer(data$offset), con=fid, size=4, endian=endian)
	
	# Update data$count after removing missing values in the acoustic or angle data:
	data$count <- suppressWarnings(max(sum(!is.na(data$power)),sum(!is.na(data$athwartship)),sum(!is.na(data$alongship))))
	writeBin(as.integer(data$count), con=fid, size=4, endian=endian) # 72 bytes in total
	
	# Write the acoustic and angle data:
	if(data$count > 0){
		s <- seq_len(data$count)
		if(data$mode != 2){
			# Power / (10 * log10(2) / 256)
			data$power <- as.integer(data$power[s] / 0.011758984205624)
			writeBin(data$power, con=fid, size=2, endian=endian)
			}
		if(data$mode > 1){
			angle <- c(t(cbind(data$athwartship[s], data$alongship[s])))
			writeBin(as.integer(angle), con=fid, size=1, endian=endian)
			}
		}
	##################################################
	##################################################
	}
