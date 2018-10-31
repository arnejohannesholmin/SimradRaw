#*********************************************
#*********************************************
#' Writes the sample data to a Simrad raw1 file.
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
#' @export
#' @rdname writeEKRaw_WriteSampledata_RAW1
#'
writeEKRaw_WriteSampledata_RAW1 <- function(fid, data, endian="little"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Writes the sample data to a Simrad raw1 file.
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
	writeBin(as.integer(data$datatype), con=fid, size=1, endian=endian)
	writeBin(as.integer(data$ncomplexpersample), con=fid, size=1, endian=endian)
	writeBin(as.double(data$gaintx), con=fid, size=4, endian=endian)
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
	writeBin(as.double(data$heading), con=fid, size=4, endian=endian)
	writeBin(as.integer(data$transmitmode), con=fid, size=2, endian=endian)
	writeBin(as.integer(data$pulseform), con=fid, size=2, endian=endian)
	writeBin(as.double(data$dirx), con=fid, size=4, endian=endian)
	writeBin(as.double(data$diry), con=fid, size=4, endian=endian)
	writeBin(as.double(data$dirz), con=fid, size=4, endian=endian)
	writeBin(as.double(data$gainrx), con=fid, size=4, endian=endian)
	writeBin(as.double(data$sacorrection), con=fid, size=4, endian=endian)
	writeBin(as.double(data$equivalentbeamangle), con=fid, size=4, endian=endian)
	writeBin(as.double(data$beamwidthalongshiprx), con=fid, size=4, endian=endian)
	writeBin(as.double(data$beamwidthathwartshiprx), con=fid, size=4, endian=endian)
	writeBin(as.double(data$anglesensitivityalongship), con=fid, size=4, endian=endian)
	writeBin(as.double(data$anglesensitivityathwartship), con=fid, size=4, endian=endian)
	writeBin(as.double(data$angleoffsetalongship), con=fid, size=4, endian=endian)
	writeBin(as.double(data$angleoffsetathwartship), con=fid, size=4, endian=endian)
	suppressWarnings(writeChar(data$spare, con=fid, nchars=2, eos=NULL))
	writeBin(as.integer(data$noisefilter), con=fid, size=2, endian=endian)
	writeBin(as.integer(data$beamwidthmode), con=fid, size=2, endian=endian)
	writeBin(as.integer(data$beammode), con=fid, size=2, endian=endian)
	writeBin(as.double(data$beamwidthhorizontaltx), con=fid, size=4, endian=endian)
	writeBin(as.double(data$beamwidthverticaltx), con=fid, size=4, endian=endian)
	writeBin(as.integer(data$offset), con=fid, size=4, endian=endian)
	
	# Update data$count after removing missing values in the acoustic or angle data:
	data$count=suppressWarnings(sum(!is.na(data$data)))
	writeBin(as.integer(data$count), con=fid, size=4, endian=endian) # 132 bytes in total
	
	# Write the complex acoustic data:
	if(integer.base.b(data$datatype, endian=endian)[4]==1){
		s <- seq_len(data$count)
		writeBin(as.double(c(t(cbind(Re(data$data[s]), Im(data$data[s]))))), con=fid, size=4, endian=endian)
		}
	##################################################
	##################################################
	}
