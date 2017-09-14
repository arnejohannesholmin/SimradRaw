#*********************************************
#*********************************************
#' Reads the sample data from a Simrad raw1 file.
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
#' @rdname readEKRaw_ReadSampledata_RAW1
#'
readEKRaw_ReadSampledata_RAW1<-function(fid, endian="little"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Reads the sample data from a Simrad raw1 file.
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
	sampledata$datatype = readBin(con=fid, what="int", n=1, size=1, endian=endian, signed=TRUE)
	sampledata$ncomplexpersample = readBin(con=fid, what="int", n=1, size=1, endian=endian, signed=TRUE)
	temp = readBin(con=fid, what="double", n=13, size=4, endian=endian, signed=TRUE)
		sampledata$gaintx = temp[1]
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
		sampledata$heading = temp[13]
	sampledata$transmitmode = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$pulseform = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	temp = readBin(con=fid, what="double", n=12, size=4, endian=endian, signed=TRUE)
		sampledata$dirx = temp[1]
		sampledata$diry = temp[2]
		sampledata$dirz = temp[3]
		sampledata$gainrx = temp[4]
		sampledata$sacorrection = temp[5]
		sampledata$equivalentbeamangle = temp[6]
		sampledata$beamwidthalongshiprx = temp[7]
		sampledata$beamwidthathwartshiprx = temp[8]
		sampledata$anglesensitivityalongship = temp[9]
		sampledata$anglesensitivityathwartship = temp[10]
		sampledata$angleoffsetalongship = temp[11]
		sampledata$angleoffsetathwartship = temp[12]
	sampledata$spare = readChar(con=fid, nchars=2, useBytes=TRUE)
	sampledata$noisefilter = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$beamwidthmode = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$beammode = readBin(con=fid, what="int", n=1, size=2, endian=endian, signed=TRUE)
	sampledata$beamwidthhorizontaltx = readBin(con=fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$beamwidthverticaltx = readBin(con=fid, what="double", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$offset = readBin(con=fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	sampledata$count = readBin(con=fid, what="int", n=1, size=4, endian=endian, signed=TRUE) # 132 bytes in total
	# Read acoustic data:
	if(integer.base.b(sampledata$datatype, endian=endian)[4]==1){
		
		# From the matlab code of Simrad:
		# sdata = fread(fid,[2*sampledata.ncomplexpersample,sampledata.count],'float32');
		# sdata = reshape(sdata,[2 sampledata.ncomplexpersample sampledata.count]);
			# % trx32multiratefilterdelay = 16; %samples
		# trx32multiratefilterdelay = 1; %samples
			# % txpulsesamples = round(sampledata.pulselength/sampledata.sampleinterval);
			# % sdata = sdata(:,:,txpulsesamples+1:end);
		# sdata = sdata(:,:,trx32multiratefilterdelay:end);
		# sampledata.count = size(sdata,3); % Update sampledata.count by the result of trx32multiratefilterdelay.
		# sampledata.data = squeeze(complex(sdata(1,:,:),sdata(2,:,:)));
		# % Sum over complex samples: sum(sdata,2), then square, the sum the real and imaginary part, and finally take the 10*log10 of the result:
		# sampledata.power = 10*log10(squeeze(sum((sum(sdata,2)).^2)));
	
		sdata = readBin(con=fid, what="double", n=2 * sampledata$ncomplexpersample * sampledata$count, size=4, endian=endian, signed=TRUE)
		dim(sdata)=c(2, sampledata$ncomplexpersample, sampledata$count)
		#trx32multiratefilterdelay = 1
		#sdata = sdata[,,seq(trx32multiratefilterdelay,sampledata$count), drop=FALSE]
		# Update the count of the current channel:
		#sampledata$count = dim(sdata)[3]
		# Save as complex type:
		sampledata$data = complex(real=sdata[1,,], imaginary=sdata[2,,])
		dim(sampledata$data)=c(sampledata$ncomplexpersample, sampledata$count)
		# Sum over the number of complex values per sample, and take the square of the abs, which is the same as the sum of the real part squared and the imaginary part squared:
		sampledata$power = 10*log10(abs(colSums(sampledata$data))^2)
		}
	sampledata
	##################################################
	##################################################
	}
