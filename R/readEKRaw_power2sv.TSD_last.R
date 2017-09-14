#*********************************************
#*********************************************
#' Converts power to volume backscattering coefficient from data in the TSD format.
#'
#' @param x  is a list of the data (one element for each time step!!!).
#' @param beams  is a list of the beam configuration of the sonar or echosounder.
#' @param cali  optinal calibration information containing gain and Sa_correction values, possibly given pulse length values and frequencies.
#' @param list.out  is TRUE to return the data as a list of acoustic values and calibration values.
#' @param tiltcorr  is TRUE to apply the tilt correction used for fishery sonars.
#' @param toTS  is TRUE to apply the TS calibration instead for the Sv calibration.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD dim_all listOfEqual2array
#'
#' @export
#' @rdname readEKRaw_power2sv.TSD
#'
readEKRaw_power2sv.TSD_last <- function(x, beams=list(), cali=NULL, list.out=FALSE, tiltcorr=0, toTS=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	# Update: 2015-04-27 - Fixing bugs.
	# Update: 2015-12-03 - Using full dimension for all elements to ensure that nothing wrong is happening.
	# Last: 2015-12-21 - Added the calibration data as a list returned from a calibration xml file.
	########### DESCRIPTION: ###########
	# Converts power to volume backscattering coefficient from data in the TSD format.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a list of the data (one element for each time step!!!).
	# ---beams--- is a list of the beam configuration of the sonar or echosounder.
	# ---cali--- optinal calibration information containing gain and Sa_correction values, possibly given pulse length values and frequencies.
	# ---list.out--- is TRUE to return the data as a list of acoustic values and calibration values.
	# ---tiltcorr--- is TRUE to apply the tilt correction used for fishery sonars.
	# ---toTS--- is TRUE to apply the TS calibration instead for the Sv calibration.
	

	##################################################
	##################################################
	# Function used for expanding the dimenstions of the beams variables:
	expandBeamsVars <- function(beams){
		expandVar <- function(x){
			d <- dim(x)
			l <- length(x)
			if(length(d)==0){
				x <- matrix(x, nrow=beams$numb, ncol=beams$numt, byrow=l==beams$numt)
			}
			x
		}
		tobeexpanded <- !names(beams) %in% c("numb", "numt")
		beams[tobeexpanded] <- lapply(beams[tobeexpanded], expandVar)
		beams
	}
	# Convenient function for conversion from log to linear values:
	exp10 <- function(x){
		10^(x/10)
		}
	
	# Detect the type of raw file:
	if(length(beams$gai1)>0 || (is.list(x) && length(x$data$pings$gaintx)>0)){
		raw=1
		}
	else{
		raw=0
		}
	
	# If an object as read directly using readEKRaw() is used, extract the beams information here:
	if(is.list(x) && length(x$data)>0){
		# Beams variables with different location in raw0 and raw1:
		# Raw1:
		if(raw==1){
			beams = list(
				# 1. Sa-correction: sacr:
				sacr = x$data$pings$sacorrection, 
				# 2. Gain: gai1, gai2:
				gai1 = x$data$pings$gaintx, 
				gai2 = x$data$pings$gainrx, 
				# 3. Equivalent beam angle: eqba:
				eqba = x$data$pings$equivalentbeamangle, 
				# 4. Elevation angle (tilt): dirx:
				dirx = x$data$pings$dirx)
			}
		# Raw0:
		else{
			beams = list(
				# 1. Sa-correction, sacr:
				sacr = x$data$config$sacorrectiontable[,1], 
				# 2. Gain: gain:
				gain = x$data$config$gain, 
				# 3. Equivalent beam angle: eqba:
				eqba = x$data$config$equivalentbeamangle, 
				# 4. Elevation angle (tilt): dirx:
				dirx = x$data$config$dirx)
			}	
		
		# Beams variables with the same location in raw0 and raw1:
		# 5. Transmit power: tpow:
		beams$tpow = x$data$pings$transmitpower
		# 6. Pulse length: plsl:
		beams$plsl = x$data$pings$pulselength[1,]
		# 7. average speed of sound: asps:
		beams$asps = x$data$pings$soundvelocity[1,]
		# 8. frequency: freq:
		beams$freq = x$data$pings$frequency
		}
	else{
		x = list(data=list(pings=list(power=x)))
		}
	
	# Expand all beams variables to a matrix with dimension c(numb , numt):
	beams$numt = ncol(beams$freq)
	beams$numb = nrow(beams$freq)
	beams <- expandBeamsVars(beams)
	

	# Apply input calibration:
	if(length(cali)>0 && is.list(cali)){
		if(length(cali$gain)==1){
			beams$gain = cali$gain
			}
		else if(length(dim(cali$gain))>1){
			# Disregard frequency but pick out the closest pulse lengths:
			closestplsl = apply(outer(cali$gain[,"pl"]*1e-6, beams$plsl), 2, which.min)
			beams$gain = cali$gain[closestplsl,1]
			closestplsl = apply(outer(cali$sacr[,"pl"]*1e-6, beams$plsl), 2, which.min)
			beams$sacr = cali$sacr[closestplsl,1]
			}
		}
	else{
		if(raw==1){
			beams$gain = beams$gai1 + beams$gai2
			}
		else{
			beams$gain = 2 * beams$gain # Multiply by 2 to correspond to the summation of two gains when raw==1
			}
		}
	
	# Important: expand the pulselength and speed of sound to matrix using byrow=TRUE:
	#beams$plsl = matrix(beams$plsl, ncol=ncol(beams$freq), nrow=nrow(beams$freq), byrow=TRUE)
	#beams$asps = matrix(beams$asps, ncol=ncol(beams$freq), nrow=nrow(beams$freq), byrow=TRUE)
	#beams$gain = matrix(beams$gain, ncol=ncol(beams$freq), nrow=nrow(beams$freq), byrow=TRUE)
	
	
	
	# Calculate wavelength:
	beams$lmbd =  beams$asps / beams$freq
	
	# Elevation angle correction. Adopted from Gavin's code on 2015-05-19. Note that the tilt is negative below horizontal, thus the minus sign. However, this has no effect since cos(x) = cos(-x):
	if(isTRUE(tiltcorr[[1]])){
		cat("Tilt-correction applied for fishery sonar\n")
		beams$tiltcorr = 40*log10(cos(-beams$dirx*pi/180))
		}
	else{
		beams$tiltcorr = tiltcorr
		}
		
	# Set correct dimensions:
	beams[c("tpow", "lmbd", "asps", "plsl", "gain", "tiltcorr", "sacr", "eqba")] = lapply(beams[c("tpow", "lmbd", "asps", "plsl", "gain", "tiltcorr", "sacr", "eqba")], array, dim=c(beams$numb, beams$numt))
	if(toTS){
		beams$calf = 10 * log10( beams$tpow * beams$lmbd^2 / (16*pi^2) ) + beams$gain + beams$tiltcorr
		}
	else{
		beams$calf = 10 * log10( beams$tpow * beams$lmbd^2 * beams$asps * beams$plsl / (32*pi^2) ) + beams$gain + beams$tiltcorr + 2*beams$sacr + beams$eqba
		}
	
	# Drop to an array if all time steps have identical dimensions:
	x$data$pings$power = TSD::listOfEqual2array(x$data$pings$power)
	if(is.list(x$data$pings$power)){
		for(i in seq_len(beams$numt)){
			thisdim = TSD::dim_all(x$data$pings$power[[i]])
			# The power field in the raw files is already in dB, so we need to linearize it here:
			x$data$pings$power[[i]] = exp10(x$data$pings$power[[i]] - rep(beams$calf[,i], each=thisdim[1]))
			}
		}
	else{
		x$data$pings$power = exp10(x$data$pings$power - rep(beams$calf, each=dim(x$data$pings$power)[1]))
		}
	#dims = lapply(x$data$pings$power, dim_all)
	#if(identical(rep(dims[[1]],length(dims)), unlist(dims, use.names=FALSE))){
	#	x$data$pings$power = unlist(x$data$pings$power, use.names=FALSE)
	#	thisdim = c(dims[[1]], length(dims))
	#	dim(x$data$pings$power) = thisdim
	#	# The power field in the raw files is already in dB, so we need to linearize it here:
	#	x$data$pings$power = exp10(x$data$pings$power - rep(beams$calf, each=thisdim[1]))
	#	}
	# Apply 'beams$calf' to each time step:
	#else{
	#	for(i in seq_len(beams$numt)){
	#		thisdim = dim_all(x$data$pings$power[[i]])
	#		# The power field in the raw files is already in dB, so we need to linearize it here:
	#		x$data$pings$power[[i]] = exp10(x$data$pings$power[[i]] - rep(beams$calf[,i], each=thisdim[1]))
	#		}
	#	}
	
	# Return:
	if(list.out){
		list(vbsc=x$data$pings$power, Cgai=beams$gain, Csac=beams$sacr, Ctcr=beams$tiltcorr, Ccal=beams$calf)
		}
	else{
		x$data$pings$power
		}
	##################################################
	##################################################
	}
