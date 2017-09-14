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
readEKRaw_power2sv.TSD_old<-function(x,beams){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	# Last: 2015-04-27 - Fixing bugs.
	########### DESCRIPTION: ###########
	# Converts power to volume backscattering coefficient from data in the TSD format.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is a list of the data.
	# ---beams--- is a list of the beam configuration of the sonar or echosounder.
	

	##################################################
	##################################################
	# Detect the type of raw file:
	if(length(beams$gai1)>0){
		raw=1
		}
	else{
		raw=0
		}
	
	numt=length(x)
		
	#  extract calibration parameters for this xcvr
	c = beams$asps
	f = beams$freq
	# Get the correct gain to use:
	if(raw==1){
		gain = beams$gai1 + beams$gai2
		phi = beams$eqba
		}
	else{
		gain = 2*beams$gain # Multiply by 2 to correspond to the summation of two gains when raw==1
		phi = beams$eqba
		#dimgain = c(length(gain), numt)
		if(length(dim(gain))<2){
			gain = array(gain, dim=c(length(gain), numt))
			phi = array(phi, dim=c(length(phi), numt))
			}
		}
	pt = beams$tpow
	tau = beams$plsl
	sacr = beams$sacr
	sacr = array(sacr, dim=c(NROW(sacr), numt))
	# Calculate wavelength:
	lambda =  c / f
	# Impedance:
	if(raw==1){
		impedance = 53
		}
	else{
		impedance = 1
		}
	
	save(list=c("gain", "sacr", "phi", "pt", "lambda", "c", "tau", "impedance"), file="testDrop_old.txt")
	C = 10^drop( - (gain + 2*sacr + phi) / 10 )  *  (32*pi^2)  /  drop(pt * lambda^2 * c * tau * impedance)
	
	# Apply 'C' to each time step:
	for(i in seq_len(numt)){
		thisdim = dim_all(x[[i]])
		# The power field in the raw files is already in dB, so we need to linearize it here:
		x[[i]] = 10^(x[[i]]/10) * matrix(C[,i], nrow=thisdim[1], ncol=thisdim[2], byrow=TRUE)
		}
	
	# Return:
	x
	##################################################
	##################################################
	}
