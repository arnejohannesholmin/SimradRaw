#*********************************************
#*********************************************
#' Calculate the Time Varied Gain function.
#'
#' @param beams  is the list of beams inputs including "lenb" (lengths of beams), "sint" (pulse length), "absr" (absorption coefficent of the beams) and "asps" (average speed of sound).
#' @param x  is the input acoustic data arranged in an array having the radial part along the first dimension, the beams along the second or second and third, and time steps and other optional dimensions along the last dimension (lenb,numb,numt).
#' @param linear  is TRUE if x is in linear volume backscatter values (sv) and FALSE if x is in logarithmic volume backscatter values (Sv).
#' @param TVG.exp  is the exponent of the eamotric spreading of the sound wave, theoretically 2 for Sv and 4 for TS.
#' @param delta  is used in Gavins work.
#' @param R0  is used in Gavins work, shifting the first voxel to be imaginarily placed inside of the sonar.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname get.TVG_old
#'
get.TVG_old<-function(x, beams, linear=TRUE, TVG.exp=2, TVG.Corr=-2){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-04-02 - Clean version.
	########### DESCRIPTION: ###########
	# Calculate the Time Varied Gain function.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is the input acoustic data arranged in an array having the radial part along the first dimension, the beams along the second or second and third, and time steps and other optional dimensions along the last dimension (lenb,numb,numt).
	# ---beams--- is the list of beams inputs including "lenb" (lengths of beams), "sint" (pulse length), "absr" (absorption coefficent of the beams) and "asps" (average speed of sound).
	# ---linear--- is TRUE if x is in linear volume backscatter values (sv) and FALSE if x is in logarithmic volume backscatter values (Sv).
	# ---TVG.exp--- is the exponent of the eamotric spreading of the sound wave, theoretically 2 for Sv and 4 for TS.
	# ---TVG.Corr--- is a correction for the position of the first voxel, placed two voxels inside the transducer as default. Not to be changed unless you know what you are doing.
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(is.list(x)){
		if(!is.null(x$vbsc)){
			x=x$vbsc
			linear=TRUE
			}
		else if(!is.null(x$mvbs)){
			x=x$mvbs
			linear=FALSE
			}
		}
	if(is.null(beams$lenb)){
		lenb=dim(x)[1]
		}
	else{
		lenb=max(beams$lenb)
		}
	# Treatment of inputs 'c', 'tau' and 'K':
	dr = beams$asps[1] * beams$sint[1]/2
	r = 0:(lenb-1) * dr
	# Correct the midpoint of the first voxel:
	r[1] = beams$asps[1] * beams$sint[1]/8
	# Apply the TCG correction:
	r = r + TVG.Corr * dr
	# Set negative ranges to 0:
	r[r<0] = 0
	
	
	##### Execution and output #####
	# Apply the exponent if given in 'TVG.exp', defaulted to 20 log r, suitable for Sv:
	TVG=r^TVG.exp * 10^( outer(r,2*beams$absr/10) )
	# Apply logarithm if requested:
	if(linear){
		TVG
		}
	else{
		10*log10(TVG)
		}
	##################################################
	##################################################
	}
