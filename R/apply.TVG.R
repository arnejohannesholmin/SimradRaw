#*********************************************
#*********************************************
#' Adds or removes TVG.
#'
#' @param x  is the input acoustic data arranged in an array having the radial part along the first dimension, the beams along the second or second and third, and time steps and other optional dimensions along the last dimension (lenb,numb,numt).
#' @param rm  is TRUE to remove the TVG instead of adding it.
#' @param beams  is the list of beams inputs including "lenb" (lengths of beams), "sint" (pulse length), "absr" (absorption coefficent of the beams) and "asps" (average speed of sound).
#' @param linear  is TRUE if x is in linear volume backscatter values (sv) and FALSE if x is in logarithmic volume backscatter values (Sv).
#' @param TVG.exp  is the exponent of the eamotric spreading of the sound wave, theoretically 2 for Sv and 4 for TS.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname apply.TVG
#'
apply.TVG<-function(x, beams, rm=FALSE, linear=TRUE, TVG.exp=2, Ro=NULL, thr1m=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2009-08-07 - Bad version.
	# Update: 2009-08-13 - Clean version.
	# Update: 2009-09-15 - Added support for list inputs 'beam' and 'dyn', containing 'alpha', and 'c', 'tau' and 'K' respectively.
	# Update: 2010-02-09 - Changed to support list input using the CTD format.
	# Update: 2010-11-09 - Included the 'ctd' input in 'beams'.
	# Update: 2014-04-02 - Changed to apply get.TVG().
	# Last: 2015-04-23 - Changed to accept list for 'x' and added the option 'rm', so that apply.TVG() now is the only function used for both adding and removing TVG.
	########### DESCRIPTION: ###########
	# Adds or removes TVG.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is the input acoustic data arranged in an array having the radial part along the first dimension, the beams along the second or second and third, and time steps and other optional dimensions along the last dimension (lenb,numb,numt).
	# ---rm--- is TRUE to remove the TVG instead of adding it.
	# ---beams--- is the list of beams inputs including "lenb" (lengths of beams), "sint" (pulse length), "absr" (absorption coefficent of the beams) and "asps" (average speed of sound).
	# ---linear--- is TRUE if x is in linear volume backscatter values (sv) and FALSE if x is in logarithmic volume backscatter values (Sv).
	# ---TVG.exp--- is the exponent of the eamotric spreading of the sound wave, theoretically 2 for Sv and 4 for TS.
	
	
	##################################################
	##################################################
	##### Preparation #####
	# Function for extracting one ping of a matrix of data, og simply the vector if given as one:
	getping<-function(y, p){
		if(length(dim(y))==2){
			y[,p]
			}
		else{
			y
			}
		}
	
	beamsnames = c("lenb", "asps", "sint", "absr", "rres", "plsl", "rofs")
	if(is.list(x) && all(c("vbsc", beamsnames) %in% names(x))){
		beams = x[beamsnames]
		x = x$vbsc
		}
	# Convert to list for convenience:
	if(!is.list(x)){
		x = list(x)
		}
	# Apply the TVG to each element of the list, often representing time steps:
	for(i in seq_along(x)){
		# Select the current time step in the beam configuration data:
		thisbeams = list(lenb=getping(beams$lenb, i), plsl=beams$plsl[1], asps=beams$asps[1], sint=beams$sint[1], absr=getping(beams$absr, i), rofs=beams$rofs[1])
		TVG = get.TVG(thisbeams, x[[i]], linear, TVG.exp, Ro=Ro, thr1m=thr1m)
		
		# Apply logarithm if requested:
		if(linear){
			if(rm){
				x[[i]] = x[[i]] / c(TVG)
				}
			else{
				x[[i]] = x[[i]] * c(TVG)
				}
			}
		else{
			if(rm){
				x[[i]] = x[[i]] - 10*log10(c(TVG))
				}
			else{
				x[[i]] = x[[i]] + 10*log10(c(TVG))
				}
			}
		}
	
	
	##### Execution and output #####
	# Drop the list if it was generated at the top:
	if(length(x)==1){
		x[[1]]
		}
	else{
		x
		}
	##################################################
	##################################################
	}
