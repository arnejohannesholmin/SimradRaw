#*********************************************
#*********************************************
#' Calculate the Time Varied Gain function.
#'
#' @param beams  is the list of beams inputs including "lenb" (lengths of beams), "sint" (pulse length), "absr" (absorption coefficent of the beams) and "asps" (average speed of sound).
#' @param x  is the input acoustic data arranged in an array having the radial part along the first dimension, the beams along the second or second and third, and time steps and other optional dimensions along the last dimension (lenb,numb,numt).
#' @param linear  is TRUE if x is in linear volume backscatter values (sv) and FALSE if x is in logarithmic volume backscatter values (Sv).
#' @param TVG.exp  is the exponent of the eamotric spreading of the sound wave, theoretically 2 for Sv and 4 for TS.
#' @param Ro  is used in Gavins work, shifting the first voxel to be imaginarily placed inside of the sonar.
#' @param thr1m Logical: If TRUE apply a rule that TVG closer than 1 m should not cause increasedd level (used in LSSS?))
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname get.TVG
#'
get.TVG<-function(beams, x=NULL, linear=TRUE, TVG.exp=2, Ro=NULL, thr1m=FALSE){
	
	############### LOG: ###############
	# Start: 2014-04-02 - Clean version.
	
	##### Preparation #####
	if(is.list(x)){
		if(length(x$vbsc)>0){
			x <- x$vbsc
			linear <- TRUE
		}
		else if(length(x$mvbs)>0){
			x <- x$mvbs
			linear <- FALSE
		}
	}
	if(length(beams$lenb)==0){
		beams$lenb <- dim(x)[1]
	}
	else{
		beams$lenb <- max(beams$lenb, na.rm=TRUE)
	}
	if(length(beams$plsl)==0){
		beams$plsl <- 0
	}
	# Get the ranges to the voxels:	
	if(length(beams$rres)==0){
		beams$rres <- soundbeam_range(beams, pos="res")
	}
	r <- soundbeam_range(beams, pos="mid", Ro=Ro)
	
	
	##### Execution and output #####
	# Apply the exponent if given in 'TVG.exp', defaulted to 20 log r, suitable for Sv:
	TVG <- pmax(1, r^TVG.exp * 10^( outer(r, 2 * beams$absr / 10)))
	TVG <- r^TVG.exp * 10^( outer(r, 2 * beams$absr / 10))
	# Apply a rule that TVG closer than 1 m should not cause increasedd level (used in LSSS?)):
	if(thr1m){
		TVG[r<1] <- TVG[min(which(r>=1))]
	}
	# Apply logarithm if requested:
	if(linear){
		TVG
	}
	else{
		10 * log10(TVG)
	}
}
