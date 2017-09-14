#*********************************************
#*********************************************
#' Get integer base of the input. Based on code by Lars Nonboe Andersen, Simrad.
#'
#' @param x  is the value of sampledata$datatype.
#' @param b  is the base.
#' @param endian' is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31). NA
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname integer.base.b
#'
integer.base.b <- function(x, b=2, endian="little"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Get integer base of the input. Based on code by Lars Nonboe Andersen, Simrad.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is the value of sampledata$datatype.
	# ---b--- is the base.
	# ---endian' is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	

	##################################################
	##################################################
	ndigits <- (floor(logb(max(x), base=2))+1) 
	Base.b <- array(NA, dim=c(length(x), ndigits))
	for(i in 1:ndigits){
		Base.b[, ndigits-i+1] <- (x %% b)
		x <- (x %/% b)
		}
	if(substr(endian,1,1)=="l"){
		Base.b[,seq(ndigits,1),drop=FALSE]
		}
	else{
		Base.b
		}
	##################################################
	##################################################
	}

