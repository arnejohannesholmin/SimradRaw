#*********************************************
#*********************************************
#' Shifts sonar data by an empirical range offset.
#'
#' @param x  The acoustic data to be shifted by the offset
#' @param rofs  The empirical range offset.
#' @param rres.exp  The radial resolution of the sonar (c * dt / 2, where c is speed of sound and dt is sampling interval duration)
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname offsetSamples
#' 
offsetSamples <- function(x, beams){
	offsetFirstDimension <- function(y, offset){
		d <- dim(x)
		l <- length(d)
		if(l==2){
			nvalid <- d[1] - offset
			x[seq_len(nvalid),] <- x[offset + seq_len(nvalid),]
			x[nvalid + seq_len(offset),] <- NA
		}
		else if(l==3){
			nvalid <- d[1] - offset
			x[seq_len(nvalid),,] <- x[offset + seq_len(nvalid),,]
			x[nvalid + seq_len(offset),,] <- NA
		}
		else{
			warning("The function offsetFirstDimension() only treates two and three-dimensional arrays")
		}
		x
	}
	
	# Get the rounded offset:
	offset <- round(beams$rofs[1] / beams$rres[1])
	
	# Return:
	if(is.list(x)){
		x <- lapply(x, offsetFirstDimension, offset=offset)
	}
	else{
		x <- offsetFirstDimension(x, offset=offset)
	}
	return(x)
}
