#*********************************************
#*********************************************
#' Shifts sonar data by an empirical range offset.
#'
#' @param x  The acoustic data to be shifted by the offset
#' @param beams  A list containing the fields rofs (range offset in meters) and one of rres (range resolution) og the pair asps (average speed of sound) and sint (sampling interval).
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
