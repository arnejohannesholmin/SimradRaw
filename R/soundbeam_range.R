#*********************************************
#*********************************************
#' This function calculates various variables related to the voxels of an aoustic system, such as the mid points or edge points of the voxels. NOTE: This function only considers one time step, which is a weekness, since the radial resolution may change between pings, particularly with fishery sonars.
#'
#' @param x  is a list of the variables 'lenb', and 'rres' or 'asps' and 'sint'.
#' @param pos  is "res" to return the radial resolution, "max" to return the maximum range of the "mid" to return midpoints of the voxels and "edge" to return edge points, and numeric to return 'pos' positions between 0 and the maximum range to the edges.
#' @param adds	A list of variables to add to the input list x.
#' @param Ro	The range offset in meters.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom utils tail head
#'
#' @export
#' @rdname soundbeam_range
#'
soundbeam_range <- function(x, pos=c("mid","edge","max","res","grid"), adds=NULL, Ro=NULL){
	
	############### LOG: ###############
	# Start: 2014-04-02 - Clean version.
	
	##### Preparation #####
	if(length(adds)){
		x <- c(adds, x)
	}
	pos3 <- tolower(substr(pos[1],1,3))
	x$lenb <- max(x$lenb, na.rm=TRUE)
	if(length(x$rres)==0){
		# Get range resolution:
		x$rres <- x$asps * x$sint/2
	}
	# To support data.frame, the first element is selected for lenb and rres throughout the function:
	#x$rres <- head(c(x$rres), 1)
	
	
	##### Execution and output #####
	# Get ranges:
	if(pos3=="res" || pos3=="rre"){
		out <- x$rres[1]
	}
	else if(pos3=="max"){
		out <- (x$lenb[1] - 0.5) * x$rres[1]
	}
	else if(pos3=="mid"){
		#r <- c(x$rres/4, seq_len(x$lenb-1) * x$rres)
		if(length(Ro)==0 && length(x$rofs)){
			Ro <- x$rofs[1]
		}
		else{
			Ro <- 0
		}
		# Subtract the offset due to digital signal processing (given in calibration files but defaulted to 3 for raw1 and hard coded for raw0):
		out <- seq(0, x$lenb[1] - 1) * x$rres[1] - Ro
	}
	else if(pos3=="edg"){
		out <- c(0, seq_len(x$lenb[1]) * x$rres[1] - x$rres[1] / 2)
	}
	else if(pos3=="gri"){
		out <- c(seq(0, x$lenb[1]) * x$rres[1])
	}
	# Radial partitioning (as a sequence of distances from r=0 to (lenb-0.5)*dr, where 'lenb' is the maximum length of the beams and -0.5 is to adjust for the fact that the midpoint of the first voxel is at r=0, and we here wish to get the edges of the voxels):
	else if(is.numeric(pos)){
		out <- seq(0, (x$lenb[1] - 0.5) * x$rres[1], length=pos)
	}
	
	return(out)
}
