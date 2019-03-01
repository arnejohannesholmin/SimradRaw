#*********************************************
#*********************************************
#' (Internal) Strips the input from NAs, used to remove missing pings in EKRaw2TSD_oneFile().
#'
#' @param x a matrix or array of acoustic data.
#' @param NAind indices of the NAs.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_stripNA
#' 
readEKRaw_stripNA <- function(x, NAind=NULL){
	if(length(NAind)){
		if(is.logical(NAind)){
			NAind <- which(NAind)
		}
		if(length(dim(x))==0){
			x[-NAind]
			}
		else if(length(dim(x))==2){
			x[,-NAind]
			}
		else if(length(dim(x))==3){
			x[,,-NAind]
			}
		}
	else{
		x
		}
	}
