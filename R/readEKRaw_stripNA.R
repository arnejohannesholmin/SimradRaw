#*********************************************
#*********************************************
#' (Internal) Strips the input from NAs, used in EKRaw2TSD_oneFile().
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
