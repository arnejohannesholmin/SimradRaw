#*********************************************
#*********************************************
#' Reads calibration files in the xml format used by LSSS.
#'
#' @param x	The path to a calibration file in XML format.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom XML xmlParse xmlToList
#'
#' @export
#' @rdname readcalfile
#' 
readcalfile <- function(x){
	indata <- xmlParse(file=x)
	indata <- xmlToList(indata)[[1]]
	
	# Convert to a list:
	if(!is.list(indata)){
		indata <- as.list(indata)
	}
	# Old format used in drafts of the paper :
	if(length(indata$SA)){
		indata$gain <- matrix(as.numeric(unlist(indata$g)),ncol=4, byrow=TRUE)
		colnames(indata$gain) <- names(indata$g[[1]])
		indata$sacr <- matrix(as.numeric(unlist(indata$SA)),ncol=2, byrow=TRUE)
		colnames(indata$sacr) <- names(indata$SA[[1]])
		atRo <- which(tolower(names(indata$.attrs)) == "ro")[1]
		indata$rofs <- as.numeric(indata$.attrs[atRo])
		indata$rdlt <- strsplit(indata$.attrs["delta"], ",", fixed=TRUE)[[1]]
		indata$rdlt <- as.data.frame(strsplit(indata$rdlt, ":", fixed=TRUE))
	}
	else{
		indata <- lapply(indata, as.numeric)
		indata$gain <- indata$g
		atRo <- which(tolower(names(indata)) == "ro")[1]
		indata$rofs <- indata[[atRo]]
		#indata$plsf <- indata$tauEff / indata$tau
		#indata$plsl <- indata$tau
		indata$plse <- indata$tauEff
		indata$plsl <- indata$tau
		indata$freq <- indata$frequency
	}
	indata
}
