#*********************************************
#*********************************************
#' Extracts one or more beams from raw data read by readEKRaw().
#' 
#' Used in \code{\link\{readEKRaw_split}}.
#'
#' @param x			A list of data read by readEKRaw().
#' @param beamnr	A vector of indices of the beams to extract.
#' @param drop		Logical: if TRUE drop the output when only one beam is extracted.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' 
readEKRaw_extractBeams <- function(x, beamnr=NULL, drop=FALSE){
	# Remove variables with missing names (left overs from the reading function, but should be removed in the future):
	x$data$config <- x$data$config[!is.na(names(x$data$config))]
	x$data$pings <- x$data$pings[!is.na(names(x$data$pings))]
	
	# 'beamnr' can be NULL, a list of vectors, or a vector that will be converted to a list:
	nbeams <- length(x$data$config[[1]])
	if(length(beamnr)==0){
		beamnr <- as.list(seq_len(nbeams))
	}
	else{
		if(!is.list(beamnr)){
			beamnr <- as.list(beamnr)
			}
		beamnr <- lapply(beamnr, intersect, seq_len(nbeams))
		}
	
	out = vector("list", length(beamnr))
	for(i in seq_along(out)){
		out[[i]] <- x
		out[[i]]$header$transceivercount <- length(beamnr[[i]])
		# Extract the beams from the config:
		out[[i]]$data$config <- lapply(out[[i]]$data$config, function(xx) if(length(dim(xx))==2) xx[beamnr[[i]], , drop=FALSE] else xx[beamnr[[i]]])
		# Extract the beams from the pings:
		out[[i]]$data$pings <- lapply(out[[i]]$data$pings, function(xx) if(is.list(xx)) lapply(xx, function(xxx) xxx[, beamnr[[i]], drop=FALSE]) else xx[beamnr[[i]], , drop=FALSE])
		}
	if(drop && length(out)==1){
		out <- out[[1]]
		beamnr <- beamnr[[1]]
	}
	invisible(list(data=out, beamnr=beamnr))
}
