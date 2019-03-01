#*********************************************
#*********************************************
#' Read the header from a Simrad raw file.
#'
#' @param x			The raw vector.
#' @param endian	The endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#' @param ...		Used for robustness.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#'
readEKRaw_GetFileHeader <- function(x, endian="little", ...){
	
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	
	# Read configuration header:
	offset <- 0
	schema <- readEKRaw_GetSchema("ConfigHeader")
	# Extract the ConfigHeader from the raw vector:
	configheader <- convertRaw(x, schema=schema, endian=endian, offset=offset)
	configheader$time <- attr(x, "dgTime")
	# Set the offset used in the following reading of each transceiver:
	offset <- offset + max(schema$end)
	
	# Extract individual transceiver configurations:
	configXcvr <- list()
	if(configheader$transceivercount>10000 || configheader$transceivercount<0){
		warning(paste0("Data not properly read (transceivercount higher than 10000 or negative (", configheader$transceivercount, ") . Try to change endian)"))
		return(list())
	}
	for(i in seq_len(configheader$transceivercount)){
		# Read transceiver configuration:
		schema <- readEKRaw_GetSchema("TransceiverConfig")
		# Extract the TransceiverConfig from the raw vector:
		thisconfigXcvr <- convertRaw(x, schema=schema, endian=endian, offset=offset)
		# Set the offset used in the next reading of each transceiver:
		offset <- offset + max(schema$end)
		
		for(j in seq_along(thisconfigXcvr)){
			configXcvr[[names(thisconfigXcvr )[j]]][[i]] <- thisconfigXcvr[[j]]
		}
	}
	# Collapse the tables into matrices:
	for(i in seq_along(configXcvr)){
		if(length(configXcvr[[i]][[1]])>1){
			configXcvr[[i]] <- matrix(unlist(configXcvr[[i]]), byrow=TRUE, nrow=configheader$transceivercount)
		}
	}
	if(length(configXcvr)>0){
		names(configXcvr) <- names(thisconfigXcvr)
	}
	
	# Return the header and tranceiver configurations:
	list(header=configheader, transceiver=configXcvr)
}
