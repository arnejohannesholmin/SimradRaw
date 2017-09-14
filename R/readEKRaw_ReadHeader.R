#*********************************************
#*********************************************
#' Read the header from a Simrad raw file.
#'
#' @param fid  is the path to the raw file.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_ReadHeader
#'
readEKRaw_ReadHeader<-function(fid, dgHeader, endian="little"){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Read the header from a Simrad raw file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---fid--- is the path to the raw file.
	# ---endian--- is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
	

	##################################################
	##################################################
	# Read configuration header:
	configheader = readEKRaw_ReadConfigHeader(fid, endian=endian)
	configheader$time = dgHeader$dgTime
	
	# Extract individual xcvr configurations:
	configXcvr=list()
	if(configheader$transceivercount>10000 || configheader$transceivercount<0){
		warning(paste0("Data not properly read (transceivercount higher than 10000 or negative (", configheader$transceivercount, ") . Try to change endian)"))
		return(list())
		}
	for(i in seq_len(configheader$transceivercount)){
		thisconfigXcvr = readEKRaw_ReadTransceiverConfig(fid, endian=endian)
		for(j in seq_along(thisconfigXcvr)){
			configXcvr[[names(thisconfigXcvr )[j]]][[i]] = thisconfigXcvr[[j]]
			}
		}
	# Collapse the tables into matrices:
	for(i in seq_along(configXcvr)){
		if(length(configXcvr[[i]][[1]])>1){
			configXcvr[[i]] = matrix(unlist(configXcvr[[i]]),byrow=TRUE,nrow=configheader$transceivercount)
			}
		}
	if(length(configXcvr)>0){
		names(configXcvr)=names(thisconfigXcvr)
		}
	
	# Return the header and tranceiver configurations:
	list(header=configheader, transceiver=configXcvr)
	##################################################
	##################################################
	}
