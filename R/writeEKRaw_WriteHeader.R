#*********************************************
#*********************************************
#' Writes the header to a Simrad raw file.
#'
#' @param fid  is the path to the raw file.
#' @param config  is a list of the transceiver configuration.
#' @param dgLen  is the length of the datagram in bytes.
#' @param dgType  is the datagram type.
#' @param dgTime  is the time stamp of the datagram.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname writeEKRaw_WriteHeader
#'
writeEKRaw_WriteHeader <- function(fid, config, dgLen, dgType, dgTime, endian="little"){
	
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	
	# Write the datagram length:
	writeBin(as.integer(dgLen), con=fid, size=4, endian=endian)
	
	# Write datagram header:
	writeEKRaw_WriteDgHeader(fid, dgType, dgTime, endian=endian)
	
	# Write configuration header:
	writeEKRaw_WriteConfigHeader(fid, config$header, endian=endian)
	
	# Write individual xcvr configurations:
	for(i in seq_len(config$header$transceivercount)){
		writeEKRaw_WriteTransceiverConfig(fid, config$transceiver, i, endian=endian)
		}
	
	# Write the datagram length again:
	writeBin(as.integer(dgLen), con=fid, size=4, endian=endian)
}
