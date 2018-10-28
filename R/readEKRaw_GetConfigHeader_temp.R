#*********************************************
#*********************************************
#' Reads the configuration header from a Simrad raw0 file.
#'
#' @param x  is a raw vector read from a Simrad raw file.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#'
readEKRaw_GetConfigHeader <- function(x, endian="little"){
	# Get the raw datagram schema and convert the raw vector:
	schema <- readEKRaw_GetSchema("ConfigHeader")
	sampledata <- convertRaw(x, schema=schema, endian=endian)
}
