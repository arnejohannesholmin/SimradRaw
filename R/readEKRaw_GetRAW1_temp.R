#*********************************************
#*********************************************
#' Reads the sample data from a Simrad raw1 file.
#'
#' @param x  is the path to the raw file.
#' @param endian  is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#'
readEKRaw_GetRAW1 <- function(x, endian="little", complex.out=FALSE, ...){
	readEKRaw_GetRAW(x, dgName="RAW1", endian=endian, complex.out=complex.out, ...)
}
