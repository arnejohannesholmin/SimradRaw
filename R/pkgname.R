#' Read and write Simrad raw files
#'
#' This R package provides functions for reading, writing, splitting and other processing of Simrad raw files (raw0 (echosounder) and raw1 (fishery sonar)). The code is based on the Matlab library written by dr. Rick Towler, NOAA Alaska Fisheries Science Center. The functions use the TSD package for efficient reading of the data (writes to a temporary TSD file and reads afterwards to avoid appending in memory).
#'
#' Details of the package
#' @docType package
#' @name SimradRaw
#'
"_PACKAGE"
