#*********************************************
#*********************************************
#' Shifts sonar data by an empirical range offset.
#'
#' @param x  A list containing the fields rofs (range offset in meters) and one of rres (range resolution) og the pair asps (average speed of sound) and sint (sampling interval).
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname getRangeOffsetInUnitsOfSamples
#' 
getRangeOffsetInUnitsOfSamples <- function(beams){
	round(beams$rofs / soundbeam_range(beams, pos=c("res"))[1])
}
