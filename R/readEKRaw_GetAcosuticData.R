#*********************************************
#*********************************************
#' Gets acosutic (or electrical) data from a raw vector read from a raw file.
#'
#' @param x				is the raw vector read from the raw file.
#' @param sampledata	is the list read so far using e.g., \code{\link{readEKRaw_GetRAW1}}.
#' @param dgName		is the datagram name.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#'
readEKRaw_GetAcosuticData <- function(x, sampledata, pos, dgName=c("RAW0", "RAW1"), endian="little", complex.out=FALSE){
	if(dgName[1] == "RAW0"){
		if(sampledata$count > 0){
			
			if(sampledata$mode != 2){
				thissize <- 2
				len <- thissize * sampledata$count
				thisind <- pos + seq_len(len)
	
				power0 <- readBin(con=x[thisind], what="int", n=len, size=thissize, endian=endian, signed=TRUE)
				# Power * 10 * log10(2) / 256
				sampledata$power <- (power0 * 0.011758984205624)
			}
			if(sampledata$mode > 1){
				thissize <- 1
				nangles <- 2
				len <- nangles * thissize * sampledata$count
				thisind <- pos + seq_len(len)
	
				angle <- readBin(con=x[thisind], what="int", n=len, size=thissize, endian=endian, signed=TRUE)
				angle <- matrix(angle, nrow=sampledata$count, ncol=2, byrow=TRUE)
				sampledata$athwartship <- angle[,1]
				sampledata$alongship <- angle[,2]
			}
		}
	}
	else if(dgName[1] == "RAW1"){
		# Read acoustic data (if the fourth bit is 1):
		bitpos <- which(as.integer(intToBits(sampledata$datatype))==1L)
		if(identical(bitpos, 4L)){
			thissize <- 4
			len <- thissize * 2 * sampledata$ncomplexpersample * sampledata$count
			thisind <- pos + seq_len(len)
	
			powerComplex <- readBin(con=x[thisind], what="double", n=len, size=thissize, endian=endian, signed=TRUE)
			dim(powerComplex) <- c(2, sampledata$ncomplexpersample, sampledata$count)
			# Save as complex type:
			powerComplex <- complex(real=powerComplex[1,,], imaginary=powerComplex[2,,])
			dim(powerComplex) <- c(sampledata$ncomplexpersample, sampledata$count)
	
			if(complex.out){
				sampledata$data <- powerComplex
			}
			# Sum over the number of complex values per sample, and take the square of the abs, which is the same as the sum of the real part squared and the imaginary part squared:
			sampledata$power <- 10*log10(abs(colSums(powerComplex))^2)
		}
	}
	
	sampledata
}
