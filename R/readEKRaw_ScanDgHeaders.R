#*********************************************
#*********************************************
#' Read all datagram headers of a Simrad raw file.
#'
#' Provides the facility to modify parts of EK80/WBAT .raw files so that these files can be read by the Large Scale Survey System (LSSS) software.
#'
#' @param filename The path to directory of raw files or a vector of the paths to the raw files.
#' @param addTransducerSeralNumber A string giving the transducer serial number.
#' @param endian the endianness of the file, defaulted to "little".
#' @param msg logical: if TRUE print a time bar during file reading.
#'
#' @return A data frame of datagram info.
#'
#' @importFrom TSD is.TSD
#' @importFrom tools file_ext
#' @importFrom data.table rbindlist
#'
#' @export
#' @rdname readEKRaw_ScanDgHeaders
#' 
readEKRaw_ScanDgHeaders <- function(raw, endian="little", msg=TRUE, raw.out=FALSE){
	
	#  Read the entire file if not previously read:
	if(is.character(raw) && file.exists(raw)){
		fid <- file(raw, "rb")
		on.exit(close(fid))
		raw <- readBin(fid, "raw", file.info(raw)$size)
	}
	
	nbytesDgLen <- 4
	nBytesDgHeader <- 12
	totalsteps <- length(raw)
	
	
	# Define the output 'out', the datagram indices 'i', and the position in the raw vector 'at':
	out <- list()
	at <- 0
	i <- 0
	
	while(at < totalsteps){
		# Increment datagram index:
		i <- i + 1
		# Get the datagram length in bytes:
		dgLen <- readBin(raw[at + seq_len(nbytesDgLen)], what="int", n=1, size=nbytesDgLen, endian=endian, signed=TRUE)
		if(dgLen == 0){
			warning("The datagram at position ", at, " is empty.")
		}
	
		# If the length of the datagram is 0, abort the reading:
		if(sum(dgLen)==0 && at < totalsteps){
			break
		}
		
		# Read the datagram header:
		dgHeader <- readEKRaw_GetDgHeader(raw[at + nbytesDgLen + seq_len(nBytesDgHeader)], timeOffset=0, endian=endian)
		at <- at + dgLen + 8 # 4 bytes each from reading the 'dgLen' at the start and end of the datagram, in total 8 bytes
		
		# Add the number of bytes used for the datagram length before and anfter the datagram:
		Nbytes <- dgLen + 2 * nbytesDgLen
		out[[i]] <- c(list(Nbytes=Nbytes, dgLen=dgLen), dgHeader)
	}
	
	if(msg){
		cat("\n", sep="")
	}
	
	# Return the header and data:
	dg <- as.data.frame(data.table::rbindlist(out), stringsAsFactors=FALSE)
	colnames(dg) <- c("Nbytes", "dgLen", "dgName", "dgTime")
	
	# Warning if there is no CON0 datagram:
	if(!dg$dgName[1] %in% c("CON0", "XML0")){
		warning("No CON0 or XML0 datagram at the beginning of the file.")
	}
	
	# Output also the raw vector for each datagram, where the leading and trailing length info (4 bytes on each side) is removed, as well as the 12 bytes of the datagram header:
	dg$starts <- cumsum(dg$Nbytes)
	dg$ends <- dg$starts - nbytesDgLen
	dg$starts <- c(0, dg$starts[-length(dg$starts)]) + nbytesDgLen + nBytesDgHeader + 1
	
	if(raw.out){
		rawsplit <- readEKRaw_SplitRawVector(list(raw=raw, dg=dg))$raw
		list(dg=dg, raw=raw, rawsplit=rawsplit)
	}
	else{
		list(dg=dg, raw=raw)
	}
}

readEKRaw_SplitRawVector <- function(x, dg=NULL, ind=NULL){
	if(length(ind) == 0){
		ind <- seq_along(x$dg$starts)
	}
	x$dg <- x$dg[ind, ]
	# Run thorugh the rows of the datagram info matrix x$dg, and extract the datagrams as raw vectors:
	vec <- seq_len(nrow(x$dg))
	x$raw <- lapply(vec, function(i) x$raw[seq(x$dg$starts[i], x$dg$ends[i])])
	names(x$raw) <- x$dg$dgName
	x
}
