#*********************************************
#*********************************************
#' Splits Simrad raw files by beams.
#'
#' @param x The path to a Simrad raw file or a directory of Simrad raw files.
#' @param t The time steps to extract the data for (use the default t="all" to keep all time steps)
#' @param beamnr A vector or list of beam indices. If empty (default) split into one file for each beam. If several beams should be in one file, use a list such as list(1, 2:4) for a four beam file, resulting in one file for the first beam and one file for the last three beams.
#' @param write Logical: If TRUE, write the splitted data into separate files (in separate directories as default).
#' @param newdir The path to the directory in which to put the splitted files, defaulted to one directory per beam in the same directory as the original raw files.
#' @param nameadd A string to be appended to the file names of the splitted files, defaulted to "Beams_1", "Beams_2,3,4" if splitting a four beam file into the first beam and the last three beams.
#' @param msg Logical: If TURE print a time bar for reading and writing of each individual raw file.
#' @param msgbar Logical: If TURE print a time bar showing the progression of splitting multiple files.
#' @param ext The file extension of the raw files.
#' @param ... Further arguments passed on to list.files(), such as 'recursive'.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#' @rdname readEKRaw_split
#' 
readEKRaw_split <- function(x, t="all", beamnr=NULL, write=TRUE, newdir=NULL, nameadd=NULL, msg=FALSE, msgbar=TRUE, ext="raw", ...){
	# Function used for getting new file names, given 'newdir', 'nameadd' and the beamnumber(s). Returns only one file name!!!:
	newFileName <- function(x, beamnr, nameadd, newdir){
		# Get the new directory and new names of the files:
		if(length(nameadd)==0){
			nameadd <- paste0("Beams_", paste(beamnr, collapse=","))
		}
		if(length(newdir)==0){
			newdir <- file.path(dirname(x[1]), nameadd)
		}
		suppressWarnings(dir.create(newdir))
		newfilename <- basename(x)
		newfilename <- paste0(substr(newfilename, 1, nchar(newfilename)-nchar(ext)-1), "_", nameadd, ".", ext)
		file.path(newdir, newfilename)
	}
	
	# Get the paths to the raw files if given as a directory:
	if(isTRUE(file.info(x)$isdir)){
		x <- list.files(x, full.names=TRUE, paste0("^.*[:.:](", ext, ")$"), ...)
	}
	
	if(msgbar){
		infostring = "Extracting beams from Simrad raw files:"
		cat(infostring,"\n",sep="")
		totalsteps = length(x)
		stepfact = nchar(infostring)/totalsteps
		oldvalue = 0
	}
	for(i in seq_along(x)){
		if(msgbar){
			thisvalue = floor(i*stepfact)
			if(thisvalue > oldvalue){
				cat(rep(".",thisvalue-oldvalue),if(i == totalsteps) "\n", sep="")
				oldvalue = thisvalue
				}
			}
		
		# Read the raw file:
		data <- readEKRaw(x[i], t=t, msg=msg)
		data <- readEKRaw_extractBeams(data, beamnr=beamnr)
		thisnewdir <- rep(newdir, length.out=length(data$beamnr))
		
		#data <- readEKRaw_extractBeams(data, )
		if(write){
			for(j in seq_along(data$beamnr)){
				# The file name is set based on the available beams in the file:
				f <- newFileName(x[i], data$beamnr[[j]], nameadd, thisnewdir[j])
				writeEKRaw(data$data[[j]], con=f, msg=msg)
			}
		}
	}
	# Return the last data:
	#invisible(data)
	# Return the last file name:
	invisible(f)
}
