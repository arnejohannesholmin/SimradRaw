#*********************************************
#*********************************************
#' Reads a Simrad raw file.
#'
#' @param f  is the path to the raw file.
#' @param t  is a vector of the time steps to read.
#' @param endian is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31). NA
#' @param timeOffset  is the time offset of the datagram.
#' @param minTimeDiff  is the minimum difference in matlab time betwee two pings.
#' @param drop.out  is TRUE to drop dimensions of the data.
#' @param msg  is TRUE to print a time bar during reading.
#' @param prenumt  is the number of time steps to reserve in the temporary file sto which data are saved during reading in order to restrict memeory usage. High values result in fewer temporary file but higher CPU time. The default value of 10 should be relatively optimal.
#' @param na.rm  is TRUE to remove missing pings.
#' @param cleartemp  is TRUE to clear all files in the temporary directory in which files are written in the TSD format for faster reading of the raw files. These files are deleted at startup of the operating system, but can be deleted specifically here if needed.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD interpret.mtim is.TSD NAs read.TSD strff write.TSD zeropad prettyIntegers
#' @importFrom XML xml xmlParse
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom utils tail
#'
#' @export
#' @rdname readEKRawOld
#'
readEKRawOld <- function(f, t=1, endian="little", timeOffset=0, minTimeDiff=Inf, drop.out=FALSE, msg=TRUE, prenumt=10, na.rm=TRUE, cleartemp=FALSE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	# Update: 2015-04-23 - Changed to return pings separately as elements of lists, and filling in NAs to form arrays in each ping, but keeping variable lengths between pings, in order to save space particularly when one ping has long beams and the others have short beams, in which case a lot of NAs will be saved.
	# Update: 2015-10-08 - Added support for reading non-UTF-8 characters.
	# Update: 2015-11-16 - Fixed bug when reading files with long difference in time between beams. Now trying to associate new pings by either that the idx reached numb, or that idx==1.
	# Last: 2016-10-23 - Changed to writing multiple temporary TSD files, speeding up for high number of pings.
	

	##################################################
	##################################################
	##### Preparation #####
	getthispind <- function(pind, prenumt=0){
		if(prenumt==0){
			prenumt <- Inf
		}
		(pind - 1) %% prenumt + 1
	}
	# Funciton used for generating file names of the temporary TSD files to which the acoustic data are written, and read from at the end:
	getTempFileName <- function(tmpdir, f, rawtype=0, find=NULL){
		file.path(tmpdir, paste0("tempRaw", rawtype, "_", basename(file_path_sans_ext(f)), "_file", find, ".tsd"))
	}
	# Function used for filling in missing values to form an array for each ping:
	fillInNA_ping <- function(x, l, drop.out=FALSE){
		out <- TSD::NAs(max(l),length(l))
		out[rep(max(l) * seq(0,length(l)-1), l) + sequence(l)] <- x
		if(drop.out){
			out <- drop(out)
		}
		out
	}
	# Function that merges time steps located in separate elements of lists, given that these have equal lengths for all time steps:
	list2array <- function(x){
		lens <- unlist(lapply(x, length), use.names=FALSE)
		if(all(lens == lens[1])){
			x <- unlist(x, use.names=FALSE)
			dim(x) <- c(lens[1], length(lens))
		}
		x
	}
	# Function used for writing data to the tmporary TSD file:
	readEKRaw_writeTemp <- function(data, tmpdir, f, pind, prenumt, rawtype){
		# Update 'thispind', the ping index in the current file with number 'thisfind', and generate the current file name:
		thispind <- getthispind(pind, prenumt)
		thisfind <- ceiling(pind / prenumt)
		rawfile <- getTempFileName(tmpdir, f, rawtype=rawtype, find=thisfind)
	
		# Get the lengths of each variable in order to save all beams on one time step after collapsing each variable:
		channelLengths <- lapply(data$pings,function(xx) unlist(lapply(xx,length), use.names=FALSE))
		if(length(channelLengths)){
			names(channelLengths) <- paste0("L", TSD::zeropad(seq_along(channelLengths), 3))
			# Unlist the data and give names:
			data$pings <- lapply(data$pings, unlist, use.names=FALSE)
			names(data$pings) <- paste0("V", TSD::zeropad(seq_along(data$pings),3))
			# Write the data in one go:
			TSD::write.TSD(c(data$pings,channelLengths), rawfile, numt=1, append=thispind>1, reserve=prenumt, header=list(dtyp=list(V002="doub")))
		}
		else{
			NULL
		}
	}
	
	# Chech the name of the file and whether it is a TSD file:
	if(tolower(tools::file_ext(f))!="raw"){
		warnings(paste0("The file ",f, " does not have file extension \"raw\""))
	}
	if(TSD::is.TSD(f)){
		stop(paste0("The file ",f, " is a TSD file. Only SIMRAD raw files accepted"))
	}
	
	# Declare the data list:
	data <- list()
	#validDatagrams <- c("CON1","RAW0","RAW1","TAG0","NME0","SVP0")
	#validRawDatagrams <- c("RAW0","RAW1")
	nBytesDgHeader <- 12
	nBytesConfigHeader <- 516
	nBytesTransceiverCount <- 320
	nBytesHeader <- 8
	nNMEA <- 0
	nTAG <- 0
	# Ping index number:
	pind <- 0
	thispind <- 0
	#thisfind <- 0
	pingsnames <- NULL
	fileSize <- file.info(f)$size
	previousTime <- -Inf
	datagramLengths <- NULL
	
	# 't' may be given as "all", indicating Inf (all time steps):
	if(TSD::strff("all",t)){
		t <- Inf
	}
	
	# Very large values of 't' are interpreted as 
	if(any(nchar(t)>4)){
		mtim <- TSD::interpret.mtim(t)
		t <- c(0, Inf)
	}
	else{
		mtim <- c(0, Inf)
	}
	
	# Open the raw file:
	fid <- file(f,"rb")
	
	# Read configuration datagram 'CON0':
	# > > > > > > > > > > #
	# (DG1) Skip bytes:
	len <- readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	# Read the first datagram header:
	# (DG2) Skip bytes:
	dgHeader <- readEKRaw_ReadDgHeader(fid, timeOffset=0, endian=endian)
	if(length(dgHeader)==0){
		warning(paste("Empty file ",f))
		close(fid)
		return(list())
	}
	
	if(dgHeader$dgType == "CON0"){
		# (DG3) Skip bytes:
		config <- readEKRaw_ReadHeader(fid, dgHeader, endian=endian)
		# Skip bytes:
		numb <- config$header$transceivercount
	}
	else{
		warning("No CON0 datagram at the beginning of the file.")
		# (DG3) Skip bytes:
		temp <- readBin(fid, what="raw", len-nBytesDgHeader)
		config <- list()
		numb <- 1
	}
	# (DG4) Skip bytes:
	len <- readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
	# < < < < < < < < < < #
	
	data$config <- config$transceiver
	nBytesRead <- nBytesHeader + nBytesDgHeader + nBytesConfigHeader + if(length(config)) config$header$transceivercount * nBytesTransceiverCount else 0
	
	# Define file names for raw0 and raw1 files to which the correponding datagrams are written in the TSD format, for quick reading at the end of the function:
	options(digits.secs=6)
	fname <- paste0(rev(rev(unlist(strsplit(f, "/", fixed=TRUE)))[c(1,3,4)]), collapse="_")
	tmpdir <- tempdir()
	# Clear all files in the tmpdir:
	if(cleartemp){
		ltemp <- list.files(tmpdir, full.names=TRUE)
		ltemp <- ltemp[tolower(substr(basename(), 1, 5)) == "tempr"]
		unlink(ltemp)
	}
		
	rawtype <- NULL
	completePing <- FALSE
	

	##### Read the file, processing individual datagrams: #####
	totalsteps=file.info(f)$size
	if(msg){
		# Plotting of time bar:
		infostring="Reading the SIMRAD raw file(s):"
		cat(infostring,"\n", sep="")
		stepfact=nchar(infostring)/totalsteps
		oldvalue=0
	}
	
	
	##### Execution and output #####
	clean <- TRUE
	while(nBytesRead<=2*fileSize){
		if(msg){
			# Print a dot if the floor of the new value exceeds the old value:
			thisvalue=floor(seek(fid)*stepfact)
			if(thisvalue > oldvalue){
				cat(rep(".",thisvalue-oldvalue))
				oldvalue=thisvalue
			}
		}
			
		# > > > > > > > > > > #
		len <- readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
		# If the length of the datagram is 0, abort the reading:
		alreadyread <- seek(fid)
		if(sum(len)==0 && alreadyread<totalsteps){
			warning(paste0("File ", f, " was only partially read.", if(msg) paste0(" Number of bytes read: ", alreadyread, " of ", totalsteps, " (",  format(100*alreadyread/totalsteps,digits=1), " percent read).")))
			clean <- FALSE
			break
		}
		# < < < < < < < < < < #
		
		# Read the datagram header:
		# > > > > > > > > > > #
		dgHeader <- readEKRaw_ReadDgHeader(fid, timeOffset, endian=endian)
		# < < < < < < < < < < #
		
		nBytesRead <- nBytesRead + len + 8 # 4 bytes each from reading the 'len' and 'lastlen', in total 8 bytes
		
		# If reading subsets - check if we're done:
		#if(length(dgHeader$dgType)==0 || nchar(dgHeader$dgType)==0 || (length(dgHeader$dgTime) && dgHeader$dgTime > mtim[2])){
		if(length(dgHeader$dgType)==0 || (length(dgHeader$dgTime) && dgHeader$dgTime > mtim[2])){
			# Move file pointer back to beginning of the current datagram:
			#seek(fid, where=-(nBytesDgHeader+4), origin="current")
			break
		}
		
		##### Process datagrams by type: #####
		
		# Process NMEA datagram:
		if(dgHeader$dgType=="NME0"){
			# Process NME0 datagram:
			
			# > > > > > > > > > > #
			###seek(fid, len-nBytesDgHeader, "current")
			text <- readChar(fid, len-nBytesDgHeader)
			# < < < < < < < < < < #
			
			# Assign raw NMEA data:
			nNMEA <- nNMEA + 1
			data$NMEA$time[[nNMEA]] <- dgHeader$dgTime
			data$NMEA$string[[nNMEA]] <- text
		}
		# Process TAG0 datagram:
		else if(dgHeader$dgType=="TAG0"){
			# Process annotation datagram:
			
			# > > > > > > > > > > #
			text <- readChar(fid, len-nBytesDgHeader)
			# < < < < < < < < < < #
			
			# Assign raw TAG0data:
			nTAG <- nTAG + 1;
			data$annotations$time[[nTAG]] <- dgHeader$dgTime
			data$annotations$text[[nTAG]] <- text
		}
		# Process RAW0 datagram:
		else if(dgHeader$dgType=="RAW0"){
			rawtype <- 0
			# Process sample datagram:
			
			# > > > > > > > > > > #
			sampledata <- readEKRaw_ReadSampledata_RAW0(fid)
			# < < < < < < < < < < #
			
			idx <- sampledata$channel
			# Check if we have moved into a new ping, determined by comparing the time of the current RAW0 datagram to the previous registered ping time:
			if(any(completePing, idx==1, 86400 * (dgHeader$dgTime - previousTime) > minTimeDiff)){
				
				### Write the data to the temporary file: ###
				if(pind>0){
					# Write and catch reserve error:
					temp <- readEKRaw_writeTemp(data=data, tmpdir=tmpdir, f=f, pind=pind, prenumt=prenumt, rawtype=rawtype)
					if(is.character(temp) && temp=="Error:reserve"){
						return(temp)
					}
					# Reset the data:
					data$pings <- list()
				}
				
				pind <- pind+1
				previousTime <- dgHeader$dgTime
				
				#  If reading subsets - check if we're done:
				if (pind > max(t)){
					#  Move file pointer back to beginning of this datagram:
					seek(fid, -(len + 4), "current")
					break
				}
				
				# Allocate space for the sample data:
				data$pings$number <- list()
				data$pings$time <- list()
				data$pings$mode <- list()
				data$pings$transducerdepth <- list()
				data$pings$frequency <- list()
				data$pings$transmitpower <- list()
				data$pings$pulselength <- list()
				data$pings$bandwidth <- list()
				data$pings$sampleinterval <- list()
				data$pings$soundvelocity <- list()
				data$pings$absorptioncoefficient <- list()
				data$pings$heave <- list()
				data$pings$roll <- list()
				data$pings$pitch <- list()
				data$pings$temperature <- list()
				data$pings$trawlupperdepthvalid <- list()
				data$pings$trawlopeningvalid <- list()
				data$pings$trawlupperdepth <- list()
				data$pings$trawlopening <- list()
				data$pings$offset <- list()
				data$pings$count <- list()
				# Allocate space for the 'power' and electrical angles:
				if (sampledata$count > 0){
					if(sampledata$mode != 2){
						data$pings$power <- list()
					}
					if(sampledata$mode > 1){
						data$pings$alongship_e <- list()
						data$pings$athwartship_e <- list()
					}
				}
			}
			# Update completePing:
			completePing <- idx==numb
				
			# Check if we're storing this ping:
			if (pind %in% t || (dgHeader$dgTime >= mtim[1] & dgHeader$dgTime <= mtim[2])){
				# Copy this pings data into output arrays:
				data$pings$number[[idx]] <- pind
				data$pings$time[[idx]] <- dgHeader$dgTime
				data$pings$mode[[idx]] <- sampledata$mode
				data$pings$transducerdepth[[idx]] <- sampledata$transducerdepth
				data$pings$frequency[[idx]] <- sampledata$frequency
				data$pings$transmitpower[[idx]] <- sampledata$transmitpower
				data$pings$pulselength[[idx]] <- sampledata$pulselength
				data$pings$bandwidth[[idx]] <- sampledata$bandwidth
				data$pings$sampleinterval[[idx]] <- sampledata$sampleinterval
				data$pings$soundvelocity[[idx]] <- sampledata$soundvelocity
				data$pings$absorptioncoefficient[[idx]] <- sampledata$absorptioncoefficient
				data$pings$heave[[idx]] <- sampledata$heave
				data$pings$roll[[idx]] <- sampledata$roll
				data$pings$pitch[[idx]] <- sampledata$pitch
				data$pings$temperature[[idx]] <- sampledata$temperature
				data$pings$trawlupperdepthvalid[[idx]] <- sampledata$trawlupperdepthvalid
				data$pings$trawlopeningvalid[[idx]] <- sampledata$trawlopeningvalid
				data$pings$trawlupperdepth[[idx]] <- sampledata$trawlupperdepth
				data$pings$trawlopening[[idx]] <- sampledata$trawlopening
				data$pings$offset[[idx]] <- sampledata$offset
				data$pings$count[[idx]] <- sampledata$count
				# Store sample data:
				if (sampledata$count > 0){
					if(sampledata$mode != 2){
						# Store power:
						data$pings$power[[idx]] <- sampledata$power
					}
					if(sampledata$mode > 1){
						# Store angles:
						data$pings$alongship_e[[idx]] <- sampledata$alongship
						data$pings$athwartship_e[[idx]] <- sampledata$athwartship
					}
				}
				if(length(pingsnames)==0){
					pingsnames <- names(data$pings)
				}
			}
		}
		# Process RAW1 datagram:		
		else if(dgHeader$dgType=="RAW1"){
			rawtype <- 1
			# Process sample datagram:
			
			# > > > > > > > > > > #
			sampledata <- readEKRaw_ReadSampledata_RAW1(fid)
			# < < < < < < < < < < #
			
			idx <- sampledata$channel
			# Check if we have moved into a new ping, determined by comparing the time of the current RAW1 datagram to the previous registered ping time:
			if(any(completePing, idx==1, 86400 * (dgHeader$dgTime - previousTime) > minTimeDiff)){
				
				### Write the data to the temporary file: ###
				if(pind>0){
					# Write and catch reserve error:
					temp <- readEKRaw_writeTemp(data=data, tmpdir=tmpdir, f=f, pind=pind, prenumt=prenumt, rawtype=rawtype)
					if(is.character(temp) && temp=="Error:reserve"){
						return(temp)
					}
					# Reset the data:
					data$pings <- list()
				}
				
				pind <- pind+1
				previousTime <- dgHeader$dgTime
				
				#  If reading subsets - check if we're done:
				if (pind > max(t)){
					#  Move file pointer back to beginning of this datagram:
					seek(fid, -(len + 4), "current")
					break
				}
				
				# Allocate space for the sample data:
				data$pings$number <- list()
				data$pings$time <- list()
				data$pings$datatype <- list()
				data$pings$ncomplexpersample <- list()
				data$pings$gaintx <- list()
				data$pings$frequency <- list()
				data$pings$transmitpower <- list()
				data$pings$pulselength <- list()
				data$pings$bandwidth <- list()
				data$pings$sampleinterval <- list()
				data$pings$soundvelocity <- list()
				data$pings$absorptioncoefficient <- list()
				data$pings$heave <- list()
				data$pings$roll <- list()
				data$pings$pitch <- list()
				data$pings$temperature <- list()
				data$pings$heading <- list()
				data$pings$transmitmode <- list()
				data$pings$pulseform <- list()
				data$pings$dirx <- list()
				data$pings$diry <- list()
				data$pings$dirz <- list()
				data$pings$gainrx <- list()
				data$pings$sacorrection <- list()
				data$pings$equivalentbeamangle <- list()
				data$pings$beamwidthalongshiprx <- list()
				data$pings$beamwidthathwartshiprx <- list()
				data$pings$anglesensitivityalongship <- list()
				data$pings$anglesensitivityathwartship <- list()
				data$pings$angleoffsetalongship <- list()
				data$pings$angleoffsetathwartship <- list()
				data$pings$spare <- list()
				data$pings$noisefilter <- list()
				data$pings$beamwidthmode <- list()
				data$pings$beammode <- list()
				data$pings$beamwidthhorizontaltx <- list()
				data$pings$beamwidthverticaltx <- list()
				data$pings$offset <- list()
				data$pings$count <- list()
				# Allocate space for the 'data' and 'power':
				if (sampledata$count > 0){
					data$pings$data <- list()
					data$pings$power <- list()
				}
			}
			# Update completePing:
			completePing <- idx==numb
					
			# Check if we're storing this ping:
			if (pind %in% t || (dgHeader$dgTime >= mtim[1] & dgHeader$dgTime <= mtim[2])){
				# Copy this pings data into output arrays:
				data$pings$number[[idx]] <- pind
				data$pings$time[[idx]] <- dgHeader$dgTime
				data$pings$datatype[[idx]] <- sampledata$datatype
				data$pings$ncomplexpersample[[idx]] <- sampledata$ncomplexpersample
				data$pings$gaintx[[idx]] <- sampledata$gaintx
				data$pings$frequency[[idx]] <- sampledata$frequency
				data$pings$transmitpower[[idx]] <- sampledata$transmitpower
				data$pings$pulselength[[idx]] <- sampledata$pulselength
				data$pings$bandwidth[[idx]] <- sampledata$bandwidth
				data$pings$sampleinterval[[idx]] <- sampledata$sampleinterval
				data$pings$soundvelocity[[idx]] <- sampledata$soundvelocity
				data$pings$absorptioncoefficient[[idx]] <- sampledata$absorptioncoefficient
				data$pings$heave[[idx]] <- sampledata$heave
				data$pings$roll[[idx]] <- sampledata$roll
				data$pings$pitch[[idx]] <- sampledata$pitch
				data$pings$temperature[[idx]] <- sampledata$temperature
				data$pings$heading[[idx]] <- sampledata$heading
				data$pings$transmitmode[[idx]] <- sampledata$transmitmode
				data$pings$pulseform[[idx]] <- sampledata$pulseform
				data$pings$dirx[[idx]] <- sampledata$dirx
				data$pings$diry[[idx]] <- sampledata$diry
				data$pings$dirz[[idx]] <- sampledata$dirz
				data$pings$gainrx[[idx]] <- sampledata$gainrx
				data$pings$sacorrection[[idx]] <- sampledata$sacorrection
				data$pings$equivalentbeamangle[[idx]] <- sampledata$equivalentbeamangle
				data$pings$beamwidthalongshiprx[[idx]] <- sampledata$beamwidthalongshiprx
				data$pings$beamwidthathwartshiprx[[idx]] <- sampledata$beamwidthathwartshiprx
				data$pings$anglesensitivityalongship[[idx]] <- sampledata$anglesensitivityalongship
				data$pings$anglesensitivityathwartship[[idx]] <- sampledata$anglesensitivityathwartship
				data$pings$angleoffsetalongship[[idx]] <- sampledata$angleoffsetalongship
				data$pings$angleoffsetathwartship[[idx]] <- sampledata$angleoffsetathwartship
				data$pings$spare[[idx]] <- sampledata$spare
				data$pings$noisefilter[[idx]] <- sampledata$noisefilter
				data$pings$beamwidthmode[[idx]] <- sampledata$beamwidthmode
				data$pings$beammode[[idx]] <- sampledata$beammode
				data$pings$beamwidthhorizontaltx[[idx]] <- sampledata$beamwidthhorizontaltx
				data$pings$beamwidthverticaltx[[idx]] <- sampledata$beamwidthverticaltx
				data$pings$offset[[idx]] <- sampledata$offset
				data$pings$count[[idx]] <- sampledata$count
				# Store sample data:
				if (sampledata$count > 0){
					data$pings$data[[idx]] <- sampledata$data
					data$pings$power[[idx]] <- sampledata$power
				}
				if(length(pingsnames)==0){
					pingsnames=names(data$pings)
				}
			}
		}
		# Process CON1 datagram:
		else if(dgHeader$dgType=="CON1"){
			# Read ME70 CON datagram
			
			# > > > > > > > > > > #
			#text <- readChar(fid, len-nBytesDgHeader)
			text <- readBin(fid, what="raw", len-nBytesDgHeader)
			text <- rawToChar(text, multiple = FALSE)
			# < < < < < < < < < < #
			
			data[["conf"]]$time <- dgHeader$dgTime
			data[["conf"]]$text <- text
			# At some point this may return a data structure containing the parameters defined in the CON1 datagram but currently only the text contained in the datagram is returned. The CON1 XML string is non-conformant in that the values for each node are stored within the opening tag as "value" instead of between the opening and closing tag. Most XML packages do not parse this correctly. A custom parser needs to be written to return a sane structure containing the CON1 data.  <<Rick Towler>>
		}
		# Process SVP0 datagram:
		else if(dgHeader$dgType=="SVP0"){
			#  Read Sound Velocity Profile datagram
			
			# > > > > > > > > > > #
			#text <- readChar(fid, len-nBytesDgHeader)
			text <- readBin(fid, what="raw", len-nBytesDgHeader)
			text <- rawToChar(text, multiple = FALSE)
			# < < < < < < < < < < #
			
			data$svp$time <- dgHeader$dgTime
			data$svp$text <- text
		}
		# Process XML0 datagram:
		else if(dgHeader$dgType=="XML0"){
			xml <- readBin(fid, what="raw", len-nBytesDgHeader)
			xml <- rawToChar(xml, multiple = FALSE)
			xml <- xmlParse(xml)
		}
		# Skip unknown datagrams:
		#else if(maxBadBytes > 0){
		else{
			seek(fid, len-nBytesDgHeader, origin="current")
			#seek(fid, 1, origin="current")
		}
		
		# Datagram length is repeated:
		# > > > > > > > > > > #
		lastLen <- readBin(fid, what="int", n=1, size=4, endian=endian, signed=TRUE)
		datagramLengths <- rbind(datagramLengths, c(len, lastLen))
		# < < < < < < < < < < #
		
		lastType <- dgHeader$dgType;
	}
	
	# Write the last ping, if raw0 or raw1 datagrams were read:
	if(length(rawtype)){
		# Write and catch reserve error and update 'pind' if 'channelLengths' had length in readEKRaw_writeTemp() (unknown why):
		temp <- readEKRaw_writeTemp(data=data, tmpdir=tmpdir, f=f, pind=pind, prenumt=prenumt, rawtype=rawtype)
		if(length(temp)){
			if(is.character(temp) && temp=="Error:reserve"){
				return(temp)
			}
			pind <- pind+1
		}
	}
	
	close(fid)
	if(msg){
		cat("\n", sep="")
	}
	numt <- pind-1
	
	# Read the temporary files:
	dataRead <- TRUE
	# Get file names of all temporary files:
	thisfind <- ceiling(numt / prenumt)
	rawfiles <- getTempFileName(tmpdir, f, rawtype=rawtype, find=seq_len(thisfind))
	if(length(rawfiles) && all(file.exists(rawfiles)) && unclass(Sys.time()) - unclass(file.info(tail(rawfiles, 1))$mtime) < 100){
		# Read the files:
		data$pings <- TSD::read.TSDs(rawfiles, t="all", header=FALSE, info=FALSE, dimension=FALSE, drop.out=FALSE, clean=FALSE, merge=TRUE, msg=FALSE)
		unlink(rawfiles)
	}
	else{
		dataRead <- FALSE
		warning(paste("Pings data missing in file ",f))
	}
	
	if(numt==0){
		data$pings <- NULL
	}
	else if(dataRead){
		# Fill inn NAs, so that the data are returned with equal lengts along each chanel for each ping. That is, the beam lengths may change between pings but not within pings. Each ping is returned as an array. However, the variable 'lenb' keeps trach of the true lengths of the beams, in the case that there are missing values at the end of some of the beams, as is the case for the MS70 sonar:
		namespings <- names(data$pings)
		vars <- namespings[substr(namespings,1,1)=="V"]
		lens <- namespings[substr(namespings,1,1)=="L"]
		nbeams <- lapply(data$pings[lens], function(xx) sapply(xx, length))
		
		# Scan through the lengths to detect pings with fewer beams than the other beams:
		#differingNrBeams <- lapply(nbeams, function(xx) which(xx!=xx[1]))
		differingNrBeams <- lapply(nbeams, function(xx) which(xx!=config$header$transceivercount))
		#allPingsEqualNrBeams <- sapply(nbeams, function(xx) all(xx)==xx[1])
		# Remove incomplete pings:
		if(length(unlist(differingNrBeams))){
			pingsToRemove <- unique(unlist(differingNrBeams))
			cat("Incomplete pings:", prettyIntegers(pingsToRemove), "\n")
			data$pings <- lapply(data$pings, function(xx) xx[-pingsToRemove])
			numt <- numt - length(pingsToRemove)
		}
		if(!clean){
			data$pings <- lapply(data$pings, function(xx) xx[-length(xx)])
			numt <- numt-1
		}
			
		# Return if all pings were removed:
		if(length(data$pings[[1]])==0){
			return(list(header=config$header, data=data, numt=0, numb=0))
		}
		
		#splitseq <- rep(seq_len(numt), each=length(thislen)/numt)
		# Split first into time steps, and then fill in NAs to create an array for each ping:
		for(i in seq_along(vars)){
			# Split the variable lengths into time steps:
			thislen <- unlist(data$pings[[lens[i]]], use.names=FALSE)
			#thislen <- split(thislen, splitseq)
			if(round(length(thislen)/numt) != length(thislen)/numt){
				warning("Differing number of beams. This leads to an error. This could be due to too low values of minTimeDiff. Try increasing to Inf")
			}
			dim(thislen) <- c(length(thislen)/numt, numt)
			pinglen <- colSums(thislen)
		
			#splitseq <- rep(seq_len(pinglen), pinglen)
			data$pings[[vars[i]]] <- unlist(data$pings[[vars[i]]], use.names=FALSE)
			data$pings[[vars[i]]] <- split(data$pings[[vars[i]]], rep(seq_along(pinglen), pinglen))
			for(p in seq_along(data$pings[[vars[i]]])){
				data$pings[[vars[i]]][[p]] <- fillInNA_ping(data$pings[[vars[i]]][[p]], thislen[,p], drop.out=drop.out)
			}	
			data$pings[[lens[i]]] <- NULL
		}
		names(data$pings) <- pingsnames
	
		# Collapse variables that are not acoustic or electric angle data to arrays with time along the last dimension:
		notVariableBetweenPings <- !names(data$pings) %in% c("data", "power", "alongship_e", "athwartship_e")
		data$pings[notVariableBetweenPings] <- lapply(data$pings[notVariableBetweenPings], list2array)
	
		##### Merge the data into appropriate structures: #####
		data$NMEA$time <- unlist(data$NMEA$time, use.names=FALSE)
		data$NMEA$string <- unlist(data$NMEA$string, use.names=FALSE)
		data$annotations$time <- unlist(data$annotations$time, use.names=FALSE)
		data$annotations$text <- unlist(data$annotations$text, use.names=FALSE)
		
		# Drop the empty dimensions if required:
		if(drop.out){
			data$pings <- lapply(data$pings, drop)
		}
		numb <- max(sapply(data$pings$number, length))
	}
	else{
		numb <- NA
	}
	
	if(na.rm==TRUE){
		areNA <- if(length(dim(data$pings$number))==0) which(is.na(data$pings$number[1])) else which(is.na(data$pings$number[1,]))
		data$pings <- lapply(data$pings, readEKRaw_stripNA, areNA)
	}
	
	# Return the header and data:
	list(header=config$header, data=data, numt=numt, numb=numb, datagramLengths=datagramLengths)
	##################################################
	##################################################
}
