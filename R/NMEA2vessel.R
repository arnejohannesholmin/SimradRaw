#*********************************************
#*********************************************
#' Return vessel times, positions, headings and speeds located in an NMEA atring.
#'
#' @param x  is an NMEA string.
#' @param cleanNMEA  is FALSE to return the data as read, without shaving off incomplete time steps, 1 to remove incomplete and duplicate time steps, and 2 to additionally clean missing info at the end.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD ftim2mtim NAs
#' @importFrom utils tail head
#' @importFrom stats approx
#'
#' @export
#' @rdname NMEA2vessel
#'
NMEA2vessel<-function(x, cleanNMEA=1, interpolate=TRUE){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2015-11-24 - Changed to support inpartial data using cleanNMEA=FALSE.
	########### DESCRIPTION: ###########
	# Return vessel times, positions, headings and speeds located in an NMEA atring.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---x--- is an NMEA string.
	# ---cleanNMEA--- is FALSE to return the data as read, without shaving off incomplete time steps, 1 to remove incomplete and duplicate time steps, and 2 to additionally clean missing info at the end.
	
	
	##################################################
	##################################################
	##### Preparation #####
	if(is.list(x)){
		x = unlist(x)
		}
	if(!is.character(x)){
		return(list())
		}
	
	rawvessel = list()
	# Get the vessel information from the NMEA strings:
	atZDA = setdiff( grep("ZDA",x), grep("K,D",x) ) # Time. Take the possible error of VTG mixing into the string into account.
	atGGA = grep("GGA",x) # Latitude, longitude
	#atGLL = grep("GLL",x)
	atHDT = grep("HDT",x) # Heading
	atVTG = grep("VTG",x) # Speed
	atVLW = grep("VLW",x) # Sailed distance
	
	# Merge to a list for convenience:
	atAll = list(atZDA=atZDA, atGGA=atGGA, atHDT=atHDT, atVTG=atVTG, atVLW=atVLW)
	
	# Clean the data to return only one value per time step:
	if(cleanNMEA){
		# Remove empty elements:
		atAll = atAll[sapply(atAll,length)>0]
		
		# Shave of incomplete time steps before the first time information:
		first = head(atAll$atZDA, 1)
		atAll[-1] = lapply(atAll[-1], function(xx) xx[xx>=first])
		# Remove empty elements:
		atAll = atAll[sapply(atAll,length)>0]
		
		# Shave of incomplete time steps after the last time information:
		last = tail(atAll$atZDA, 1)
		if(!all(sapply(atAll[-1], tail, 1) > last)){
			atAll = lapply(atAll, function(xx) xx[xx<last])
			}
		
		# Remove instances of multiple time steps and add NAs for missing time steps:
		rmDupAddNA <- function(x,y){
			notdup = !duplicated(findInterval(x,y))
			x = x[notdup]
			if(length(x)<length(y)){
				temp = NAs(length(y))
				temp[findInterval(x,y)] = x
				x = temp
				}
			x
			}
		atAll[-1] = lapply(atAll[-1], rmDupAddNA, atAll$atZDA)
		
		# Remove NAs, indicating missing information between time steps:
		valid = !logical(length(max(unlist(lapply(atAll,length)))))
		if(length(atAll$atGGA)){
			valid = valid & !is.na(atAll$atGGA)
			}
		if(length(atAll$atHDT)){
			valid = valid & !is.na(atAll$atHDT)
			}
		if(length(atAll$atVTG)){
			valid = valid & !is.na(atAll$atVTG)
			}
		atAll = lapply(atAll, "[", valid)
		if(length(atAll$atZDA)==0){
			warning("Time information missing in the NMEA string. Try using cleanNMEA=FALSE")
			}
		}
	
	
	# Split into parts separated by commas:
	parts = strsplit(x,",")
	
	
	##### Execution #####
	# Get the time:
	if(length(atAll$atZDA)>0){
		hhmmss = unlist(lapply(parts[atAll$atZDA],"[",2))
		dd = unlist(lapply(parts[atAll$atZDA],"[",3))
		mm = unlist(lapply(parts[atAll$atZDA],"[",4))
		yy = unlist(lapply(parts[atAll$atZDA],"[",5))
		# If any time information is missing or nun-numeric for a time step, simply remove that time step (happens seldomly):
		nas = is.na(cbind(yy,mm,dd,hhmmss,as.numeric(yy),as.numeric(mm),as.numeric(dd),as.numeric(hhmmss)))
		if(any(nas)){
		#if(any(is.na(c(yy,mm,dd,hhmmss,))) || ){
			valid = rowSums(nas)==0
			#yy = yy[-NArows]
			#mm = mm[-NArows]
			#dd = dd[-NArows]
			#hhmmss = hhmmss[-NArows]
			}
		else{
			valid = !logical(length(hhmmss))
			}
		rawvessel$imtm = ftim2mtim(paste0(yy,mm,dd,hhmmss))
		}
	else{
		rawvessel$imtm = NULL
		}	
	# Get the positions if present:
	if(length(atAll$atGGA)>0){
		suppressWarnings(lat <- as.numeric(sapply(parts[atAll$atGGA],"[",3))/100)
		NS = sapply(parts[atAll$atGGA],"[",4)
		suppressWarnings(lon <- as.numeric(sapply(parts[atAll$atGGA],"[",5))/100)
		EW = sapply(parts[atAll$atGGA],"[",6)
		if(any(NS == "S", na.rm=TRUE)){
			lat = -lat
			}
		if(any(EW == "W", na.rm=TRUE)){
			lon = -lon
			}
		# Converting from cardinalsystem to numerical system:
		rawvessel$iltv = floor(lat) + (lat%%1)*10/6
		rawvessel$ilnv = floor(lon) + (lon%%1)*10/6
		# Insert NAs for the strings with less than 10 parts:
		lessThan10 = which(unlist(lapply(parts[atAll$atGGA], length))<10)
		rawvessel$iltv[lessThan10] = NA
		rawvessel$ilnv[lessThan10] = NA
		}
	# Get the heading if present:
	if(length(atAll$atHDT)>0){
		# This is defined as angles clockwise from North. We change to counter clockwise, which is more logical:
		suppressWarnings(rawvessel$irzv <- -as.numeric(sapply(parts[atAll$atHDT],"[",2)))
		}
	# Get the speed if present:
	if(length(atAll$atVTG)>0){
		suppressWarnings(rawvessel$iisv <- sapply(parts[atAll$atVTG],"[",6))
		# Insert NAs for speed information that is longer than 6 characters:
		rawvessel$iisv[nchar(rawvessel$iisv)>6] = NA
		suppressWarnings(rawvessel$iisv <- as.numeric(rawvessel$iisv))
		}
	# Get the vessel log if present:
	if(length(atAll$atVLW)>0){
		rawvessel$isdv <- sapply(parts[atAll$atVLW],"[",2)
		if(is.list(rawvessel$isdv)){
			empty = sapply(rawvessel$isdv, length)==0
			rawvessel$isdv[empty] = as.list(NAs(sum(empty)))
		}
		suppressWarnings(rawvessel$isdv <- as.numeric(rawvessel$isdv))
		}
	
	if(length(rawvessel$imtm)>0){
		# Order the raw vessel data by the imtm:
		rawvessel = lapply(rawvessel, "[", order(rawvessel$imtm))
		
		# Remove incomplete and duplicate time steps, but only if two or more non-NA times are present:
		if(cleanNMEA>1 || sum(!is.na(rawvessel$imtm))<=1){
			notIsNA = rowSums(as.data.frame(lapply(rawvessel, is.na)))==0
			rawvessel = lapply(rawvessel, "[", notIsNA)
			}
		else if(interpolate && length(rawvessel[names(rawvessel)!="imtm"])){
			rawvessel[names(rawvessel)!="imtm"] = lapply(rawvessel[names(rawvessel)!="imtm"], function(xx) approx(x=rawvessel$imtm, y=xx, xout=rawvessel$imtm, rule=2)$y)
			}
		}
	
		
	##### Output #####
	rawvessel
	##################################################
	##################################################
	}
