#*********************************************
#*********************************************
#' Reads the sample data from a Simrad raw1 file.
#'
#' @param x  is the path to the raw file.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @export
#'
readEKRaw_GetSchema <- function(dgName=c("RAW0", "RAW1", "ConfigHeader", "TransceiverConfig")){
	
	# Define the type and length of the variables, except the acoustic data, which has length depending on these variables:
	schema_RAW0 <- list(
		list(var="channel",						what="int",    n=1, size=2), 
		list(var="mode_low",					what="int",    n=1, size=1), 
		list(var="mode_high",					what="int",    n=1, size=1), 
		                                	
		list(var="transducerdepth",				what="double", n=1, size=4), 
		list(var="frequency",					what="double", n=1, size=4), 
		list(var="transmitpower",				what="double", n=1, size=4), 
		list(var="pulselength",					what="double", n=1, size=4), 
		list(var="bandwidth",					what="double", n=1, size=4), 
		list(var="sampleinterval",				what="double", n=1, size=4), 
		list(var="soundvelocity",				what="double", n=1, size=4), 
		list(var="absorptioncoefficient",		what="double", n=1, size=4), 
		list(var="heave", 						what="double", n=1, size=4), 
		list(var="roll", 						what="double", n=1, size=4), 
		list(var="pitch", 						what="double", n=1, size=4), 
		list(var="temperature", 				what="double", n=1, size=4), 
		                                	
		list(var="trawlupperdepthvalid", 		what="int",    n=1, size=2), 
		list(var="trawlopeningvalid", 			what="int",    n=1, size=2), 
		                                	
		list(var="trawlupperdepth",				what="double", n=1, size=4), 
		list(var="trawlopening",				what="double", n=1, size=4), 
		
		list(var="offset",						what="int",    n=1, size=4), 
		list(var="count",						what="int",    n=1, size=4)
	)
	schema_RAW1 <- list(
		list(var="channel",						what="int",    n=1, size=2), 
		list(var="datatype",					what="int",    n=1, size=1), 
		list(var="ncomplexpersample",			what="int",    n=1, size=1), 
		                                	
		list(var="gaintx",						what="double", n=1, size=4), 
		list(var="frequency",					what="double", n=1, size=4), 
		list(var="transmitpower",				what="double", n=1, size=4), 
		list(var="pulselength",					what="double", n=1, size=4), 
		list(var="bandwidth",					what="double", n=1, size=4), 
		list(var="sampleinterval",				what="double", n=1, size=4), 
		list(var="soundvelocity",				what="double", n=1, size=4), 
		list(var="absorptioncoefficient",		what="double", n=1, size=4), 
		list(var="heave", 						what="double", n=1, size=4), 
		list(var="roll", 						what="double", n=1, size=4), 
		list(var="pitch", 						what="double", n=1, size=4), 
		list(var="temperature", 				what="double", n=1, size=4), 
		list(var="heading", 					what="double", n=1, size=4), 
		                                	
		list(var="transmitmode", 				what="int",    n=1, size=2), 
		list(var="pulseform", 					what="int",    n=1, size=2), 
		                                	
		list(var="dirx",						what="double", n=1, size=4), 
		list(var="diry",						what="double", n=1, size=4), 
		list(var="dirz",						what="double", n=1, size=4), 
		list(var="gainrx",						what="double", n=1, size=4), 
		list(var="sacorrection",				what="double", n=1, size=4), 
		list(var="equivalentbeamangle",			what="double", n=1, size=4), 
		list(var="beamwidthalongshiprx",		what="double", n=1, size=4), 
		list(var="beamwidthathwartshiprx",		what="double", n=1, size=4), 
		list(var="anglesensitivityalongship", 	what="double", n=1, size=4), 
		list(var="anglesensitivityathwartship",	what="double", n=1, size=4), 
		list(var="angleoffsetalongship", 		what="double", n=1, size=4), 
		list(var="angleoffsetathwartship", 		what="double", n=1, size=4), 
		
		list(var="spare",						what="char",   n=2, size=1), 
		
		list(var="noisefilter",					what="int",    n=1, size=2), 
		list(var="beamwidthmode",				what="int",    n=1, size=2), 
		list(var="beammode",					what="int",    n=1, size=2), 
		
		list(var="beamwidthhorizontaltx",		what="double", n=1, size=4), 
		list(var="beamwidthverticaltx",			what="double", n=1, size=4), 
		
		list(var="offset",						what="int",    n=1, size=4), 
		list(var="count",						what="int",    n=1, size=4)
	)
	schema_ConfigHeader <- list(
		list(var="surveyname",					what="char",   n=128, size=1), 
		list(var="transectname",				what="char",   n=128, size=1), 
		list(var="soundername",					what="char",   n=128, size=1), 
		list(var="version",						what="char",   n=30,  size=1), 
		
		list(var="multiplexing",				what="int",    n=1,   size=2), 
		list(var="timebias",					what="int",    n=1,   size=4), 
		                                	
		list(var="soundvelocityaverage",		what="double", n=1,   size=4), 
		list(var="soundvelocitytransducer",		what="double", n=1,   size=4), 
		list(var="mruoffsetx",					what="double", n=1,   size=4), 
		list(var="mruoffsety",					what="double", n=1,   size=4), 
		list(var="mruoffsetz",					what="double", n=1,   size=4), 
		list(var="mrualphax",					what="double", n=1,   size=4), 
		list(var="mrualphay",					what="double", n=1,   size=4), 
		list(var="mrualphaz",					what="double", n=1,   size=4), 
		list(var="gpsoffsetx", 					what="double", n=1,   size=4), 
		list(var="gpsoffsety", 					what="double", n=1,   size=4), 
		list(var="gpsoffsetz", 					what="double", n=1,   size=4), 
		                                	
		list(var="spare",						what="char",   n=48,  size=1), 
		
		list(var="transceivercount", 			what="int",    n=1, size=4)
	)
	schema_TransceiverConfig <- list(
		list(var="channelid",					what="char",   n=128, size=1), 
		
		list(var="beamtype",					what="int",    n=1,   size=4), 
		
		list(var="frequency",					what="double", n=1,   size=4), 
		list(var="gain",						what="double", n=1,   size=4), 
		list(var="equivalentbeamangle",			what="double", n=1,   size=4), 
		list(var="beamwidthalongship",			what="double", n=1,   size=4), 
		list(var="beamwidthathwartship",		what="double", n=1,   size=4), 
		list(var="anglesensitivityalongship",	what="double", n=1,   size=4), 
		list(var="anglesensitivityathwartship",	what="double", n=1,   size=4), 
		list(var="anglesoffsetalongship",		what="double", n=1,   size=4), 
		list(var="angleoffsetathwartship",		what="double", n=1,   size=4), 
		list(var="posx",						what="double", n=1,   size=4), 
		list(var="posy",						what="double", n=1,   size=4), 
		list(var="posz",						what="double", n=1,   size=4), 
		list(var="dirx",						what="double", n=1,   size=4), 
		list(var="diry",						what="double", n=1,   size=4), 
		list(var="dirz",						what="double", n=1,   size=4), 

		list(var="pulselengthtable",			what="double", n=5,   size=4), 
		                                	
		list(var="spare2",						what="char",   n=8,   size=1), 
		
		list(var="gaintable",					what="double", n=5,   size=4), 
	
		list(var="spare3",						what="char",   n=8,   size=1), 
		
		list(var="sacorrectiontable",			what="double", n=5,   size=4), 
		
		list(var="spare4",						what="char",   n=52,  size=1)
	)
	schema_FIL1 <- list(
		list(var="stage",						what="int",    n=1,   size=2), 
		
		list(var="spare",						what="char",   n=2,   size=1), 
		list(var="channelID",					what="char",   n=128, size=1), 
		
		list(var="noofcoefficients",			what="int",    n=1,   size=2), 
		list(var="decimationfactor",			what="int",    n=1,   size=2), 
		
		list(var="frequency",					what="double", n=1,   size=4), 
		list(var="gain",						what="double", n=1,   size=4), 
		list(var="equivalentbeamangle",			what="double", n=1,   size=4), 
		list(var="beamwidthalongship",			what="double", n=1,   size=4), 
		list(var="beamwidthathwartship",		what="double", n=1,   size=4), 
		list(var="anglesensitivityalongship",	what="double", n=1,   size=4), 
		list(var="anglesensitivityathwartship",	what="double", n=1,   size=4), 
		list(var="anglesoffsetalongship",		what="double", n=1,   size=4), 
		list(var="angleoffsetathwartship",		what="double", n=1,   size=4), 
		list(var="posx",						what="double", n=1,   size=4), 
		list(var="posy",						what="double", n=1,   size=4), 
		list(var="posz",						what="double", n=1,   size=4), 
		list(var="dirx",						what="double", n=1,   size=4), 
		list(var="diry",						what="double", n=1,   size=4), 
		list(var="dirz",						what="double", n=1,   size=4), 

		list(var="pulselengthtable",			what="double", n=5,   size=4), 
		                                	
		
		list(var="gaintable",					what="double", n=5,   size=4), 
	
		list(var="spare3",						what="char",   n=8,   size=1), 
		
		list(var="sacorrectiontable",			what="double", n=5,   size=4), 
		
		list(var="spare4",						what="char",   n=52,  size=1)
	)
	
	# Get the datagram schema by type:
	schema <- get(paste("schema", dgName[1], sep="_"))
	
	# Rbind into a data frame:
	schema <- data.table::rbindlist(schema)
	
	# Get the positions of the data in the input raw vector:
	schema$len <- schema$size * schema$n
	schema$end <- cumsum(schema$len)
	schema$pos <- c(0, schema$end[-length(schema$end)])
	schema$start <- schema$pos + 1
	
	# Create a funciton that accepts ..., since the parameters of readBin are input to the funciton in readVar():
	schema$fun <- c("readBin", "rawToCharDotDotDot")[as.numeric(schema$what == "char") + 1]
	
	### schema$fun <- c(readBin, function(x, ...) rawToChar(x))[as.numeric(schema$what == "char") + 1]
	
	# Convert to a list:
	schema <- as.list(schema)
	schema
}

readEKRaw_GetSchema <- function(dgName=c("RAW0", "RAW1", "ConfigHeader", "TransceiverConfig")){
	
	# Read power from RAW0 if mode != 2:
	nPowerRAW1 <- function(x){
		x$count * as.numeric(x$mode != 2)
	}
	# Read power from RAW0 if mode == 2:
	nAngleRAW1 <- function(x){
		2 * x$count * as.numeric(x$mode == 2)
	}
	# Read acoustic data from RAW1 if the fourth bit of datatype is 1:
	nIfBitpos4 <- function(x){
		bitpos <- which(as.integer(intToBits(x$datatype))==1L)
		if(identical(bitpos, 4L)){
			2 * x$ncomplexpersample * x$count
		}
		else{
			0
		}
	}
	# Read coefficients of filter:
	nFIL1 <- function(x){
		2 * x$noofcoefficients
	}
	
	# Define the type and length of the variables, except the acoustic data, which has length depending on these variables:
	schema_RAW0 <- list(
		list(var="channel",						what="int",    n=1, size=2), 
		list(var="mode_low",					what="int",    n=1, size=1), 
		list(var="mode_high",					what="int",    n=1, size=1), 
		                                	
		list(var="transducerdepth",				what="double", n=1, size=4), 
		list(var="frequency",					what="double", n=1, size=4), 
		list(var="transmitpower",				what="double", n=1, size=4), 
		list(var="pulselength",					what="double", n=1, size=4), 
		list(var="bandwidth",					what="double", n=1, size=4), 
		list(var="sampleinterval",				what="double", n=1, size=4), 
		list(var="soundvelocity",				what="double", n=1, size=4), 
		list(var="absorptioncoefficient",		what="double", n=1, size=4), 
		list(var="heave", 						what="double", n=1, size=4), 
		list(var="roll", 						what="double", n=1, size=4), 
		list(var="pitch", 						what="double", n=1, size=4), 
		list(var="temperature", 				what="double", n=1, size=4), 
		                                	
		list(var="trawlupperdepthvalid", 		what="int",    n=1, size=2), 
		list(var="trawlopeningvalid", 			what="int",    n=1, size=2), 
		                                	
		list(var="trawlupperdepth",				what="double", n=1, size=4), 
		list(var="trawlopening",				what="double", n=1, size=4), 
		
		list(var="offset",						what="int",    n=1, size=4), 
		list(var="count",						what="int",    n=1, size=4), 
		
		list(var="power0",						what="int",    n=nPowerRAW1, size=2), 
		list(var="angle",						what="int",    n=nAngleRAW1, size=1)
	)
	schema_RAW1 <- list(
		list(var="channel",						what="int",    n=1, size=2), 
		list(var="datatype",					what="int",    n=1, size=1), 
		list(var="ncomplexpersample",			what="int",    n=1, size=1), 
		                                	
		list(var="gaintx",						what="double", n=1, size=4), 
		list(var="frequency",					what="double", n=1, size=4), 
		list(var="transmitpower",				what="double", n=1, size=4), 
		list(var="pulselength",					what="double", n=1, size=4), 
		list(var="bandwidth",					what="double", n=1, size=4), 
		list(var="sampleinterval",				what="double", n=1, size=4), 
		list(var="soundvelocity",				what="double", n=1, size=4), 
		list(var="absorptioncoefficient",		what="double", n=1, size=4), 
		list(var="heave", 						what="double", n=1, size=4), 
		list(var="roll", 						what="double", n=1, size=4), 
		list(var="pitch", 						what="double", n=1, size=4), 
		list(var="temperature", 				what="double", n=1, size=4), 
		list(var="heading", 					what="double", n=1, size=4), 
		                                	
		list(var="transmitmode", 				what="int",    n=1, size=2), 
		list(var="pulseform", 					what="int",    n=1, size=2), 
		                                	
		list(var="dirx",						what="double", n=1, size=4), 
		list(var="diry",						what="double", n=1, size=4), 
		list(var="dirz",						what="double", n=1, size=4), 
		list(var="gainrx",						what="double", n=1, size=4), 
		list(var="sacorrection",				what="double", n=1, size=4), 
		list(var="equivalentbeamangle",			what="double", n=1, size=4), 
		list(var="beamwidthalongshiprx",		what="double", n=1, size=4), 
		list(var="beamwidthathwartshiprx",		what="double", n=1, size=4), 
		list(var="anglesensitivityalongship", 	what="double", n=1, size=4), 
		list(var="anglesensitivityathwartship",	what="double", n=1, size=4), 
		list(var="angleoffsetalongship", 		what="double", n=1, size=4), 
		list(var="angleoffsetathwartship", 		what="double", n=1, size=4), 
		
		list(var="spare",						what="char",   n=2, size=1), 
		
		list(var="noisefilter",					what="int",    n=1, size=2), 
		list(var="beamwidthmode",				what="int",    n=1, size=2), 
		list(var="beammode",					what="int",    n=1, size=2), 
		
		list(var="beamwidthhorizontaltx",		what="double", n=1, size=4), 
		list(var="beamwidthverticaltx",			what="double", n=1, size=4), 
		
		list(var="offset",						what="int",    n=1, size=4), 
		list(var="count",						what="int",    n=1, size=4), 
		
		list(var="data",						what="double",    n=nIfBitpos4, size=4)
	)
	schema_ConfigHeader <- list(
		list(var="surveyname",					what="char",   n=128, size=1), 
		list(var="transectname",				what="char",   n=128, size=1), 
		list(var="soundername",					what="char",   n=128, size=1), 
		list(var="version",						what="char",   n=30,  size=1), 
		
		list(var="multiplexing",				what="int",    n=1,   size=2), 
		list(var="timebias",					what="int",    n=1,   size=4), 
		                                	
		list(var="soundvelocityaverage",		what="double", n=1,   size=4), 
		list(var="soundvelocitytransducer",		what="double", n=1,   size=4), 
		list(var="mruoffsetx",					what="double", n=1,   size=4), 
		list(var="mruoffsety",					what="double", n=1,   size=4), 
		list(var="mruoffsetz",					what="double", n=1,   size=4), 
		list(var="mrualphax",					what="double", n=1,   size=4), 
		list(var="mrualphay",					what="double", n=1,   size=4), 
		list(var="mrualphaz",					what="double", n=1,   size=4), 
		list(var="gpsoffsetx", 					what="double", n=1,   size=4), 
		list(var="gpsoffsety", 					what="double", n=1,   size=4), 
		list(var="gpsoffsetz", 					what="double", n=1,   size=4), 
		                                	
		list(var="spare",						what="char",   n=48,  size=1), 
		
		list(var="transceivercount", 			what="int",    n=1, size=4)
	)
	schema_TransceiverConfig <- list(
		list(var="channelid",					what="char",   n=128, size=1), 
		
		list(var="beamtype",					what="int",    n=1,   size=4), 
		
		list(var="frequency",					what="double", n=1,   size=4), 
		list(var="gain",						what="double", n=1,   size=4), 
		list(var="equivalentbeamangle",			what="double", n=1,   size=4), 
		list(var="beamwidthalongship",			what="double", n=1,   size=4), 
		list(var="beamwidthathwartship",		what="double", n=1,   size=4), 
		list(var="anglesensitivityalongship",	what="double", n=1,   size=4), 
		list(var="anglesensitivityathwartship",	what="double", n=1,   size=4), 
		list(var="anglesoffsetalongship",		what="double", n=1,   size=4), 
		list(var="angleoffsetathwartship",		what="double", n=1,   size=4), 
		list(var="posx",						what="double", n=1,   size=4), 
		list(var="posy",						what="double", n=1,   size=4), 
		list(var="posz",						what="double", n=1,   size=4), 
		list(var="dirx",						what="double", n=1,   size=4), 
		list(var="diry",						what="double", n=1,   size=4), 
		list(var="dirz",						what="double", n=1,   size=4), 

		list(var="pulselengthtable",			what="double", n=5,   size=4), 
		                                	
		list(var="spare2",						what="char",   n=8,   size=1), 
		
		list(var="gaintable",					what="double", n=5,   size=4), 
	
		list(var="spare3",						what="char",   n=8,   size=1), 
		
		list(var="sacorrectiontable",			what="double", n=5,   size=4), 
		
		list(var="spare4",						what="char",   n=52,  size=1)
	)
	schema_FIL1 <- list(
		list(var="stage",						what="int",    n=1,   size=2), 
		
		list(var="spare",						what="char",   n=2,   size=1), 
		list(var="channelID",					what="char",   n=128, size=1), 
		
		list(var="noofcoefficients",			what="int",    n=1,   size=2), 
		list(var="decimationfactor",			what="int",    n=1,   size=2), 
		
		list(var="coefficients",				what="double", n=nFIL1,	size=4)
	)
	schema_MRU0 <- list(
		list(var="heave",						what="double", n=1,	  size=4), 
		list(var="roll",						what="double", n=1,	  size=4), 
		list(var="pitch",						what="double", n=1,	  size=4), 
		list(var="heading",						what="double", n=1,	  size=4)
	)
	schema_DEP0 <- list(
		list(var="depth",						what="double", n=1,	  size=4), 
		list(var="parameter1",					what="double", n=1,	  size=4), 
		list(var="parameter2",					what="double", n=1,	  size=4)
	)
	
	# Get the datagram schema by type:
	schema <- get(paste("schema", dgName[1], sep="_"))
	
	# Rbind into a data frame:
	#schema <- data.table::rbindlist(schema)
	
	# Transpose the list:
	names_schema <- names(schema[[1]])
	schema <- lapply(seq_along(schema[[1]]), function(i) sapply(schema, "[[", i))
	names(schema) <- names_schema
	
	# Get the positions of the data in the input raw vector:
	
	# The n0 is the n inserted0 at the functions, which are assumed to cone last,and should thus not change the position in the raw vector:
	funs <- sapply(schema$n, is.function)
	n0 <- schema$n
	n0[funs] <- 0
	n0 <- unlist(n0)
	
	schema$len <- schema$size * n0
	schema$end <- cumsum(schema$len)
	schema$pos <- c(0, schema$end[-length(schema$end)])
	schema$start <- schema$pos + 1
	# Insert NAs in ends for the functions:
	schema$end[funs] <- NA
	
	# Create a funciton that accepts ..., since the parameters of readBin are input to the funciton in readVar():
	schema$fun <- c("readBin", "rawToCharDotDotDot")[as.numeric(schema$what == "char") + 1]
	
	### schema$fun <- c(readBin, function(x, ...) rawToChar(x))[as.numeric(schema$what == "char") + 1]
	
	# Convert to a list:
	schema <- as.list(schema)
	# Set the dgName as attribute:
	attr(schema, "dgName") <- dgName[1]
	
	schema
}

# A small function that supports ... in rawToChar, and accepting embedded null:
rawToCharDotDotDot <- function(x, ...){
	paste(rawToChar(x, multiple=TRUE), collapse="")
}
# A function that converts a vector of raw data as given by the schema:
convertRawOne <- function(i, x, schema, endian="little", offset=0){
	thisind <- offset + schema$pos[i] + seq_len(schema$len[i])
	out <- do.call(schema$fun[[i]], list(x[thisind], what=schema$what[i], n=schema$n[[i]], size=schema$size[i], endian=endian, signed=TRUE))
	out
}
convertRaw_old <- function(x, schema=NULL, dgName=c("RAW0", "RAW1", "ConfigHeader", "TransceiverConfig"), endian="little", offset=0){
	# Get the schema if missing:
	if(length(schema)==0){
		schema <- readEKRaw_GetSchema(dgName[1])
	}
	# Use apply, even though this is not suited for dataframes:
	out <- lapply(seq_along(schema[[1]]), convertRawOne, x=x, schema=schema, endian=endian, offset=offset)
	names(out) <- schema$var
	out
}


convertRawOneStatic <- function(i, x, schema, endian="little", offset=0){
	thisind <- offset + schema$pos[i] + seq_len(schema$len[i])
	out <- do.call(schema$fun[[i]], list(x[thisind], what=schema$what[i], n=schema$n[[i]], size=schema$size[i], endian=endian, signed=TRUE))
	out
}
convertRawOneDependent <- function(i, x, schema, data, endian="little", offset=0){
	# Convert only if n > 0:
	thisn <- schema$n[[i]](data)
	# Get the length of the current data variable (which is set to 0 for convenience in readEKRaw_GetSchema, but can be conputed as the product of n and size):
	thislen <- schema$size[i] * thisn
	thisind <- offset + schema$pos[i] + seq_len(thislen)
	if(thisn > 0){
		out <- do.call(schema$fun[[i]], list(x[thisind], what=schema$what[i], n=thisn, size=schema$size[i], endian=endian, signed=TRUE))
	}
	else{
		out <- NULL
	}
	out
}
convertRaw <- function(x, schema=NULL, dgName=c("RAW0", "RAW1", "ConfigHeader", "TransceiverConfig"), afterStatic=c("readEKRaw_getMode"), afterDependent=c("readEKRaw_complex2power", "readEKRaw_getAngles", "readEKRaw_getPower"), endian="little", offset=0, ...){
	# Get the schema if missing:
	if(length(schema)==0){
		schema <- readEKRaw_GetSchema(dgName[1])
	}
	# Get the static and dependent schema (dependent meaning with length dependent on the variables in the static schema):
	dependent <- sapply(schema$n, is.function)
	static <- which(!dependent)
	dependent <- which(dependent)
	
	# Use apply, even though this is not suited for dataframes:
	out <- lapply(static, convertRawOneStatic, x=x, schema=schema, endian=endian, offset=offset)
	names(out) <- schema$var[static]
	
	# Run various processing of the static data prior to extracting the data with dynamic length:
	for(fun in afterStatic){
		out <- do.call(fun, list(x=out, ...))
	}
	
	# Then the dependent:
	if(length(dependent)){
		temp <- lapply(dependent, convertRawOneDependent, x=x, schema=schema, data=out, endian=endian, offset=offset)
		names(temp) <- schema$var[dependent]
		out <- c(out, temp)
	}
	# Remove empty elements:
	out <- out[lengths(out) > 0]
	
	# Run various processing of the dependent data:
	for(fun in afterDependent){
		out <- do.call(fun, list(x=out, ...))
	}
	
	# Add certain variables for RAW datagrams:
	if(startsWith(attr(schema, "dgName"), "RAW")){
		# Set the 'number', which is the ping index of the file (set to NA here and then reset in readEKRawAll()), and the datagram time:
		out$number <- NA
		out$time <- attr(x, "dgTime")
	}
	
	out
}






