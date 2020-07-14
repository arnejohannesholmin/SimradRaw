#*********************************************
#*********************************************
#' Calcualtes the data gram length of a datagram to write to a Simrad raw file.
#'
#' @param thisdata  is a list of the datagram.
#' @param thisdatagramName  is the type of datagram.
#' @param nBytesDgHeader  is the number of bytes of the datagram header.
#' @param nBytesSampledataInfo  is the number of bytes of the sample data information.
#'
#' @return
#'
#' @examples
#' \dontrun{}
#'
#' @importFrom TSD strff
#'
#' @export
#' @rdname getDgLen
#'
getDgLen<-function(thisdata, thisdatagramName, nBytesDgHeader, nBytesSampledataInfo){
	
	############ AUTHOR(S): ############
	# Arne Johannes Holmin
	############ LANGUAGE: #############
	# English
	############### LOG: ###############
	# Start: 2014-11-10 - Clean version.
	########### DESCRIPTION: ###########
	# Calcualtes the data gram length of a datagram to write to a Simrad raw file.
	########## DEPENDENCIES: ###########
	#
	############ VARIABLES: ############
	# ---thisdata--- is a list of the datagram.
	# ---thisdatagramName--- is the type of datagram.
	# ---nBytesDgHeader--- is the number of bytes of the datagram header.
	# ---nBytesSampledataInfo--- is the number of bytes of the sample data information.
	

	##################################################
	##################################################
	if(TSD::strff("raw0",thisdatagramName)){
		nBytesDgHeader + nBytesSampledataInfo + (2*as.numeric(length(thisdata$power)>0) + as.numeric(length(thisdata$athwartship)>0) + as.numeric(length(thisdata$alongship)>0)) * thisdata$count
		}
	else if(TSD::strff("raw1",thisdatagramName)){
		nBytesDgHeader + nBytesSampledataInfo + 4 * 2*thisdata$ncomplexpersample*thisdata$count
		}
	else if(TSD::strff("nme",thisdatagramName)){
		# Add two null characters for some unknown reason:
		nBytesDgHeader + nchar(thisdata) + 2
		}
	else{
		nBytesDgHeader + nchar(thisdata)
		}
	##################################################
	##################################################
	}
