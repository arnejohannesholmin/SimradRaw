% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/readEKRaw.R
\name{readEKRaw_ReadHeader}
\alias{readEKRaw_ReadHeader}
\title{Read the header from a Simrad raw file.}
\usage{
readEKRaw_ReadHeader(fid, dgHeader, endian = "little")
}
\arguments{
\item{fid}{is the path to the raw file.}

\item{dgHeader}{A list containing the dgTime.}

\item{endian}{is the endian of the file, defaulted to .Platform$endian (changed from "big" by Arne Johannes Holmin 2012-07-31).}
}
\description{
Read the header from a Simrad raw file.
}
\examples{
\dontrun{}

}
