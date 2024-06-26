% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get.TVG.R
\name{get.TVG}
\alias{get.TVG}
\title{Calculate the Time Varied Gain function.}
\usage{
get.TVG(beams, x = NULL, linear = TRUE, TVG.exp = 2, Ro = NULL, thr1m = FALSE)
}
\arguments{
\item{beams}{is the list of beams inputs including "lenb" (lengths of beams), "sint" (pulse length), "absr" (absorption coefficent of the beams) and "asps" (average speed of sound).}

\item{x}{is the input acoustic data arranged in an array having the radial part along the first dimension, the beams along the second or second and third, and time steps and other optional dimensions along the last dimension (lenb,numb,numt).}

\item{linear}{is TRUE if x is in linear volume backscatter values (sv) and FALSE if x is in logarithmic volume backscatter values (Sv).}

\item{TVG.exp}{is the exponent of the eamotric spreading of the sound wave, theoretically 2 for Sv and 4 for TS.}

\item{Ro}{is used in Gavins work, shifting the first voxel to be imaginarily placed inside of the sonar.}

\item{thr1m}{Logical: If TRUE apply a rule that TVG closer than 1 m should not cause increasedd level (used in LSSS?))}
}
\description{
Calculate the Time Varied Gain function.
}
\examples{
\dontrun{}

}
