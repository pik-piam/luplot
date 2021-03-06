% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/magpie2map.R
\name{magpie2map}
\alias{magpie2map}
\title{Function to convert a MAgPIE object into a map.}
\usage{
magpie2map(x)
}
\arguments{
\item{x}{A MAgPIE object with 1 year and 1 elem}
}
\value{
A map object which is a list of three elements: x, y coordinates on
a world map and z values at each coordinate.
}
\description{
This function converts MAgPIE objects into map objects as they are used in
image.plot. Currently, only MAgPIE objects are allowed which contain data
for exactly one year and element.
}
\seealso{
\code{\link{plotmap}}
}
\author{
Jan Philipp Dietrich
}
