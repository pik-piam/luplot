#' Function to convert a MAgPIE object into a map.
#' 
#' This function converts MAgPIE objects into map objects as they are used in
#' image.plot. Currently, only MAgPIE objects are allowed which contain data
#' for exactly one year and element.
#' 
#' 
#' @usage magpie2map(x)
#' @param x A MAgPIE object with 1 year and 1 elem
#' @return A map object which is a list of three elements: x, y coordinates on
#' a world map and z values at each coordinate.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{plotmap}}
#' @export
#' @importFrom magclass is.magpie ncells nyears ndata

magpie2map <- function(x) {
  if(!is.magpie(x)) stop("Data is not in MAgPIE format!")
  if(ncells(x)!=59199) stop("Data has more or less than 59199 cells, transformation rule exists at the moment only for 0.5deg data!")
  if(nyears(x)!=1) stop("Data for no or more than 1 year is provided, but only 1 data set can be transformed, please remove unwanted data first!")
  if(ndata(x)!=1) stop("Data for no or more than 1 element is provided, but only 1 data set can be transformed, please remove unwanted data first!")
  out <- list(); out$x <- (0:719)/2-179.75; out$y <- (0:359)/2-89.75
  out$z <- matrix(NA,720,360); out$z[getCoordinates()] <- x[,1,1]
  return(out)
}

