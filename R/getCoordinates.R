getCoordinates <- function(degree=FALSE) {
  out <- coord
  if(degree) {
    out$lon <- (out$lon-0.5)*0.5-180
    out$lat <- (out$lat-0.5)*0.5-90  
  }
  return(as.matrix(out))
}