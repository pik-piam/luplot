#' Function to plot maps
#' 
#' Plots maps of map objects or MAgPIE objects
#' 
#' This function is an easy application of the mapCountryData function for ISO3
#' country-level magpie objects
#' 
#' @usage plotcountrymap(x, hatching=FALSE, ...)
#' @param x a magpie object with ISO3 country names, one year and one name
#' dimension
#' @param hatching if hatching is activated, the second name column will be
#' used for hatching (stripes)
#' @param ... Further attributes which are passed on to the function
#' mapCountryData of the package rworldmap
#' @author Benjamin Leon Bodirsky
#' @seealso \code{\link{plotmap2}}
#' @examples
#' \dontrun{
#' data(population_magpie)
#' test<-population_magpie
#' dimnames(test)[[1]]<-c("AFG","DEU","FRA","EGY","IND","IDN","RUS","CHN","USA","YEM")
#' plotcountrymap(test[,"y2005",1])
#' }
#' @export
#'@importFrom magclass getYears getRegions 
#'@importFrom rworldmap joinCountryData2Map mapCountryData
#'@importFrom utils methods

plotcountrymap<-function(x,hatching=FALSE,...) {
 # require(methods)
  namedim<-getNames(x)
  if(is.null(namedim)){namedim=1}
  year<-getYears(x)
  if(is.null(year)){year=1}
  if (hatching==FALSE) {
    if (length(namedim)>1) {stop("please provide only one name-column per plot")}
  } else {
    if (length(namedim)>2) {stop("please provide only one name-column per plot")}
  }
  if (length(year)>1) {stop("please provide only one year per plot")}
  countries <- getRegions(x)
  values <- as.vector(x[,year,namedim])
  
  if (hatching){
    DF <- data.frame(country = countries,namedim = values,hatching=as.vector(x[,year,2]))
    dimnames(DF)[[2]][[2]] <- paste(namedim[1],substr(year,2,5))
    dimnames(DF)[[2]][[3]] <- paste(namedim[2],substr(year,2,5))
    mapobject <- joinCountryData2Map(DF, joinCode = "ISO3",nameJoinColumn = "country")
    mapCountryData(mapobject, nameColumnToPlot = dimnames(DF)[[2]][[2]],nameColumnToHatch=dimnames(DF)[[2]][[3]],...)
    
  } else{
    DF <- data.frame(country = countries,namedim = values)
    dimnames(DF)[[2]][[2]] <- paste(namedim,substr(year,2,5))
    mapobject <- joinCountryData2Map(DF, joinCode = "ISO3",nameJoinColumn = "country")
    mapCountryData(mapobject, nameColumnToPlot = dimnames(DF)[[2]][[2]],...)
    
  }

}
