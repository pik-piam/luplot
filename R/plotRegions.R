#' plotRegions
#' 
#' Maps world regions based on a provided mapping.
#' 
#' @param mapping A path to the mapping that provides iso to regional definition. 
#' @param cex Controls the legend font size. Default is 1. 
#' @param pt.cex Controls the legend color box size. Default is 2. 
#' @param title Title of the map
#' @param addLegend decides whether a legend is plotted or not
#' @param ... further arguments forwarded to \code{\link{plotcountrymap}}
#' @return Plots a regional map.
#' @details The function works only with the mappings as defined in the \code{inputdata/mappings/regional} folder
#' @seealso \code{\link{plotcountrymap}}
#' @author Mishko Stevanovic
#' 
#' @examples
#' \dontrun{
#' plotRegions(mapping="inputdata/mappings/regional/regionmappingMAgPIE.csv")
#' }
#' 
#' @export
#' @importFrom mip plotstyle
#' @importFrom utils read.csv
#' @importFrom rworldmap addMapLegendBoxes

plotRegions <- function(mapping=NULL, cex=1, pt.cex=2, title="", addLegend=TRUE, ...){
  
  if(is.null(mapping)) stop("The path to the mapping is missing. Provide the correct path.")
  
  # get and prepare the mapping
  mag_reg <- read.csv(mapping,sep=";")
  map <- as.data.frame(mag_reg[,c("RegionCode","CountryCode")])
  map$RegionCode <- as.character(map$RegionCode)
  map$CountryCode <- as.character(map$CountryCode)
  
  # adds missing iso countries to the regional mapping
  ## Kosovo to Serbia
  tiso <- "KOS"
  treg <- map[match("SRB",map$CountryCode),"RegionCode"]
  map <- rbind(map, c(treg,tiso))
  
  # convert to a spatial magpie object
  x <- as.magpie(map[2:1],spatial=1)
  getNames(x) <- title
  
  # colors for regional map
  col <- plotstyle(levels(as.factor(map[[1]])))
  
  # create a plot
  maplot <- plotcountrymap(x, colourPalette=col,
                           catMethod = "categorical",
                           addLegend=FALSE,
                           ...)
  if(addLegend) do.call(addMapLegendBoxes, c(maplot
                               , x='bottomleft'
                               , title="Regions"
                               , cex=cex
                               , pt.cex=pt.cex))

}


