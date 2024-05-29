#' Function to plot maps
#' 
#' This function plots a world region map directly from .csv files
#' 
#' @param file a .csv file 
#' @param col color
#' @param ... Further attributes such as color palette and map name
#' @author Jan Philipp Dietrich, Ewerton Araujo
#' @importFrom utils read.csv
#' @seealso
#' \code{\link{plotcountrymap}},\code{\link{plotmap2}}
#' @examples
#' \dontrun{
#' plotRegionMapping("../regionmapping.csv", col = NULL, mapTitle = "")
#' }
#' @export


plotRegionMapping <- function(file, col=NULL, ...) {
  x <- read.csv(file, sep = ";")
  x <- x[-1]
  x$RegionCode <- as.character(x$RegionCode)
  m <- as.magpie(x,spatial=1, tidy=TRUE)
  if(is.null(col)) {
    col <- c("#FF0000","#00FF00", "#0000FF", "#FFFF00", "#FF00FF", "#00FFFF", "#800000", "#008000","#000080", "#808000", "#800080", "#008080", "#FFA500", "#FFA07A", "#EEE8AA", "#00BFFF")
  }
  plotcountrymap(m, catMethod="categorical", col=col, ...)
}