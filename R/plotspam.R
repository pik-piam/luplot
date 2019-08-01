#' Plots Spam object as map
#' 
#' Function to plot spam relation objects as maps
#' 
#' 
#' @usage plotspam(spam,type="pdf",name="spamplot", folder=".",color="brewer")
#' @param spam A spam relation object (either object itself or file name of a
#' spam object)
#' @param type output type: "screen", "pdf", or "png"
#' @param name name to be used in file name
#' @param folder folder the output should be written to
#' @param color brewer: 12 colors iterating, random: (number of cluster)-different colors
#' @author Jan Philipp Dietrich, Kristine Karstens
#' @seealso \code{\link{map2image}}
#' @examples
#' 
#' \dontrun{plotspam("0.5-to-h100.spam")}
#' @export
#' @importFrom grDevices rgb
#' @importFrom stats kmeans runif
#' @importFrom luscale read.spam speed_aggregate
#' @importFrom magclass as.magpie



plotspam <- function(spam,type="pdf",name="spamplot",folder=".", color="brewer") {
  #require("RColorBrewer", quietly = TRUE)
  #require("luscale", quietly = TRUE)
  
  if(is.character(spam)) spam <- read.spam(spam)
  
  test        <- 1:dim(spam)[1]
  names(test) <- paste("GLO",test,sep=".")
  test        <- as.magpie(test)
  plot        <- speed_aggregate(test,rel=t(spam))
  
  if(color=="brewer"){
    palette=c(rep(brewer.pal(12, "Set3"),dim(spam)[1] %/% 12),brewer.pal(dim(spam)[1] %% 12, "Set3"))
    
  } else if(color=="random"){
    
    n <- 10000
    k <- dim(spam)[1]
    ColorSpace <- array(c(runif(n), runif(n), runif(n)), c(n,3))
    km         <- kmeans(ColorSpace, k, iter.max=20)
    palette    <- rgb(km$centers)
    
  } else stop("Color scheme is not available.")
  
  map2image(plot,name=name,folder=folder,type=type,legend_range=c(0,dim(spam)[1]),legend_colours=palette)
}
