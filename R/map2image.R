#' Plot MAgPIE object as map
#' 
#' Function to plot MAgPIE objects as maps (without limitations of plotmap)
#' 
#' 
#' @usage map2image(x, type = "pdf", name = NULL, folder = ".", years =
#' getYears(x), data = getNames(x), onefile = TRUE, res = c(30,
#' 15),main_add=NULL, ...)
#' @param x MAgPIE object or file name of MAgPIE file
#' @param type output type: "screen", "pdf", or "png"
#' @param name name to be used in file name, if NULL either the MAgPIE object
#' file name is used or the file is named "map2image"
#' @param folder folder the output should be written to
#' @param years years to use
#' @param data data columns to use
#' @param onefile (only pdf) should be plotted in one or in separate files?
#' @param res output resolution c(x,y)
#' @param main_add additional map title
#' @param \dots Additional arguments passed to plotmap
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{plotmap}}
#' @examples
#' 
#' #magmap("test.mz")
#' @export
#'@importFrom magclass getYears read.magpie
#'@importFrom grDevices png

map2image <- function(x,type="pdf",name=NULL,folder=".",years=getYears(x),data=getNames(x),onefile=TRUE,res=c(30,15),main_add=NULL,...) {
  if(is.character(x)) {
    if(is.null(name)) name <- x
    x <- read.magpie(x)
  } else {
    if(is.null(name)) name <- "map2image"
  }

  if(is.numeric(years)) years <- getYears(x)[years]
  if(is.numeric(data)) data <- getNames(x)[data]
  if(is.null(years)) years <- getYears(x)
  if(is.null(years)) years <- 1
  if(is.null(data))  data <- getNames(x)
  if(is.null(data)) data <- 1

  .path <- function(...,ftype=NULL) {
    if(!is.null(ftype)) if(substring(ftype,1,1)!='.') ftype <- paste('.',ftype,sep='')
    out <- paste(...,sep="/")
    out <- paste(gsub("//","/",out),ftype,sep="")
    first <- list(...)[[1]]
    .tmp <- function(first,out) {
      manipulate <- FALSE
      if(is.null(first)) manipulate <- TRUE
      else if(first=="") manipulate <- TRUE
      if(manipulate) out <- gsub("^/+","",out)
      return(out)
    }
    if(length(first)>1) {
      for(i in 1:length(first)) out[i]<- .tmp(first[i],out[i])
    } else {
      out <- .tmp(first,out)
    }
    return(out)
  }
  
  
  if(type=="pdf") {
	  pdf(file=ifelse(onefile, .path(folder,name,ftype="pdf"), .path(folder,paste(name,"%03d.pdf",sep=""))),res[1],res[2],onefile)
    cex.corr=1
  } else if (type=="png") {
  	png(filename=.path(folder,paste(name,"%03d.png",sep="")),width=res[1],height=res[2],units="in",res=72)
  	cex.corr=1
  } else {
    dev.new(width=res[1],height=res[2])
    cex.corr=0.5
  }
  for(y in years) {
    for(d in data) {
      try(plotmap(x[,y,d],main=paste(main_add,substr(y,2,5),"-",d),cex.corr=cex.corr,...))
    }
  }
  if(type=="pdf" | type=="png") dev.off()
}

