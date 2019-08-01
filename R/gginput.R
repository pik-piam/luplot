#' gginput
#' 
#' Converts a MAgPIE object into a dataframe usable for ggplot
#' 
#' 
#' @usage gginput(data, scatter=NULL, mappings=NULL, na.rm=FALSE, verbose=TRUE)
#' @param data MAgPIE object which provides the data for ggplot
#' @param scatter In the case you want to make a scatterplot you have to
#' specify here which data dimension of the MAgPIE object should be used for
#' the scatter plot (name of the dimension)
#' @param mappings A list of mappings used to create combinations of columns
#' which then can be used for aesthetics in ggplot
#' @param na.rm Boolean which decides whether NA values should be removed from
#' the data.
#' @param verbose Boolean which decides whether info messages should be printed to screen or not.
#' @return Dataframe usable for ggplot
#' @author Jan Philipp Dietrich
#' @examples
#' \dontrun{
#' library(ggplot2)
#' data(population_magpie)
#' ggplot(gginput(population_magpie,scatter="scenario"),aes(x=.value.A2,y=.value.B1,color=i)) 
#' + geom_point() + geom_smooth(method="loess") + facet_wrap(~.spat1)
#' }
#' @export
#' @importFrom stats reshape na.omit

gginput <- function(data,scatter=NULL,mappings=NULL,na.rm=FALSE,verbose=TRUE) {
  df <- as.data.frame(data,rev=2)
  dimtype <- attr(df,"dimtype")
  if(!is.null(scatter)) {
    if(scatter %in% dimtype) {
      scatter <- names(df)[scatter==dimtype]
    }
    if(!(scatter %in% names(df))) stop('Unknown data column "',scatter,'" for scatter plot data.')
    dimtype <- dimtype[scatter!=names(df)]
    df <- reshape(df,timevar=scatter,idvar=names(df)[!(names(df) %in% c(".value",scatter))],direction="wide")
    tmp <- df[grep("^\\.value\\.",names(df),value=TRUE)]
    names(tmp) <- paste0(".value",1:length(tmp))
    df <- cbind(df,tmp)
  }
  nonval <- length(dimtype)-1
  df <- cbind(df[1:nonval],df)
  names(df)[1:nonval] <- dimtype[1:nonval]
  if(na.rm) {
    data <- na.omit(data)
  }
  if(!is.null(mappings)){
    for(m in names(mappings)) {
      if(!all(mappings[[m]]%in% names(df))) {
        nf <- mappings[[m]][!(mappings[[m]]%in% names(df))]
        stop("Following mapping elements could either not be found in the data or are not available for a mapping as they are already used in argument \"scatter\": ", paste(nf,collapse=", "))
      }
      df[m] <- do.call(paste,c(df[mappings[[m]]],sep="."))
    }  
  }
  if(verbose) {
    n <- names(df)
    cat("\nData successfully prepared for ggplot!\n")
    cat(" data columns: ")
    cat(paste(grep("^\\.value",grep("^\\.",n,value=TRUE),value=TRUE,invert=TRUE),"=",grep("^\\.",n,value=TRUE,invert=TRUE)))
    
    if(!is.null(scatter)) {
    cat("\n value columns: ",
        paste(grep("^\\.value[^\\.]",n,value=TRUE),"=",grep("^\\.value\\.",n,value=TRUE)))
    } else {
      cat("\n value column: .value")
    }
    cat("\n")
  }
  return(df)
}