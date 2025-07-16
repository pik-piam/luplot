#' swoutput
#' 
#' Creates LaTeX code for plot (\code{\link[luplot]{magpie2ggplot2}}) and data
#' table (\code{\link[xtable]{print.xtable}}) based on MAgPIE objects and adds
#' it to a \code{\link[lusweave:swStream-class]{swStream}} object.
#' 
#' Creates preformatted plots and data tables based on MAgPIE objects. Format
#' is optimized for readability. Data tables with more than 12 columns are
#' rescaled for fitting on portrait pages.
#' 
#' @usage
#' swoutput(stream,data,unit="unit",plot=TRUE,table=TRUE,scenarioName="default",
#' textSize=16,digits=0,plotLevel=NULL,...)
#' @param stream The \code{swStream} object to be modified
#' @param data MAgPIE object or list of MAgPIE objects. For a list of MAgPIE
#' objects the name of the list entry (has to be character) is used as scenario
#' name.
#' @param unit unit of data (Character). Used as ylab in
#' \code{\link[luplot]{magpie2ggplot2}} and table caption in
#' \code{\link[xtable]{print.xtable}}
#' @param plot TRUE or FALSE. If TRUE, LaTeX code for plot
#' (\code{\link[luplot]{magpie2ggplot2}}) is added
#' @param table TRUE or FALSE. If TRUE, LaTeX code for data table
#' (\code{\link[xtable]{print.xtable}}) is added
#' @param scenarioName Name of the scenario. Only used if data is not a list.
#' @param textSize Size of text used in \code{\link[luplot]{magpie2ggplot2}}
#' @param digits Number of digits for rounding. Used in
#' \code{\link[xtable]{xtable}}
#' @param plotLevel NULL for all (default), reg or glo
#' @param ... Further options passed to \code{\link[luplot]{magpie2ggplot2}} and
#' \code{\link[xtable]{print.xtable}}
#' @return No return value
#' @author Florian Humpenoeder
#' @importFrom lusweave swfigure swtable
#' @importFrom magclass ncells nyears ndata getNames getRegions
#' @export
#' @seealso
#' \code{\link[lusweave:swStream-class]{swStream}},\code{\link[lusweave]{swopen}},\code{\link[lusweave]{swclose}},\code{\link[lusweave]{swR}},\code{\link[lusweave]{swtable}},\code{\link[lusweave]{swfigure}}
#' @examples
#'
#' \dontrun{
#' sw<-swopen("croparea_reg.pdf")
#' swlatex(sw,"\newpage")
#' swlatex(sw,"\section{Croparea - regional}")
#' swoutput(sw,croparea(gdx),"mio. ha",scenario="test")
#' swclose(sw)}
#'
swoutput <- function(stream, data, unit = "unit", plot = TRUE, table = TRUE,
                     scenarioName = "default", textSize = 16, digits = 0, plotLevel = NULL, ...) {
  if (!is.list(data)) {
    temp <- data
    data <- list()
    data[[scenarioName]] <- temp
  }
  if (is.null(plotLevel)) ggdata <- data
  else if (plotLevel == "reg") {
    ggdata <- lapply(data,function(x) x[setdiff(getRegions(x),"GLO"),,])     
  } else if (plotLevel == "glo") {
    plotLevel <- "GLO"
    ggdata <- lapply(data,function(x) x[plotLevel,,])     
  }
  if (plot) swfigure(stream,print,magpie2ggplot2(data=ggdata,ylab=unit,text_size=textSize,...),sw_option="width=10")
  if (table) {
    for (scenario in names(sapply(data, names))) {
#       if (nyears(data[[scenario]]) > 12) {
#         scalebox <- 12/nyears(data[[scenario]])
#       } else {
#         scalebox <- 1
#       }
      if (ncells(data[[scenario]]) > 1 & nyears(data[[scenario]]) > 1 & ndata(data[[scenario]]) > 1) {
        for (i in getNames(data[[scenario]])) {  
          swtable(stream,data[[scenario]][,,i],table.placement="H",caption.placement="top",caption=paste(gsub("_","\\\\_",scenario),"-",gsub("_","\\\\_",i),"-",unit),vert.lines=1,align="r",hor.lines=1,transpose=T,digits=digits,...)
        } 
      } else {
        swtable(stream,data[[scenario]],table.placement="H",caption.placement="top",caption=paste(gsub("_","\\\\_",scenario),"-",unit),vert.lines=1,align="r",hor.lines=1,transpose=T,digits=digits,...)
      }
    }
  }
}