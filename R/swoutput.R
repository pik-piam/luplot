#' swoutput
#' 
#' Creates LaTeX code for plot (\code{\link[luplot]{magpie2ggplot}}) and data
#' table (\code{\link[xtable]{print.xtable}}) based on MAgPIE objects and adds
#' it to a \code{"\linkS4class{swStream}"} object.
#' 
#' Creates preformatted plots and data tables based on MAgPIE objects. Format
#' is optimized for readability. Data tables with more than 12 columns are
#' rescaled for fitting on portrait pages.
#' 
#' @usage
#' swoutput(stream,data,unit="unit",plot=TRUE,table=TRUE,scenario_name="default",
#' text_size=16,digits=0,plot_level=NULL,...)
#' @param stream The \code{swStream} object to be modified
#' @param data MAgPIE object or list of MAgPIE objects. For a list of MAgPIE
#' objects the name of the list entry (has to be character) is used as scenario
#' name.
#' @param unit unit of data (Character). Used as ylab in
#' \code{\link[luplot]{magpie2ggplot}} and table caption in
#' \code{\link[xtable]{print.xtable}}
#' @param plot TRUE or FALSE. If TRUE, LaTeX code for plot
#' (\code{\link[luplot]{magpie2ggplot}}) is added
#' @param table TRUE or FALSE. If TRUE, LaTeX code for data table
#' (\code{\link[xtable]{print.xtable}}) is added
#' @param scenario_name Name of the scenario. Only used if data is not a list.
#' @param text_size Size of text used in \code{\link[luplot]{magpie2ggplot}}
#' @param digits Number of digits for rounding. Used in
#' \code{\link[xtable]{xtable}}
#' @param plot_level NULL for all (default), reg or glo
#' @param ... Further options passed to \code{\link[luplot]{magpie2ggplot}} and
#' \code{\link[xtable]{print.xtable}}
#' @return No return value
#' @author Florian Humpenoeder
#' @importFrom lusweave swfigure swtable
#' @importFrom magclass ncells nyears ndata getNames getRegions
#' @export
#' @seealso
#' \code{"\linkS4class{swStream}"},\code{\link{swopen}},\code{\link{swclose}},\code{\link{swR}},\code{\link{swtable}},\code{\link{swfigure}}
#' @examples
#' 
#' \dontrun{
#' sw<-swopen("croparea_reg.pdf")
#' swlatex(sw,"\newpage")
#' swlatex(sw,"\section{Croparea - regional}")
#' swoutput(sw,croparea(gdx),"mio. ha",scenario="test")
#' swclose(sw)}
#' 
swoutput <- function(stream,data,unit="unit",plot=TRUE,table=TRUE,scenario_name="default",text_size=16,digits=0,plot_level=NULL,...) {
  if (!is.list(data)) {
    temp <- data
    data <- list()
    data[[scenario_name]] <- temp
  }
  if (is.null(plot_level)) ggdata <- data
  else if (plot_level == "reg") {
    ggdata <- lapply(data,function(x) x[setdiff(getRegions(x),"GLO"),,])     
  } else if (plot_level == "glo") {
    plot_level <- "GLO"
    ggdata <- lapply(data,function(x) x[plot_level,,])     
  }
  if (plot) swfigure(stream,print,magpie2ggplot2(data=ggdata,ylab=unit,text_size=text_size,...),sw_option="width=10")
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