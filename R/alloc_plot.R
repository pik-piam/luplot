#' alloc_plot
#' 
#' Land allocation plot. This plot was created to be able to nicely plot the
#' output of the land function which can be found in the magpie library.
#' 
#' 
#' @usage
#' alloc_plot(land,title="",level="reg",print=TRUE,x="Year",
#' weight="Value/1000",fill="Data1",colors=NULL,legend_name="Land Type",
#' xlab=NULL,ylab="Area [billion ha]",
#' norm=FALSE,ncol=10,file=NULL,scale=4)
#' @param land A MAgPIE object created by the land function of the magpie
#' library (or a MAgPIE object of the same structure)
#' @param title Diagram title
#' @param level Aggregation level/Facetting. "reg" for a regional, "glo" for a
#' global or "cell" for a cellular plot. Alternatively, you can also define
#' here the facetting as used in the ggplot2 library, e.g. "Year~Region", or
#' ".~Data1"
#' @param print TRUE for direct printing. FALSE for the return of an object
#' which can be modified before printing.
#' @param x What should be printed on the x axis? (Year,Data1,Data2,...)
#' @param weight What should be stacked on top of each other? You can also use
#' calculations here such as "Value/1000" (default = Value/1000)
#' @param fill What should be distinguished by colors? (Year,Data1,Data2,...)
#' @param colors Which colors should be used (Default colors will be used for
#' colors=NULL)
#' @param legend_name Legend name, default name will be used for
#' legend_name=NULL
#' @param xlab x axis label, default name will be used for ylab=NULL
#' @param ylab y axis label, default name will be used for ylab=NULL
#' @param norm normalise land, FALSE or TRUE
#' @param ncol number of colums used for cellular plot (level="cell")
#' @param file a file name the output should be written to using ggsave
#' @param scale scale factor used in ggsave
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky, Misko Stevanovic,
#' Florian Humpenoeder
#' @seealso
#' \code{\link{comparisonplot},\link{histoplot},\link{get_layout_parameters},\link{scratch_legend}}
#' @examples
#' 
#'   \dontrun{
#'   alloc_plot(land=land(gdx,level="reg"),title="Total land (si0+nsi0 | global)",level="glo")
#'   alloc_plot(land=land(gdx,level="reg"),title="Total land (si0+nsi0 | regional)")
#'   } 
#' @export
#' @importFrom luscale superAggregate
#' @importFrom magclass dimSums
#' @importFrom ggplot2 ggplot aes_string labs scale_fill_manual ggtitle geom_bar facet_wrap facet_grid theme element_text ggsave aes label_wrap_gen
#' 
alloc_plot <- function(land,title="",level="reg",print=TRUE,x="Year",weight="Value/1000",fill="Data1",colors=NULL,legend_name="Land Type",xlab=NULL,ylab="Area [billion ha]",norm=FALSE,ncol=10,file=NULL,scale=4) {
  #require("ggplot2", quietly = TRUE)
  if (level %in% c("glo","reg","cell")) {
    if (level != "cell") land <- superAggregate(land,level=level,aggr_type="sum")
    if (norm) {
      total_land <- dimSums(land,dim=3)
      land <- land/total_land
    }
  } else stop("level is not glo, reg or cell")
  dland <- as.ggplot(land,asDate=FALSE)
  if(is.null(colors)) {
    if(fill=="Region") colors <- nice_colors()
    else colors <- c("chocolate4", "#E6AB02", "black", "darkgreen", "#66A61E", "honeydew4", "brown3")
  }  

  # assignes a time period length to each corresponding year
  period <- diff(as.numeric(levels(dland$Year)))
  year_period <- data.frame(Year=c(as.numeric(levels(dland$Year))), Period=c(1,period))
  dland$Period <- rep(NA,dim(dland)[1])
  for(i in 1:dim(dland)[1]){
    dland[i,"Period"] <- year_period[which(year_period$Year==dland[i,"Year"]),"Period"]
  }
  dland$Period <- as.numeric(dland$Period)
  # takes care that the bars' width ends at the given year's tick on the x-axis instead of being 
  # centered around each year
  dland$Year <- as.numeric(as.vector(dland$Year)) - 0.5*dland$Period
  dland$Period <- dland$Period - 0.5
  
  p <- ggplot(dland, aes_string(x=x, y=weight, fill=fill, width="Period")) 
  if(!is.null(ylab)) p <- p + labs(y=ylab)
  if(!is.null(xlab)) p <- p + labs(x=xlab)
  if(!is.null(legend_name)) { 
    p <- p + scale_fill_manual(name=legend_name,values=colors)
  } else {
    p <- p + scale_fill_manual(values=colors) 
  }
  # 0.5 regulates the space width between bars
  p <- p + ggtitle(title)  + geom_bar(stat="identity")
  if(level=="cell") {
    p <- p + facet_wrap(~Region+Cell,ncol=ncol,labeller = label_wrap_gen(multi_line=FALSE))
  } else {
    if(level=="reg") level<-".~Region"
    if(level=="glo") level<-".~Region"
    if(length(grep("~",level))==1) {
      p <- p + facet_grid(level)
  #     if(x=="Year") p <- p + scale_x_discrete(breaks=as.character(getYears(land,as.integer=TRUE)[round(nyears(land)*c(0.3,0.7))])) 
    }
  }
  p <- p + theme(axis.text.x = element_text(angle=90))
  if (!is.null(file)) {ggsave(file,p,scale=scale)} else if (print==TRUE) {print(p)} else {return(p)}
}

