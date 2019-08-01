#' histoplot2
#' 
#' Function for plotting MAgPIE results and historical data with ggplot2
#' 
#' 
#' @usage
#' histoplot2(data_mag,data_hist,hist_start=1960,color="Scenario",
#' ylab="Value",title=NULL,facet_x="Region",facet_y=NULL,
#' labs=c("MAgPIE output","Historical Data"),text_size=12,scales="fixed",ncol=5,
#' xlim=NULL,ylim=NULL,breaks_x=waiver(),
#' breaks_y=waiver(),xaxis_angle=90,xlab="Year",
#' linewidth=1,pointwidth=3,col_data=NULL,col_hist=NULL,show_grid=TRUE,
#' axis_text_col="black",space="fixed",legend_data_ncol=1,
#' legend_hist_ncol=1,facet_style="paper",na.rm=FALSE,...)
#' @param data_mag MAgPIE object or list of MAgPIE objects or dataframe of
#' MAgPIE object(s); the name of the list entries is used as scenario name in
#' the legend
#' @param data_hist list of MAgPIE objects with historial data; the name of the
#' list entries is used as dataset name in the legend
#' @param hist_start historical data will be shown starting from the year
#' specified in hist_start
#' @param color Dimension to be colored, default: "Scenario"
#' @param ylab y-axis text
#' @param title title appering at the top of the plot
#' @param facet_x x-axis facet, default: "Region"
#' @param facet_y y-axis facet, default: NULL
#' @param labs vector with legend titles for data_mag and data_hist
#' @param text_size text size of axis, legend and title
#' @param scales fixed (default), free, free_y or free_x; ?facetgrid for
#' details
#' @param ncol Number of columns used in facet_wrap function
#' @param xlim x axis limits; NULL or vector with limits
#' @param ylim y axis limits; NULL or vector with limits
#' @param breaks_x Vector of x-axis breaks, by default ggplot2 takes the
#' decision
#' @param breaks_y Vector of y-axis breaks, by default ggplot2 takes the
#' decision
#' @param xaxis_angle Angle of xaxis text in degree, default=90
#' @param xlab x-axis text
#' @param linewidth linewidth, default=1
#' @param pointwidth pointwidth, default=3
#' @param col_data vector of colors for data_mag, if NULL colors are chosen
#' automaticly
#' @param col_hist vector of colors for data_hist, if NULL colors are chosen
#' automaticly
#' @param show_grid show minor and major grid lines; FALSE (default) or TRUE
#' @param axis_text_col color of axis text and ticks; default value: "black"
#' @param space fixed (default), free, free_y or free_x; ?facetgrid for details
#' @param legend_data_ncol Number of columns used in legend for data_mag
#' @param legend_hist_ncol Number of columns used in legend for data_hist
#' @param facet_style style of facets, default or paper
#' @param na.rm Boolean deciding whether NA values should be filtered out of
#' the data or not.
#' @param ... Further options passed to as.ggplot
#' @return ggplot2 object
#' @author Florian Humpenoeder
#' @seealso \code{\link{scratch_plot},\link{histoplot},\link{magpie2ggplot2},}
#' @examples
#' 
#' \dontrun{
#' gdx <- list()
#' gdx[["Test"]] <- "fulldata.gdx"
#' mag <- mapply("*",lapply(gdx,emissions,level="glo",
#' type="co2_c",cumulative=FALSE),44/12,SIMPLIFY=FALSE)
#' hist <- lapply(getData(emissions,gdx=gdx,type="co2_c",
#' level="glo")[[1]][[2]][[1]],function(x) return(x*44/12))
#' histoplot2(data_mag = mag,data_hist = hist,ylab = expression(bold(MtCO[2]/yr)),legend_hist_ncol = 2)}
#' 
#' @export
#' @importFrom magclass getYears mbind setYears
#' @importFrom ggplot2 waiver ggplot facet_grid facet_wrap geom_line aes_string geom_point stat_summary scale_fill_manual guide_legend scale_color_manual theme ggtitle element_rect element_line element_text scale_alpha_discrete element_blank scale_x_continuous geom_vline
#' @importFrom RColorBrewer brewer.pal
#' @importFrom stats as.formula
histoplot2 <- function(data_mag,data_hist,hist_start=1960,color="Scenario",ylab="Value",title=NULL,facet_x="Region",facet_y=NULL,labs=c("MAgPIE output","Historical Data"),text_size=12,scales="fixed",ncol=5,xlim=NULL,ylim=NULL,breaks_x=waiver(),breaks_y=waiver(),xaxis_angle=90,xlab="Year",linewidth=1,pointwidth=3,col_data=NULL,col_hist=NULL,show_grid=TRUE,axis_text_col="black",space="fixed",legend_data_ncol=1,legend_hist_ncol=1,facet_style="paper",na.rm=FALSE,...){
  .e <- environment()
  #require(ggplot2, quietly = TRUE)
  #require(RColorBrewer, quietly = TRUE)
  #  assign("set1",set1,envir=.e)
  number_ticks <- function(n) {function(limits) pretty(limits, n)}
  if (!is.data.frame(data_mag)) data_mag <- as.ggplot(data_mag,...)
  if (na.rm) data_mag <- na.omit(data_mag)
  if (!is.null(data_hist)) data_hist <- lapply(data_hist, function(x) x[,which(getYears(x,as.integer = TRUE) >= as.numeric(hist_start)),])
  #set colors
  if (is.null(col_data)) {
    col_data <- brewer.pal(length(unique(data_mag[["Scenario"]])),"Set1")
  }
  
  if (is.null(col_hist)) {
    col_hist <- brewer.pal(length(data_hist),"Set2")
  }
  p <- ggplot(data_mag, aes_string(x="Year", y="Value"), environment=.e)
  if (!is.null(facet_y)) p <- p + facet_grid(as.formula(paste(facet_y, "~", facet_x)), scales=scales, space=space)
  else if (!is.null(facet_x)) p <- p + facet_wrap(as.formula(paste("~", facet_x)), scales=scales, ncol=ncol)
  
  p <- p + geom_line(mapping=aes_string(color=color), show.legend = TRUE,size=linewidth) + geom_point(mapping=aes_string(color=color), show.legend = TRUE,size=pointwidth)
  up <- function(x) {
    x
  }
  if (!is.null(data_hist)) {
    for (i in 1:length(data_hist)) {
      #   if(any(getNames(data_hist[[i]],dim = 1) %in% c("lo","up"))) p <- p + stat_summary(data=as.ggplot(data_hist[[i]][,,c("lo","up")],scenario=names(data_hist)[i]),geom="ribbon", fun.ymin="min", fun.ymax="max", mapping=aes(fill=Scenario),alpha=0.15,show.legend=FALSE)
      #   if(any(getNames(data_hist[[i]],dim = 1) %in% c("lo","up"))) p <- p + stat_summary(data=as.ggplot(data_hist[[i]][,,c("lo","up")],scenario=names(data_hist)[i]),geom="errorbar", fun.ymin="min", fun.ymax="max", mapping=aes(fill=Scenario),alpha=0.15,size=2*linewidth,show.legend=FALSE)
      #     if(any(getNames(data_hist[[i]],dim = 1) %in% c("lo","up"))) {
      #       tmp <- as.ggplot(data_hist[[i]][,,c("lo","up")],scenario=names(data_hist)[i])
      #       tmp <- dcast(tmp, Region + Year + Scenario ~ Data1, value.var="Value")
      #       print(tmp)
      #      p <- p + geom_smooth(data=tmp,mapping=aes(y=lo,ymin=lo,ymax=up,fill=Scenario),stat="identity",colour=NA,size=1,alpha=0.15,show.legend=FALSE)
      #     }
      if(any(getNames(data_hist[[i]],dim = 1) %in% c("lo","up"))) {
        tmp <- data_hist[[i]][,,c("lo","up")]
        y <- getYears(tmp,as.integer = T)
        if(length(y) == 1) {
          tmp <- mbind(setYears(tmp,y-1),tmp,setYears(tmp,y+1))
        }
        p <- p + stat_summary(data=as.ggplot(tmp,scenario=names(data_hist)[i]),geom="ribbon", fun.ymin="min", fun.ymax="max", mapping=aes_string(fill="Scenario"),alpha=0.15,show.legend=FALSE)
      }
      if(any(getNames(data_hist[[i]],dim = 1) == c("data"))) p <- p + geom_line(data=as.ggplot(data_hist[[i]][,,"data"],scenario=names(data_hist)[i]),mapping=aes_string(fill="Scenario"),color=col_hist[i],size=1)
      if(any(getNames(data_hist[[i]],dim = 1) == c("data"))) p <- p + geom_point(data=as.ggplot(data_hist[[i]][,,"data"],scenario=names(data_hist)[i]),mapping=aes_string(fill="Scenario"),color=col_hist[i],shape=1,size=3)
    }
    p <- p + scale_fill_manual(labs[2], values=col_hist[1:length(data_hist)],guide=guide_legend(order=1,ncol=legend_hist_ncol,title.position = "top", byrow = TRUE,override.aes = list(colour=col_hist[1:length(data_hist)],shape=1)))
  }
  p <- p + scale_color_manual(labs[1], values=col_data,guide=guide_legend(order=2,ncol=legend_data_ncol,title.position = "top", byrow = TRUE))
  
  p <- p + theme(legend.box="horizontal",legend.box.just = "top")
  
  if (!is.null(labs)) p  <- p + labs(color=labs[1],fill=labs[2],shape=labs[3],alpha=labs[4],linetype=labs[5])
  if (xaxis_angle == 90) hjust <- 1 else hjust <- 0.5
  p  <- p + labs(y=ylab,x=xlab) + ggtitle(title) + theme(panel.background = element_rect(fill="white", colour="black"),panel.grid.minor=element_line(colour="white"),plot.title=element_text(size=text_size+4,face="bold",vjust=1.5), legend.position="bottom", legend.title=element_text(size=text_size,face="bold"), legend.text=element_text(size=text_size), axis.title.y=element_text(angle=90,size=text_size,face="bold",vjust=1), axis.text.y=element_text(size=text_size), axis.title.x=element_text(size=text_size,face="bold",vjust=-0.3), axis.text.x=element_text(size=text_size, angle=xaxis_angle, vjust=0.5,hjust=hjust)) + scale_alpha_discrete(range=c(0.5, 1))
  if (facet_style == "default") p <- p + theme(strip.text.x=element_text(size=text_size-1), strip.text.y=element_text(size=text_size-1))
  else if (facet_style == "paper") p <- p + theme(strip.text.x=element_text(size=text_size,face="bold"),strip.text.y=element_text(size=text_size,face="bold")) + theme(strip.background = element_blank())
  if(show_grid) p <- p + theme(panel.grid.major=element_line(colour="grey80"),panel.grid.minor=element_line(colour="grey90"))
  labels <- waiver()
  
  #xaxis
  #if (!is.null(xlim)) xlim <- as.Date(paste(xlim,"-01-01",sep=""))
  # if (!is.list(breaks_x)) {
  #   labels <- format(as.Date(paste(breaks_x,"-01-01",sep="")), format = "%Y")    
  #   breaks_x <- as.Date(paste(breaks_x,"-01-01",sep=""))
  # }
  p <- p + scale_x_continuous(limits=xlim,breaks=breaks_x,labels=labels)  
  
  p <- p + geom_vline(xintercept=data_mag$Year[1],linetype=2)
  
  if (!is.null(axis_text_col)) p <- p + theme(axis.text = element_text(colour=axis_text_col),axis.ticks = element_line(colour=axis_text_col))
  return(p)
}