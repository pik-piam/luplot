#' magpie2ggplot
#' 
#' Function for plotting MAgPIE objects with ggplot
#' 
#' 
#' @usage
#' magpie2ggplot(data,ylab="Value",title="",facet_reg=T,shape_data=T,stack=F,
#' fill="Data1",alpha=NULL, legend_data="Data",legend_scenario="Scenario",
#' text_size=12,hline=NULL,legend_position="right",scales="fixed",
#' ncol=5,...)
#' @param data MAgPIE object or list of MAgPIE objects or dataframe of MAgPIE
#' object(s); the name of the list entries is used as scenario name in the
#' legend
#' @param ylab y-axis text
#' @param title title appering at the top of the plot
#' @param facet_reg facets for each region (TRUE) or each MAgPIE object in data
#' (FALSE)
#' @param shape_data shape data (TRUE) or shape scenario (FALSE)
#' @param stack dotted line plot (FALSE) or stacked area plot (TRUE); only used
#' for facet_reg=FALSE.
#' @param fill fill "Region" or "Data1" in stacked plots
#' @param alpha NULL, "Region" or "Data1" in stacked plots
#' @param legend_data title of the legend for the 3rd dimension of the MAgPIE
#' object; only used if the MAgPIE object has more than 3 dimensions
#' @param legend_scenario title of the legend for the scenarios in data
#' @param text_size text size of axis, legend and title
#' @param hline NULL or numeric. Adds a horizontal line at y=hline
#' @param legend_position right (default), left, top, bottom or none
#' @param scales fixed (default), free, free_y or free_x
#' @param ncol Number of columns used in facet_wrap function
#' @param ... Further options passed to as.ggplot
#' @return ggplot2 object representing one or more MAgPIE objects.
#' @author Florian Humpenoeder
#' @seealso \code{\link{scratch_plot},\link{histoplot},}
#' @examples
#' 
#' \dontrun{
#' crop_area_reg <- list()
#' crop_area_reg[["scenario_name"]] <- croparea("fulldata.gdx")
#' magpie2ggplot(crop_area_reg,ylab="10^6 ha",title="Croparea")}  
#' \dontrun{
#' prices <- list()
#' prices[["Scenario1"]] <- prices("Scenario1.gdx",crops=c("tece","maiz"))
#' prices[["Scenario2"]] <- prices("Scenario2.gdx",crops=c("tece","maiz"))
#' magpie2ggplot(prices,ylab="US$/ton DM",title="Agricultural prices",
#' facet_reg=T,legend_title="Commodity")}
#' @export
#' @importFrom ggplot2 ggplot aes_ facet_wrap geom_point geom_line labs guides scale_alpha_discrete facet_grid geom_area aes_string scale_fill_manual scale_color_manual geom_hline ggtitle theme element_text 


magpie2ggplot <- function(data,ylab="Value",title="",facet_reg=T,shape_data=T,stack=F,fill="Data1",alpha=NULL,legend_data="Data",legend_scenario="Scenario",text_size=12,hline=NULL,legend_position="right",scales="fixed",ncol=5,...){
  .e <- environment()
  #require("ggplot2", quietly = TRUE)
  if (!is.data.frame(data)) data <- as.ggplot(data,...)
  data$pn <- factor(sign(data$Value))
  if (facet_reg) {
    if (shape_data) {
      p <- ggplot(data, aes_(x=~Year, y=~Value, color=~Scenario, shape=~Data1, group=~interaction(Cell,Scenario,Data1)), environment=.e) + facet_wrap( ~ Region, ncol=ncol,scales=scales) + geom_point() + geom_line() + labs(shape=legend_data) + labs(color=legend_scenario)
    } else {
      p <- ggplot(data, aes_(x=~Year, y=~Value, color=~Data1, shape=~Scenario, group=~interaction(Cell,Scenario,Data1)), environment=.e) + facet_wrap( ~ Region, ncol=ncol,scales=scales) + geom_point() + geom_line() + labs(color=legend_data) + labs(shape=legend_scenario)
    }
    if (length(unique(data$Data1)) == 1) {
      p <- p + guides(shape=FALSE)
    }
  } else {
    if (stack) {
      if (shape_data) {
        p <- ggplot(data, aes_(x=~Year, y=~Value, group=~interaction(Cell,Region,Data1)), environment=.e) + facet_wrap( ~ Scenario, ncol=ncol, scales=scales) + labs(alpha=legend_data) + labs(fill=legend_scenario) + scale_alpha_discrete(range=c(1, 0.6))
      } else {
        p <- ggplot(data, aes_(x=~Year, y=~Value, group=~interaction(Cell,Region,Scenario)), environment=.e) + facet_grid(Scenario ~ Data1, scales=scales) + labs(fill=legend_scenario)
      }
      if (length(unique(data$Data1)) == 1) {
        p <- p + guides(alpha=FALSE)
      }
      if (any(data$pn == 1)) p <- p + geom_area(data=subset(data,data$pn==1),position='stack',stat='identity',aes_string(fill=fill,alpha=alpha))
      if (any(data$pn == -1)) p <- p + geom_area(data=subset(data,data$pn==-1),position='stack',stat='identity',aes_string(fill=fill,alpha=alpha))
      if (fill == "Region") {
        if (length(unique(data$Region)) == 1) {
          p <- p + scale_fill_manual(values=nice_colors(style="contrast_area",saturation=1)[11])
        } else if (length(unique(data$Region)) == 10) {
          p <- p + scale_fill_manual(values=nice_colors(style="contrast_area",saturation=1)[1:10])
        } else stop("Only global or regional MAgPIE objects are allowed!")
      }
    } else {
      if (shape_data) {
        p <- ggplot(data, aes_(x=~Year, y=~Value, color=~Region, shape=~Data1, group=~interaction(Cell,Region,Data1)), environment=.e) + facet_wrap( ~ Scenario, ncol=ncol, scales=scales) + geom_line() + geom_point() + labs(shape=legend_data) + labs(color=legend_scenario)
      } else {
        p <- ggplot(data, aes_(x=~Year, y=~Value, color=~Region, shape=~Scenario, group=~interaction(Cell,Region,Scenario)), environment=.e) + facet_wrap( ~ Data1, ncol=ncol, scales=scales) + geom_line() + geom_point() + labs(shape=legend_data) + labs(color=legend_scenario)
      }
      if (length(unique(data$Data1)) == 1) {
        p <- p + guides(shape=FALSE)
      }
      if (length(unique(data$Region)) == 1) p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[11])
      else if (length(unique(data$Region)) == 10) p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[1:10])
      else if (length(unique(data$Region)) == 11) p <- p + scale_color_manual(values=nice_colors(style="contrast_area",saturation=1)[1:11])
    }
  }
  if (!is.null(hline)) {
    p <- p + geom_hline(aes_(yintercept=~hline))
  }
  p  <- p + labs(y=ylab) + ggtitle(title) + theme(plot.title=element_text(size=text_size+4,face="bold",vjust=1.5), strip.text.x=element_text(size=text_size), legend.position=legend_position, legend.title=element_text(size=text_size,face="bold"), legend.text=element_text(size=text_size), axis.title.y=element_text(angle=90,size=text_size,face="bold",vjust=0.3), axis.text.y=element_text(size=text_size), axis.title.x=element_text(size=text_size,face="bold",vjust=-0.3), axis.text.x=element_text(size=text_size, angle=90, hjust=1.2))
  return(p)
}