#' gg.reg.x.crop
#' 
#' Creates a facet plot with regions as columns and production activities as
#' rows.
#' 
#' Requires a MAgPIE object(s). Ideally with regional dimension. If there are
#' more than one objects, they should have the same dimension. In the case of
#' plotting more than 8 \code{MAgPIE} objects, it is necessary to define new
#' color palette (see \code{palette} argument description).  Use the
#' \code{MAgPIE} dimensionality to specify which regions and production
#' activities you want to plot. For the latter, the optimal number is 6
#' activities per plot for (landscape) printing on a standard A4 paper format.
#' 
#' @usage gg.reg.x.crop(..., ylab="", legend_name="", legend_labels=NULL,
#' legend_position="bottom", plot_title=NULL, palette=NULL, y_scales="fixed",
#' y_scale_type="log10", y_breaks=c(0.1,1,10), h_line=NULL, h_color="black",
#' h_type="dashed")
#' @param ... Requires a MAgPIE object(s). Ideally with regional dimension. If
#' there are more than one objects, they should have the same dimension. In the
#' case of plotting more than 8 \code{MAgPIE} objects, it is necessary to
#' define new color palette (see \code{palette} argument description).  Use the
#' \code{MAgPIE} dimensionality to specify which regions and production
#' activities you want to plot. For the latter, the optimal number is 6
#' activities per plot for (landscape) printing on a standard A4 paper.
#' @param ylab A string for the y-axis label name to be printed on the plot.
#' Default value is an empty string, i.e. no label is printed on the plot.
#' @param legend_name A string for the legend title to be printed on the plot.
#' Default value is an empty string, i.e. no title is printed on the plot.
#' @param legend_labels A vector of names for the legend labels. If nothing is
#' specified, the legend labels will be just the names of \code{MAgPIE} objects
#' that are given as the first arguments. See the example for the specific
#' usage.
#' @param legend_position A legend position on the plot. Default is "bottom".
#' Other options are "top", "right" and "left".
#' @param plot_title A string for the main title of the plot. Default value is
#' \code{NULL} which puts no title on the plot.
#' @param palette A vector of specific colors for the plot lines. If skipped,
#' the predefined palette will be used. At the moment, 8 colors are defined in
#' the palette, which means no more than 8 \code{MAgPIE} objects can be used
#' for plotting.
#' @param y_scales Decides if the y-axis scale is "fixed" (default) or "free"
#' for all the separate plots.
#' @param y_scale_type Defines the scaling type of the y-axis. Default type is
#' \code{log10}. Another option is \code{sqrt} for the square root scaling. For
#' the full scale y-axis, type anything.
#' @param y_breaks Break points on the y-scale (ticks). Default is \code{c(0.1,
#' 1, 10)}.
#' @param h_line Puts a horizontal line to the plot. Default value is
#' \code{NULL}. It accepts a number as an argument which is the intercept on
#' the y-axis.
#' @param h_color A color for the horizontal line. The default color is
#' \code{"black"}.
#' @param h_type A line type for the horizontal line. The default type is
#' \code{"dashed"}.
#' @author Miodrag Stevanovic
#' @examples
#' 
#'   \dontrun{
#'   gg.reg.x.crop(magpie.reg.prod, magpie.supply, magpie.export,
#'    legend_labels=c("Regional\nProduction",
#'   "Supply","Export"), plot_title="Plot", palette=c("yellow","red","green"))
#'   
#'   gg.reg.x.crop(self.suff("../Some_magpie_run_in_the_output_folder/fulldata.gdx")
#'   , palette="magenta", 
#'   legend_labels="Self-Sufficiency\nRatios", y_breaks=c(0,1,10))	
#'   
#'   You can specify your magpie object as much as possible. If no scaling
#'   of y-axis is required, type for 
#'   example an empty string.
#'   gg.reg.x.crop(magpie_reg_prod[AFR,c(1:4),c("tece","begr")], y_scale_type="")
#'   } 
#' @export
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes_ scale_y_log10 geom_hline geom_line geom_point facet_grid theme element_text element_rect ggtitle scale_color_manual xlab scale_y_sqrt
#' @importFrom dplyr bind_rows

gg.reg.x.crop <- function(..., ylab="", legend_name="", legend_labels=NULL, legend_position="bottom", 
                          plot_title=NULL, palette=NULL, y_scales="fixed", y_scale_type="log10", 
                          y_breaks=c(0.1,1,10), h_line=NULL, h_color="black", h_type="dashed"){
 
  #require("dplyr", quietly = TRUE)
  #require("reshape2", quietly = TRUE)
  #require("ggplot2", quietly = TRUE) 
  
	.e <- environment()	
	
  var_names <- sapply(substitute(list(...))[-1], deparse)
	dat <- list(...)
  names(dat) <- var_names
  nr_objects <- length(dat)

  # creates data frame out of list elements
  dat <- lapply(dat,as.data.frame)
  

  # gives the right name for the variables (instead of "Freq", or "Value" in the case of magpie::as.data.frame)
  for(i in 1:nr_objects){
    names(dat[[i]])[which(names(dat[[i]])=="Value")] <- names(dat)[i]
  }  
  
  # melt the data frames in the list
  dat <- lapply(dat, melt, id=c("Cell","Region","Year","Data1"))
  
  # merge the data frames into one data frame
  dat <- bind_rows(dat)
  
  # default colour palette
  if(is.null(palette)){
    Palette <- c("royalblue3", "red", "green3", "black", "goldenrod1", "gray44", "hotpink3", "springgreen3")
  } else Palette <- palette
    
  m <- ggplot(dat, aes_(~Year, ~value, group=~variable, colour=~variable), environment = .e)
	
	# add a line to a graph
	if(is.numeric(h_line)) m <- m + geom_hline(aes_(yintercept=~h_line), colour=~h_color, size=0.3,  
                                             linetype=h_type)
	
	m <- m + geom_line(size=0.4)
	m <- m + geom_point(size=1)
	m <- m + facet_grid(Data1~Region, scales=y_scales)
  
	m <- m + theme(axis.text.x = element_text(size = 12*0.5, angle=90, lineheight = 0.9, colour = "grey50", vjust = 1))
	m <- m + theme(axis.text.y = element_text(size = 12*0.7, lineheight = 0.9, colour = "grey50", vjust = 1))
	m <- m + theme(panel.border = element_rect(fill = NA, colour="grey50"))
  m <- m + theme(legend.position = legend_position)
  if(!is.null(plot_title)) m <- m + ggtitle(plot_title)
  
  # legend
	if(!is.null(legend_labels)) m <- m + scale_color_manual(name=legend_name,values=Palette, labels=legend_labels)
	else m <- m + scale_color_manual(name=legend_name, values=Palette)
	
	m <- m + xlab("Time")
	m <- m + ylab(ylab)
  
  # y-axis scale options
	if(y_scale_type == "log10") m <- m + scale_y_log10(breaks=y_breaks)
  else if(y_scale_type == "sqrt") m <- m + scale_y_sqrt(breaks=y_breaks)
  
	print(m) 
}
