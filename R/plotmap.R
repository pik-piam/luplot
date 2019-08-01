#' Function to plot maps
#' 
#' Plots maps of map objects or MAgPIE objects
#' 
#' This function is the successor of the magpie_mapping toolset. In the
#' conversion process some functionality was removed whereas other
#' functionality was added, therefore the new function is not fully compatible
#' to the old implementation
#' 
#' @usage plotmap(x, map_limits_x = c(-180, 180), map_limits_y = c(-60, 90),
#' legend_discrete = FALSE, legend_range = c(0, 1), legend_colours =
#' terrain.colors(1000)[1000:1], legend_fontsize = 2, legend_title = "Legend",
#' legend_factor = 1, legend_show_first_break = TRUE, legend_xy = c(-155, -10),
#' legend_margin = 10, legend_horizontal = FALSE, legend_width =
#' 3,legend_outliers = c(NULL,NULL), main = NULL, title_fontsize = 3, texts =
#' NULL, cex.corr = 0.5, show_world_contours = TRUE, show_country_contours =
#' TRUE, contours_width = 1, contours_resolution = 1, show_tropic = FALSE,
#' show_water = TRUE, water_color = "#ADD8E6")
#' @param x map or MAgPIE object
#' @param map_limits_x x range in Degree of the map that should be plotted
#' @param map_limits_y y range in Degree of the map that should be plotted
#' @param legend_discrete TRUE -> discrete legend, FALSE -> continuous legend
#' @param legend_range Vector containing lower and upper bound of values that
#' should be plotted
#' @param legend_colours A vector of colors that should be used for plotting
#' @param legend_fontsize (only discrete) legend font size
#' @param legend_title (only discrete) legend title
#' @param legend_factor (only discrete) correction factor for legend values
#' @param legend_show_first_break (only discrete) should the first break
#' printed in the legend?
#' @param legend_xy (only discrete) coordinates of the legend on map
#' @param legend_margin (only continuous) legend margins
#' @param legend_horizontal (only continuous) If TRUE the legend is printed is
#' on the right side, otherwise below the map
#' @param legend_width (only continuous) legend width
#' @param legend_outliers colors for values <= and >= the legend-range. if
#' null, legend_range[1] and legend_range[2] are used for outliers.
#' @param main map title
#' @param title_fontsize title font size
#' @param texts A list of 4n vectors of the structure c(x,y,text,font size)
#' with coordinates x and y, a text and a font size, e.g. c(0,0,blablub,0.5)
#' @param cex.corr General correction of all cex settings
#' @param show_world_contours Switch to show continent contours
#' @param show_country_contours Switch to show country contours
#' @param contours_width contours width
#' @param contours_resolution Detailedness of contours. 1 means that any
#' available data point is printed, 2 that every second is printed and so on.
#' @param show_tropic Switch to show tropics
#' @param show_water Switch to show water
#' @param water_color Water color
#' @importFrom graphics abline legend
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{magpie2map}}
#' @examples
#' #plotmap(x)
#' @export
#'@importFrom magclass is.magpie
#'@importFrom fields image.plot
#'@importFrom graphics title image
#'@importFrom grDevices terrain.colors
#'@importFrom utils data
#'@importFrom sp plot
#'

plotmap <- function(x,
                    map_limits_x = c(-180,180),
                    map_limits_y = c(-60,90),
                    legend_discrete = FALSE,
                    legend_range = c(0,1),
                    legend_colours = terrain.colors(1000)[1000:1],
                    legend_fontsize = 2,
                    legend_title = "Legend",
                    legend_factor = 1,
                    legend_show_first_break = TRUE,
                    legend_xy = c(-155,-10),
                    legend_margin = 10,
                    legend_horizontal = FALSE,
                    legend_width = 3,
                    legend_outliers = c(NULL,NULL),
                    main = NULL,
                    title_fontsize = 3,
                    texts = NULL,
                    cex.corr = 0.5,
                    show_world_contours = TRUE,
                    show_country_contours = TRUE,
                    contours_width = 1,
                    contours_resolution = 1,
                    show_tropic = FALSE,
                    show_water = TRUE,
                    water_color = "#ADD8E6") {
  world_map_water <-  wrld_simpl <- NULL
  data("world", envir = environment(), package="luplot")
   if(is.magpie(x)) x <- magpie2map(x)
   x$z[which(x$z<legend_range[1])]<-legend_range[1]
   x$z[which(x$z>legend_range[length(legend_range)])]<-legend_range[length(legend_range)]
   if(!is.null(legend_outliers[1])){legend_colours<-c(legend_outliers[1],legend_colours)}
   if(!is.null(legend_outliers[2])){legend_colours<-c(legend_colours,legend_outliers[2])}   
   if(legend_show_first_break) {
      legend_level_names = paste(legend_factor*legend_range[1:length(legend_range)-1]," - ", legend_factor*legend_range[2:length(legend_range)], sep="")
    } else {
      legend_level_names = c(legend_factor*legend_range[2],paste(legend_factor*legend_range[3:length(legend_range)-1]," - ", legend_factor*legend_range[3:length(legend_range)], sep=""))
    }
      
    if (legend_discrete) suppressWarnings(image(x,xlim=map_limits_x,ylim=map_limits_y,col=legend_colours,breaks=legend_range,axes=FALSE))
    else suppressWarnings(image.plot(x,zlim=legend_range,xlim=map_limits_x,ylim=map_limits_y,col=legend_colours,axes=FALSE,horizontal=legend_horizontal,legend.width=legend_width*cex.corr,legend.mar=legend_margin,axis.args=list(cex.axis=legend_fontsize*cex.corr)))
    
    if(show_water) {
      .tempfunc <- function (x,water_color) polygon(c(x[1],x[3],x[3],x[1]),c(x[2],x[2],x[4],x[4]),col=water_color,border=NA)
      apply(world_map_water,1,.tempfunc,water_color)
    }
   
		if(show_world_contours){
			plot(wrld_simpl,xlim=map_limits_x,ylim=map_limits_y,add=T,lwd=1.2) 
		}
			
    if(show_tropic) {
	    abline(h=-23.5,lwd=1,lty=2); text(x=-160,y=-26,"-23,5deg",cex=2*cex.corr)
      abline(h= 23.5,lwd=1,lty=2); text(x=-160,y= 26,"+23,5deg",cex=2*cex.corr)
    }
    if(legend_discrete) legend(x=legend_xy[1],y=legend_xy[2],legend=legend_level_names,bg="white",fill=legend_colours,horiz=FALSE,title=legend_title, cex=legend_fontsize*cex.corr)
    if(!is.null(texts)) {
      if(!is.list(texts)) texts <- list(texts)
      for(t in texts)  {print(t);text(x=as.numeric(t[1]),y=as.numeric(t[2]), t[3], cex=as.numeric(t[4]))}
    }
    title(main = main, cex.main=title_fontsize*cex.corr)
}

