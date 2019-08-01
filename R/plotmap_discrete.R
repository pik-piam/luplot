#' Function to plot maps with a discrete scale using ggplot2
#' 
#' Plots maps of MAgPIE objects
#' 
#' Makes use of ggplot2 map plotting capabilites
#' 
#' @usage plotmap_discrete(data,file=NULL,title="World map",
#' colours="YlOrRd",legend_breaks=NULL,legendname="share",
#' show_percent=TRUE,facet_grid="Year~Data1")
#' @param data MAgPIE object
#' @param file File name the output should be written to using ggsave
#' @param title Title of the map
#' @param colours Determines the colour palette of the map. Can either be the
#' name of a colorbrewer scale (or the corresponding number, see the 'palette'
#' argument in \code{\link[ggplot2]{scale_fill_brewer}}, or a vector of
#' manually determined colours.
#' @param legend_breaks By default (NULL), the breaks are determined
#' automatically from the \code{data} object. Alternatively, manual breaks can
#' be set by providing a vector. Note that if \code{data} is continuous, breaks
#' have to be specified manually.
#' @param legendname Name of the legend
#' @param show_percent If TRUE, the percentage of cells in the corresponding
#' category is shown
#' @param facet_grid facets formula used in ggplot2 with default "Year~Data1"
#' @author Florian Humpenoeder, Markus Bonsch
#' @seealso \code{\link{plotmap}},\code{\link{plotmap2}}
#' @examples
#' #plotmap2(data)
#' @export
#' @importFrom ggplot2 scale_fill_brewer
#' 
plotmap_discrete <- function(data,file=NULL,title="World map",colours="YlOrRd",legend_breaks=NULL,legendname="share",show_percent=TRUE,facet_grid="Year~Data1") {
  #require(ggplot2, quietly = TRUE)
  #require(RColorBrewer, quietly = TRUE)
  if(is.null(attr(data,"coordinates"))){
    attr(data, "coordinates") <- getCoordinates(degree = T)
    warning("No coordinates supplied in MAgPIE object attributes. Added coordinates in default MAgPIE cell order.")
  }
  data <- as.ggplot(data,asDate=F)
  if(is.null(legend_breaks)) {
    data$Breaks<-as.factor(data$Value)
    legend_labels<-levels(data$Breaks)
  } else {
    tmp<-as.vector(data$Value)
    tmp[]<-length(legend_breaks)+1
    legend_labels<-rep("",length(legend_breaks)+1)
    legend_labels[length(legend_breaks)+1]<-paste(">",legend_breaks[length(legend_breaks)])
    for(i in length(legend_breaks):2){
      tmp[which(as.vector(data$Value)<=legend_breaks[i])]<-i
      legend_labels[i]<-paste(legend_breaks[i-1],"-",legend_breaks[i])
    }
    tmp[which(as.vector(data$Value)<legend_breaks[1])]<-1
    tmp[which(is.na(as.vector(data$Value)))]<-NA
    legend_labels[1]<-paste("<",legend_breaks[1])
    data$Breaks<-as.factor(tmp)
    legend_labels<-legend_labels[as.numeric(levels(data$Breaks))]
    
  }
  
  if(show_percent){
    tmp<-round(table(data$Breaks)/dim(data)[1]*100,1)
    percent<-paste("(",tmp,"%)",sep="")
    legend_labels<-paste(legend_labels,percent)
  }

  map <- ggplot(data,aes_string("x","y",colour="Breaks")) + 
    geom_raster(aes_string(fill="Breaks")) +
    coord_cartesian() +
    theme(aspect.ratio = 0.5, plot.title=element_text(vjust=1.5,face="bold")) +
    theme(panel.background = element_rect(fill = "lightsteelblue2")) +
    ggtitle(title) +
    scale_x_continuous(breaks=NULL) +
    scale_y_continuous(breaks=NULL) +
    labs(y=NULL,x=NULL)
#   #Determine the colours
  if(length(colours)==1){
    map<-map+scale_fill_brewer(name=legendname,labels=legend_labels,palette=colours,na.value="grey")
  } else {
    map<-map+scale_fill_manual(name=legendname,values=colours,labels=legend_labels,na.value="grey")
  }
  #facet_grid
  if(!is.null(facet_grid)) map<-map+facet_grid(facet_grid)
  if(!is.null(file)) {
    ggsave(file,map)
  } else {
    return(map)  
  }
}