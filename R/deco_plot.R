#' @importFrom grDevices gray.colors
#' @importFrom ggplot2 ggplot aes geom_bar geom_point ggplot labs scale_fill_manual
#' @importFrom quitte as.quitte
#' 
#' 

# Plot for decomposition analysis
# use function deco in library magpie4 to create the necessary object.
deco_plot<-function(x){
  color_vec=gray.colors(6)
  legend_names<-getNames(x)
  x<-x[,,rev(legend_names)]
  color_vec<-rev(color_vec)[1:length(legend_names)]
  
  x1<- as.quitte(x[,,2:length(getNames(x))])
  x2<- as.quitte(x[,,1])
  class(x1) <- setdiff(class(x1),"data.table")
  class(x2) <- setdiff(class(x2),"data.table")
  
  p <- ggplot()
  p <- p + geom_bar(data=x1,size=1,stat="identity",position="stack",aes_string(x="period",y="value",fill="data1")) 
  p <- p + geom_point(data=x2,aes_string(x="period",y="value"))
  
  p <- p + scale_fill_manual(values = color_vec) 
  #  p <- p + scale_y_continuous(limits = c(0,y_limit), labels=NULL,expand = c(0, 0)) + theme(axis.ticks.y = element_blank(),axis.text.x  = element_text(angle=90,size=8))
  p <- p + labs(y = "percent/timestep", x = "year") 
  return(p)
}