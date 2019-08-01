

#' @title mipAreaOverlapFirstNameFacetSecondNameStacked
#' @description Plots an values in an area plot over time with scenarios in overlapping layers, the first name dimension as facet and the second name dimension stacked. Only datasets with not more than one region are supported.
#'
#' @param x A magpie or quitte object, or a list of magpie or quitte objects. 
#' @param alpha alpha for each layer
#' @return Stacked area plot
#' @author Benjamin Leon Bodirsky
#' @export
#' @examples
#' 
#' \dontrun{ 
#' library(moinput)
#' countries<-c("USA")
#' tmp<-collapseNames(calcOutput("FAOmassbalance",aggregate=FALSE)[,,"dm"])
#' tmp<-tmp[countries,,c("cassav_sp","potato")][,,c("food","feed")]
#' mipAreaOverlapFirstNameFacetSecondNameStacked(x=tmp)
#' 
#' }
#' @importFrom magclass fulldim
#' @importFrom ggplot2 theme_bw aes_ annotation_raster facet_grid
#' @importFrom grDevices hcl
#' @importFrom quitte as.quitte


mipAreaOverlapFirstNameFacetSecondNameStacked<-function(x,alpha=0.5){

  x.plot <- as.quitte(x)
  
  p <- ggplot(data=x.plot) + theme_bw()
  mins<-as.vector(by(x.plot$period,INDICES = paste(x.plot$scenario,x.plot$model),FUN=min,na.rm=T,simplify = TRUE))
  maxs<-as.vector(by(x.plot$period,INDICES = paste(x.plot$scenario,x.plot$model),FUN=max,na.rm=T,simplify = TRUE))
  backcolors<-c("red","blue")
  for(i in 1:length(mins)){
    p<-p+annotation_raster(raster=hcl(h=240,l = 90,c = 20,alpha = 0.3), mins[i], maxs[i], -Inf, Inf)
  }
  
  levels(x.plot$scenario)=c(levels(x.plot$scenario),"not defined")
  x.plot$scenario[which(is.na(x.plot$scenario))]<-factor("not defined")
  
  levels(x.plot$model)=c(levels(x.plot$model),"not defined")
  x.plot$model[which(is.na(x.plot$model))]<-"not defined"
  
  if(length(levels(x.plot$region))>1){warning("The regional dimension should only have one entry.")}
  
  counter=0
  for (i in unique(x.plot$scenario)) {
    selection1<-x.plot[which(x.plot$scenario==i),]
    for(j in unique(x.plot$model)) {
      counter=counter+1

      if(length(alpha)>=counter){alpha_now<-alpha[counter]}
      selection2<-selection1[which(selection1$model==j),]
      if (any(selection2$value >= 0,na.rm=TRUE)) {
        x_pos <- selection2
        x_pos$value[selection2$value<0] <- 0
        p <- p + geom_area(data=x_pos,
                           aes_(x=~period,
                               y = ~value,
                               fill=~data2),
                           stat = "identity",colour="black",position="stack",alpha=alpha_now,linetype=1)+ facet_grid(~ data1)
      }
      if (any(selection2$value <  0,na.rm=TRUE)) {
        x_neg <- selection2      
        x_neg$value[selection2$value>=0] <- 0
        p <- p + geom_area(data=x_neg,
                           aes_(x=~period,
                               y = ~value,
                               fill=~data2),
                           stat = "identity",colour="black",position = "stack",alpha=alpha_now,linetype=1)+ facet_grid(~ data1)
      }
    }
  }
  
  return(p)
}
