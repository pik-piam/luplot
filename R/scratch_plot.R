#' scratch_plot
#' 
#' Function for a quick-and-dirty visualisation of a regional MAgPIE object.
#' 
#' 
#' @usage
#' scratch_plot(plotdata,height=NULL,main=NULL,add=FALSE,ylab=NULL,
#' large=FALSE,pch=16,lty=rep(1:6,20),legend=TRUE,region_plotorder=NULL)
#' @param plotdata An MAgPIE object to be visualized
#' @param height maximum height of the diagramm
#' @param main Title appering at the head of the page
#' @param ylab y-axis text
#' @param add "region_name" makes an area-chart out of the MAgPIE object, with
#' the value of regions and names being plotted on top of each other.  "region"
#' makes an area-chart out of the MAgPIE object, with the value of only regions
#' being plotted on top of each other, while the name-slices are plotted in
#' front of each other with a transparency of 50 percent.
#' @param large TRUE: font-size for postdocs :P
#' @param lty vector of line-types, see par, only used for line-charts
#' @param pch vector of dot-types, see par, only used for line-charts
#' @param legend legend can be switched on or of
#' @param region_plotorder reordering of regions, see \link{aggregate_for_plot}
#' @return Aim of this function is to create a fast visualisation of an MAgPIE
#' object.
#' @author Benjamin Bodirsky
#' @seealso
#' \code{\link{comparisonplot},\link{histoplot},\link{get_layout_parameters},\link{scratch_legend}}
#' @examples
#' \dontrun{
#'   data(population_magpie)
#'   scratch_plot(population_magpie,main="Population Scenarios",ylab="Million People")
#'   scratch_plot(population_magpie,main="Population Scenarios",ylab="Million People", 
#'   add="region_name")
#'   scratch_plot(population_magpie,main="Population Scenarios",ylab="Million People",
#'    add="region_name",large=TRUE)  
#'   scratch_plot(population_magpie,main="Population Scenarios",ylab="Million People",
#'   add="region")    
#' }
#' @export
scratch_plot<-function(plotdata,height=NULL,main=NULL,add=FALSE,ylab=NULL,large=FALSE,pch=16,lty=rep(1:6,20),legend=TRUE,region_plotorder=NULL) {
#plotdata<-compare_data
# height<-NULL
# main<-NULL
# add<-"region"
# ylab=NULL
# large=FALSE  
  if(add==TRUE){add="region_name"} #to make it downwards compatible
  if(!is.magpie(plotdata)){warning("plotdata has to be a magpie object")}
  if(magpieResolution(plotdata)=="cell") {stop("Cellular datasets are not supported")} 
  if (add!=FALSE) {plotdata<-aggregate_for_plot(plotdata, dimension=add,region_plotorder=region_plotorder)}  
  if (is.null(ylab)) {ylab=dimnames(plotdata)[[3]][1]}
  if (is.null(height)) {height<-max(plotdata,na.rm = TRUE)} 
  if ((length(height))==1) {height<-c(min(plotdata,na.rm = TRUE),height)}
  nregions<-dim(plotdata)[1]
  nNames<-dim(plotdata)[3]
  color<-nice_colors(style="contrast_area",saturation=1)  
  color_fill<-nice_colors(style="contrast_area",saturation=1,value=0.8) 
  color_fill_alpha<-nice_colors(style="contrast_area",saturation=1,alpha=0.5)   
  hatching_color<-nice_colors(style="contrast_area",saturation=0.9)  
  hatching_density<- 0:(length(getNames(plotdata))-1)*5
  hatching_angle<- (1:length(getNames(plotdata)))*-35
   
  years_int<-getYears(plotdata,as.integer=TRUE)
  anyisna<-function(x){any(is.na(x))}
  if(length(pch)==1){pch<-rep(pch,dim(plotdata)[3])}
  if(length(lty)==1){lty<-rep(lty,dim(plotdata)[3])}

  if (large==TRUE){
    par(oma=c(0,2,1,8))
    plot(plotdata,type="n",ylim=height,xlim=c(min(years_int),max(years_int)),xaxt="n",xlab="year",ylab=ylab,cex.lab=2,cex.axis=1.5)     
    axis(1,at=years_int,labels=getYears(plotdata,as.integer=TRUE),cex.axis=1.5)
    title(main,cex.main=3)
  } else { 
#    par(oma=c(0,0,3,0))
    plot(plotdata,type="n",ylim=height,xlim=c(min(years_int),max(years_int)),xaxt="n",xlab="year",ylab=ylab,cex.lab=1.2)       
    axis(1,at=years_int,labels=getYears(plotdata,as.integer=TRUE))
    mtext(text=main,side=3,outer=TRUE,cex=1.5) 
  }
  
  if (add==FALSE) {
    for(region in length(getRegions(plotdata)):1) {
      for (name in 1:length(getNames(plotdata))) {
        defined_time<-which(is.na(plotdata[region,,name])!=TRUE) 
        if(any(is.na(plotdata[region,defined_time,name]))) {stop("NAs within a timeline")}
        if(is.na(lty[name])==FALSE){
          lines(y=plotdata[region,defined_time,name],x=getYears(plotdata[region,defined_time,name],as.integer=TRUE),col=color[region],lwd=2,lty=lty[name])       
        }
        if(is.na(pch[name])==FALSE){
          points(y=plotdata[region,defined_time,name],x=getYears(plotdata[region,defined_time,name],as.integer=TRUE),col=color[region],lwd=2,pch=pch[name],bg="white")
        }
      } 
    }
  } else if (add=="region_name"){
    y_old<-c(rep(0,nyears(plotdata)))
    for(region in 1:length(getRegions(plotdata))) {
      for (name in 1:length(getNames(plotdata))) {
        x<-c(years_int,rev(years_int))
        y<-c(plotdata[region,,name],rev(y_old))
        polygon(x,y,col=color_fill[region],border=NA)
        polygon(x,y,col=hatching_color[region],density=hatching_density[name], angle=hatching_angle[name],lwd=3,lty=1)    
        y_old<-as.vector(plotdata[region,,name])           
      }
    }
    for(year in (1:nyears(plotdata))) {
     lines(c(years_int[year],years_int[year]),c(0,min(c(max(plotdata[,year,]),height[2]))),col="#332c29",lty=2)      
    }
    for(region in length(getRegions(plotdata)):1) {
      lines(y=plotdata[region,,length(getNames(plotdata))],x=getYears(plotdata,as.integer=TRUE),col=color[region],lwd=3,lty=1)       
    }  
  } else if (add=="region") {
    for (name in 1:length(getNames(plotdata))) {
      defined_time<-which(apply(plotdata[,,name],2,anyisna)!=TRUE) 
      if(any(is.na(plotdata[,defined_time,name]))) {stop("NAs within a timeline")} 
      y_old<-c(rep(0,nyears(plotdata[,defined_time,])))
      for(region in 1:length(getRegions(plotdata))) {     
        x<-c(years_int[defined_time],rev(years_int[defined_time]))
        y<-c(plotdata[region,defined_time,name],rev(y_old))
        polygon(x,y,col=color_fill_alpha[region],border=NA)
        polygon(x,y,col=hatching_color[region],density=hatching_density[name], angle=hatching_angle[name],lwd=3,lty=1)    
        lines(y=plotdata[region,,length(getNames(plotdata))],x=getYears(plotdata,as.integer=TRUE),col=color[region],lwd=3,lty=1)  
        y_old<-as.vector(plotdata[region,defined_time,name])         
      }
      for(region in length(getRegions(plotdata)):1) {
        lines(y=plotdata[region,defined_time,name],x=getYears(plotdata[region,defined_time,name],as.integer=TRUE),col=color[region],lwd=3,lty=1)       
      } 
    }
    for(year in (1:nyears(plotdata))) {
     lines(c(years_int[year],years_int[year]),c(0,min(c(max(plotdata[,year,],na.rm=TRUE),height[2]))),col="#332c29",lty=2)      
    }    
  
  } 
  if(legend==TRUE) {
    scratch_legend(plotdata=plotdata, large=large, color=color, hatching_density=hatching_density, hatching_angle=hatching_angle, lty=lty, pch=NULL,add=add)
  }
}