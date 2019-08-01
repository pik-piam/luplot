#' scratch_legend
#' 
#' Function to create a nice legend for a plot. It was taken out of
#' scratch_plot so it can be used by other plot functions too.
#' 
#' 
#' @usage scratch_legend(plotdata, large=FALSE, color=NULL,
#' hatching_density=NULL, hatching_angle=NULL, lty=NULL,add=FALSE,pch=NULL)
#' @param plotdata An MAgPIE object to be visualized
#' @param large TRUE: font-size for postdocs :P
#' @param color colors of the different legend items
#' @param hatching_density for area graphs
#' @param hatching_angle for area graphs
#' @param lty vector of line-types, see par, only used for line-charts
#' @param add "region_name" makes an area-chart out of the MAgPIE object, with
#' the value of regions and names being plotted on top of each other.  "region"
#' makes an area-chart out of the MAgPIE object, with the value of only regions
#' being plotted on top of each other, while the name-slices are plotted in
#' front of each other with a transparency of 50 percent.
#' @param pch vector of dot-types, see par, only used for line-charts
#' @importFrom graphics legend
#' @return visualisation of legend
#' @author Benjamin Bodirsky
#' @seealso \code{\link{scratch_plot}}
#' @examples
#' \dontrun{
#' data(population_magpie)
#'   scratch_plot(population_magpie,legend=FALSE)
#'   scratch_legend(population_magpie)
#' }
#' @export
scratch_legend<-function(plotdata, large=FALSE, color=NULL, hatching_density=NULL, hatching_angle=NULL, lty=NULL,add=FALSE,pch=NULL){
  if(!is.null(plotdata)){
    if(is.magpie(plotdata)){
      nNames<-dim(plotdata)[3]
      nregions<-dim(plotdata)[1]            
      if(is.null(color))    {color<-nice_colors(style="contrast_area",saturation=1)}   
      if(is.null(hatching_density)){hatching_density<- 0:(length(getNames(plotdata))-1)*5}
      if(is.null(hatching_angle)){hatching_angle<- (1:length(getNames(plotdata)))*-35}  
      if(is.null(pch))      {pch<-rep(16,dim(plotdata)[3])}
      if(is.null(lty))      {lty<-rep(rep(1:6,20),dim(plotdata)[3])}          
      
    }else{
      stop("Object has to be a magpie object")
    }
  }
  
#  if(is.null(tmp.u)){
    par(xpd=NA)
    tmp.u <- par('usr')
#  }      
  
  if (large==TRUE){
    if(nNames>1) {
      legend(tmp.u[2], tmp.u[3], xjust=0, yjust=0.1, rev(getRegions(plotdata)), col=color[rev(1:nregions)],fill=color[rev(1:nregions)],bty="n",x.intersp=1,cex=1.8)
      if (add!=FALSE) {
        legend(tmp.u[2], tmp.u[4], xjust=0, yjust=0.9, getNames(plotdata), bty="n",x.intersp=1,cex=1.8,fill="black",density=hatching_density,angle=hatching_angle)
      } else {
        legend(tmp.u[2], tmp.u[4], xjust=0, yjust=0.9, getNames(plotdata), lty=lty, pch=pch,lwd=2,bty="n",x.intersp=1,cex=1.8)      
      }
            
    } else {
      legend(tmp.u[2], (tmp.u[3]+tmp.u[4])/2, xjust=0, yjust=0.5, rev(getRegions(plotdata)), col=color[rev(1:nregions)],fill=color[rev(1:nregions)],bty="n",x.intersp=1,cex=2)
    }
    
  } else { 

    if (nregions>7) {              
      first_row<-ceiling(nregions/2)
      if(nNames>1) {
        first_row_names<-ceiling(nNames/2)
        legend(tmp.u[1], tmp.u[4], xjust=0, yjust=-0.5, getRegions(plotdata)[1:first_row], col=color[1:first_row],horiz=TRUE,fill=color[1:first_row],bty="n",x.intersp=0.3,cex=1)
        legend(tmp.u[1], tmp.u[4], xjust=0, yjust=0, getRegions(plotdata)[(first_row+1):nregions], col=color[(first_row+1):nregions],horiz=TRUE,fill=color[(first_row+1):nregions],bty="n",x.intersp=0.3,cex=1)
        if (add!=FALSE) {
          legend(tmp.u[2], tmp.u[4], xjust=1, yjust=0, getNames(plotdata)[1:first_row_names], bty="n",horiz=TRUE,fill="black",density=hatching_density[1:first_row],angle=hatching_angle[1:first_row])    
          legend(tmp.u[2], tmp.u[4], xjust=1, yjust=-0.5, getNames(plotdata)[(first_row_names+1):nNames], bty="n",horiz=TRUE,fill="black",density=hatching_density[(first_row_names+1):nNames],angle=hatching_angle[(first_row_names+1):nNames])
        } else {
          legend(tmp.u[2], tmp.u[4], xjust=1, yjust=0, getNames(plotdata)[1:first_row_names], lty=lty[1:first_row_names],pch=pch[1:first_row_names],lwd=2,bty="n",horiz=TRUE)    
          legend(tmp.u[2], tmp.u[4], xjust=1, yjust=-0.5, getNames(plotdata)[(first_row_names+1):nNames], lty=lty[(first_row_names+1):nNames],pch=pch[(first_row_names+1):nNames],lwd=2,bty="n",horiz=TRUE)
        }
      }else{ 
        legend((tmp.u[1]+tmp.u[2])/2, tmp.u[4], xjust=0.5, yjust=-0.5, getRegions(plotdata)[1:first_row], col=color[1:first_row],horiz=TRUE,fill=color[1:first_row],bty="n",x.intersp=0.3,cex=1)
        legend((tmp.u[1]+tmp.u[2])/2, tmp.u[4], xjust=0.5, yjust=0, getRegions(plotdata)[(first_row+1):nregions], col=color[(first_row+1):nregions],horiz=TRUE,fill=color[(first_row+1):nregions],bty="n",x.intersp=0.3,cex=1)
      }
    } else if (nregions>2) {
      if(nNames>1) {
        legend((tmp.u[1]+tmp.u[2])/2, tmp.u[4], xjust=0.5, yjust=0, getRegions(plotdata), col=color,horiz=TRUE,fill=color,bty="n",x.intersp=0.3,cex=1)        
        if (add!=FALSE) {
          legend((tmp.u[1]+tmp.u[2])/2, tmp.u[4], xjust=0.5, yjust=-0.7, getNames(plotdata), bty="n",fill="black",density=hatching_density,angle=hatching_angle,horiz=TRUE)
        } else {
          legend((tmp.u[1]+tmp.u[2])/2, tmp.u[4], xjust=0.5, yjust=-0.7, getNames(plotdata), lty=lty,lwd=2,bty="n",horiz=TRUE)
        }   
      }else{ 
        legend((tmp.u[1]+tmp.u[2])/2, tmp.u[4], xjust=0.5, yjust=0, getRegions(plotdata), col=color,horiz=TRUE,fill=color,bty="n",x.intersp=0.3,cex=1)
      }
    } else {
     if(nNames>1) {     
        legend((tmp.u[1]+tmp.u[2])/2, tmp.u[4], xjust=0.5, yjust=0, getNames(plotdata), lty=lty,lwd=2,bty="n",horiz=TRUE,col=color[1])   
     }    
    }                  
    
  } 
}