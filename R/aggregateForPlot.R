#' aggregateForPlot
#' 
#' Aggregates Data for an area chart. The value of all underlying
#' regions/years/timesteps are added to the value of the current
#' region/year/timestep.
#' 
#' 
#' @usage aggregateForPlot(plotdata,
#' dimension="region",regionPlotorder=NULL)
#' @param plotdata An MAgPIE object
#' @param dimension by "region", "name" or "year"
#' @param regionPlotorder allows to change the order of the regions by giving
#' a vector with the order
#' @return A MAgPIE object of the same dimensions as plotdata
#' @author Benjamin Bodirsky
#' @examples
#' \dontrun{
#' data(population_magpie)
#' #no aggregation
#' scratch_plot(population_magpie[,,2])
#' #aggregation
#' scratch_plot(aggregateForPlot(population_magpie[,,2],regionPlotorder=c(10,1:9)))
#' }
#' @export
#' @importFrom magclass getRegions setCells getCells getNames<- getYears getYears<- getRegions<- getNames

aggregateForPlot<-function(plotdata, dimension="region", regionPlotorder=NULL)
{
#dimension="region"
#data(population_magpie)
#plotdata<-population_magpie
#plot_order=NULL      
#regionPlotorder=c(1,3,4,5,6,7,8,9,10,2)           
  if (is.null(regionPlotorder)) {
    regionPlotorder=1:dim(plotdata)[1]
  } else if(length(regionPlotorder)!=dim(plotdata)[1]){stop("plotorder has to be vector with the length of dim(plotdata)[1]")}
  if(magpieResolution(plotdata)=="cell") {stop("Cellular datasets are not supported")} 
  if (dimension=="region") {
    aggregateplotdata<-plotdata[1,,]*0 
    plotdata_out<-plotdata*0
    dimnames(plotdata_out)[[1]]<-paste(getRegions(plotdata)[regionPlotorder],1:dim(plotdata)[1],sep=".")
    for (region_dimension in getRegions(plotdata)[regionPlotorder]) {
      plotdata_out[region_dimension,,] <- setCells(aggregateplotdata,getCells(plotdata[region_dimension,,])) + plotdata[region_dimension,,]
      aggregateplotdata<-plotdata_out[region_dimension,,]
    }
  } else if (dimension=="name") {
    aggregateplotdata<-plotdata[,,1]*0 
    for (name_dimension in getNames(plotdata)) {
      getNames(aggregateForPlot) <- name_dimension
      plotdata_out[,,name_dimension] <- aggregateplotdata + plotdata[,,name_dimension]
      aggregateplotdata<-plotdata_out[,,name_dimension]
    }
  } else if (dimension=="year") {
    aggregateplotdata<-plotdata[,1,]*0 
    for (year_dimension in getYears(plotdata)) {
      getYears(aggregateplotdata) <- year_dimension
      plotdata_out[,year_dimension,] <- aggregateplotdata + plotdata[,year_dimension,]
      aggregateplotdata<-plotdata_out[,year_dimension,]
    }
  } else if (dimension=="region_name") {
    aggregateplotdata<-plotdata[1,,1]*0 
    plotdata_out<-plotdata*0
    dimnames(plotdata_out)[[1]]<-paste(getRegions(plotdata)[regionPlotorder],1:dim(plotdata)[1],sep=".")    
    for (region_dimension in getRegions(plotdata)[regionPlotorder]) {
      getRegions(aggregateplotdata) <- region_dimension
      for (name_dimension in getNames(plotdata)) {
        plotdata_out[region_dimension,,name_dimension] <- setCells(aggregateplotdata,getCells(plotdata[region_dimension,,])) + plotdata[region_dimension,,name_dimension]
        aggregateplotdata<-plotdata_out[region_dimension,,name_dimension]
      }
    }
  } else {
    stop("aggregation_type not known")
  }
  return(plotdata_out)
}