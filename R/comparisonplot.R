#' comparisonplot
#' 
#' Compares two Magpie objects of the same structure with a barplot
#' 
#' 
#' @usage comparisonplot(measured_data, magpie_data, compare_categories=FALSE,
#' main="Comparison", ylab="", design="horizontal",
#' output_folder="",legend.text=c("Measured Data","Magpie Output"),
#' min_height=NULL, max_height=NULL)
#' @param measured_data An MAgPIE object
#' @param magpie_data Another MAgPIE object
#' @param compare_categories With FALSE each name-column of both datasets is
#' compared in a seperate plot, and each bar represents a region. With TRUE,
#' each region is plotted in a seperate plot, while each bar represents a
#' name-column.
#' @param main Title appering at the head of the page
#' @param ylab a title for the y axis
#' @param design see get_layout_parameters() for further details
#' @param output_folder Outputfolder of pdf file
#' @param legend.text Legende for the description of the different datasets
#' @param min_height Minimum Height of the y-axis. If NULL, heighest value is
#' taken.
#' @param max_height Maximum Height of the y-axis. If NULL, zero is taken.
#' @return Compares to sets of historical data in the form of MAgPIE objects.
#' Dimensions are time and value. Regions and the name of the data columns have
#' to be the same if data shall be compared.
#' @author Benjamin Bodirsky
#' @seealso
#' \code{\link{histoplot},\link{scratch_plot},\link{get_layout_parameters}}
#' @examples
#' \dontrun{
#' data(population_magpie)
#'  #Comparison requires two objects with same dimension names!
#' # Suppose the A2 dataset would be Magpie-Output, while the B1 would be the real data
#' magpie_output<-population_magpie[,1:4,1]
#' dimnames(magpie_output)[[3]]<-c("Population")
#' real_data<-population_magpie[,1:3,2]
#' dimnames(real_data)[[3]]<-c("Population")
#' # Note that only the timeslots where plotted where information on both datasets existed!
#' comparisonplot(measured_data=real_data,magpie_data=magpie_output,compare_categories=FALSE)
#' }
#' @export
#' @importFrom magclass is.magpie getRegions getYears
#' @importFrom graphics barplot layout
#' @importFrom grDevices dev.off pdf

comparisonplot<-function(measured_data, magpie_data, compare_categories=FALSE, main="Comparison", ylab="", design="horizontal", output_folder="",legend.text=c("Measured Data","Magpie Output"), min_height=NULL, max_height=NULL)
{

#  data(population_magpie)
#  magpie_data<-population_magpie[,1:4,1]
#  dimnames(magpie_output)[[3]]<-c("Population")
#  measured_data<-population_magpie[,1:3,2]
#  dimnames(real_data)[[3]]<-c("Population")
#  main="Comparison" 
#  ylab="Tg N" 
#  legend.text=c("Measured Data","Magpie Output")
#  min_height=NULL 
#  max_height=NULL 
#  design="vertical" 
#  output_folder=""
#  compare_categories=FALSE

  if(!is.magpie(measured_data)){stop("Invalid Data format of measured data. Has to be a MAgPIE-object.")}
  if(!is.magpie(magpie_data)){stop("Invalid Data format of MAgPIE data. Has to be a MAgPIE-object.")}
  if(all(getRegions(measured_data)!= getRegions(magpie_data))){stop("Data of MAgPIE and measured Data is on different regional aggregation!")}
  
  years_in_both_datasets<-getYears(measured_data)[getYears(measured_data) %in% getYears(magpie_data)]
  category_names_in_both_datasets<-getNames(measured_data)[getNames(measured_data) %in% getNames(magpie_data)]
  if(is.null(years_in_both_datasets)){stop("Years of Both Datasets do not match")}
  if(is.null(category_names_in_both_datasets)){stop("Names of Both Datasets do not match")}
  
  nyears=length(years_in_both_datasets)
  ncategories=length(category_names_in_both_datasets)
   
  layout_parameters<-get_layout_parameters(nyears*ncategories,ncol=ceiling(nyears/2),design=design)
  
  if(compare_categories==FALSE)
  {
    for(category_name in category_names_in_both_datasets) 
    { 
      folder_and_file<-paste(output_folder,main,category_name,".pdf",sep="")
      pdf(file=folder_and_file,width=layout_parameters$page_width,height=layout_parameters$page_height)
      layout(layout_parameters[[1]])  
      for(year in years_in_both_datasets) 
      {
        plotted_data<-matrix(c(measured_data[,year,category_name],magpie_data[,year,category_name]),nrow=2,byrow=TRUE) 
        if (is.null(max_height))  { max_height=max(plotted_data)}
        if (is.null(min_height))  { min_height=min(plotted_data)}    
        if (main=="Comparison"){titletext<-paste(main,category_name,year,sep=" ")} else {titletext<-main}              
        barplot(height=plotted_data,
          beside = TRUE, 
          main=titletext,
          names.arg=getRegions(magpie_data),
          legend.text=legend.text,
          col=c("#81aeb3",col="#b21e50"),
          ylab=ylab
          )
      }                                                            
      dev.off()
    }
  } else 
  {
  
    for(region in getRegions(measured_data)) 
    {                        
      folder_and_file<-paste(output_folder,main,region,".pdf",sep="")
      pdf(file=folder_and_file,width=layout_parameters$page_width,height=layout_parameters$page_height)
      layout(layout_parameters[[1]])  
      for(year in years_in_both_datasets) 
      {
        plotted_data<-matrix(c(measured_data[region,year,category_names_in_both_datasets],magpie_data[region,year,category_names_in_both_datasets]),nrow=2,byrow=TRUE) 
        if (is.null(max_height))  { max_height=max(plotted_data)}
        if (is.null(min_height))  { min_height=min(plotted_data)}           
        if (main=="Comparison"){titletext<-paste(main,region,year,sep=" ")} else {titletext<-main}              
        barplot(height=plotted_data,
          beside = TRUE, 
          main=titletext,
          names.arg=category_names_in_both_datasets,
          legend.text=legend.text,
          col=c("#81aeb3",col="#b21e50"),
          ylim=c(min_height,max_height),
          ylab=ylab
          )
      }                                  
      dev.off()
    }  
      
  }
  
}
