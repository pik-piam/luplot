#' histoplot
#' 
#' Compares two Magpie objects of the same structure on a linechart
#' 
#' 
#' @usage histoplot(measured_data, magpie_data, main="Histoplot", ylab="data",
#' xlim=NULL, ylim=NULL, file_type="normal_device",
#' output_folder="",legend=NULL, lowpass.iter = NULL, cex=1, regions=NULL, ...)
#' @param measured_data A MAgPIE object (will be plotted as dots) or a list of
#' such objects with identical dimnames
#' @param magpie_data Another MAgPIE object with dimnames identical to
#' measured_data or a list of such MAgPIE objects (will be plotted as line).
#' @param main Title appering at the head of the page (either a string or a
#' list of string and quotes - see example)
#' @param ylab a title for the y axis (either a string or a list of string and
#' quotes)
#' @param xlim the x limits (x1, x2) of the plot. Note that x1 > x2 is allowed
#' and leads to a "reversed axis".
#' @param ylim Upper end of the plot scale. If no value given, the highest
#' value in the dataset is used.
#' @param file_type Data can be viewed either on the R-Device or printed as
#' pdf. Use "pdf", "normal device", or "none"
#' @param output_folder Outputfolder of pdf file
#' @param legend Legend for the description of the different lines in the plot
#' @param lowpass.iter Number of iterations a lowpass-filter should be applied
#' on observed data (a averaged line will be plotted in addition to the
#' observation points). lowpass.iter = NULL means, that no line should be
#' plotted.
#' @param cex A numerical value giving the amount by which plotting text and
#' symbols should be magnified relative to the default
#' @param regions Allows to choose the regions that should be plotted
#' @param ... additional parameters, such as design, nrow, or ncol can be
#' forwarded to the function get_layout_parameters
#' @return Compares two sets of historical data in the form of MAgPIE objects.
#' Dimensions are time and value. Regions and the name of the data columns have
#' to be the same if data shall be compared.
#' @author Benjamin Bodirsky, Jan Philipp Dietrich, Michael Krause
#' @seealso
#' \code{\link{comparisonplot},\link{scratch_plot},\link{get_layout_parameters}}
#' @examples
#' \dontrun{
#' data(population_magpie)
#' # Comparison requires two objects with same dimension names!
#' # Suppose the A2 dataset would be Magpie-Output, while the B1 would be the real data
#' magpie_output<-population_magpie[,,1]
#' dimnames(magpie_output)[[3]]<-c("Population")
#' real_data<-population_magpie[,,2]
#' dimnames(real_data)[[3]]<-c("Population")
#'  Note that only the timeslots where plotted where information on both datasets existed!
#' histoplot(measured_data=real_data,magpie_data=magpie_output,ylim=c(0,3000),
#' main=list(quote(alpha)," und ",quote(beta)))
#' }
#' @export
#' @importFrom magclass is.magpie getRegions getYears lowpass
#' @importFrom grDevices dev.new
#' @importFrom graphics par plot text grid lines points mtext

histoplot<-function(measured_data, magpie_data, main="Histoplot", ylab="data", xlim=NULL, ylim=NULL, file_type="normal_device", output_folder="",legend=NULL, lowpass.iter = NULL, cex=1, regions=NULL, ...)
{
  color <- c("red","black","green","brown","blue","yellow","orange","grey")
  ltys  <- c(4,6,5,3,2,4,6,5,3,2)
  lwds  <- c(2,2,2,2,2)
  if(!is.list(magpie_data)) {
    magpie_data <- list(magpie_data)
  } 
  if(!is.list(measured_data)) {
    measured_data <- list(measured_data)
  }
  
  if(!is.magpie(measured_data[[1]])){stop("Invalid Data format of measured data. Has to be a MAgPIE-object.")}
  if(!is.magpie(magpie_data[[1]])){stop("Invalid Data format of MAgPIE data. Has to be a MAgPIE-object.")}
  if(all(getRegions(measured_data[[1]])!= getRegions(magpie_data[[1]]))){stop("Data of MAgPIE and measured Data is on different regional aggregation!")}
  if(is.null(getNames(measured_data[[1]]))){stop("Measured Data has no column names")}
  if(is.null(getNames(magpie_data[[1]]))){stop("Magpie Data has no column names")}
  
  if(is.null(regions)) {
    regions<-getRegions(magpie_data[[1]])
    nregions <- nregions(magpie_data[[1]])
  } else {
    nregions <- length(regions)
  }
  years_magpie<-list()
  for(nmagpie in 1:length(magpie_data)){
    years_magpie[[nmagpie]]   <- getYears(magpie_data[[nmagpie]],as.integer=TRUE)
  }
  years_measured<-list()
  for(nmeasured in 1:length(measured_data)){
    years_measured[[nmeasured]] <- getYears(measured_data[[nmeasured]],as.integer=TRUE)
  }
  
  if (is.null(xlim)) {
    xlim <-c(min(unlist(years_magpie),unlist(years_measured)),max(unlist(years_magpie),unlist(years_measured)))
  }
  calc_ylim <- is.null(ylim)
  
  ngraphics<-length(getNames(measured_data[[1]]))*nregions
  
  lp<-get_layout_parameters(ngraphics,header=TRUE,...)   
  for (data_name in getNames(measured_data[[1]]))
  {
    i <- 0
    for(region in regions) {
      i <- i+1
      if(i%%lp$maxgraphics == 1) { #new page
        if(file_type=="normal_device"){
          dev.new(width=lp$page_width,height=lp$page_height)
        } else if(file_type=="pdf") {
          if(length(regions)/lp$maxgraphics>1) {
            folder_and_file<-paste(output_folder,sub(" ","_",paste(main,collapse="")),"_",data_name,i%/%lp$maxgraphics+1,".pdf",sep="")
          } else {
            folder_and_file<-paste(output_folder,sub(" ","_",paste(main,collapse="")),"_",data_name,".pdf",sep="")
          }
          pdf(file=folder_and_file,width=lp$page_width,height=lp$page_height)        
        } else if(file_type!="none"){
          stop("Unknown file-type!")
        }
        par(oma=c(2.5,2.1,0,0))
        layout(lp$layout_matrix,heights=lp$layout_heights)
        par(cex=cex)
        par(mar=c(0,0,0,0))
        plot(NA,axes=FALSE,ylab="",xlab="",ylim=c(0,1),xlim=c(0,1))
        if(length(regions)/lp$maxgraphics>1) {
          header.tmp <- paste(data_name,(i-1)%/%lp$maxgraphics+1,"/",ceiling(length(regions)/lp$maxgraphics))
        } else {
          header.tmp <- data_name
        }
        
        if(is.list(main)) {
          tmp <- main
          names(tmp) <- paste("a",1:length(main),sep="")
          for(ii in (length(main)+1):20) tmp[paste("a",ii,sep="")] <- ""    
          tmp[["b"]] <- paste(" ",header.tmp,sep="")
          main.expr <- substitute(a1*a2*a3*a4*a5*a6*a7*a8*a9**a10*a11*a12*a13*a14*a15*a16*a17*a18*a19*a20*b,tmp)   
        } else {
          main.expr <- paste(main,header.tmp)
        }         
        
        
        par(xpd=NA)
        text(0.1,1,main.expr,cex=2.3,adj=c(0,1))
        if(!is.null(legend)) {      
          if(is.null(lowpass.iter)) lty.obs <- NA
          else lty.obs <- 1
          legend(0.9,0,legend=paste(legend,"  "),
                 lty=c(lty.obs,ltys),lwd=c(lwds[1],lwds),col=c("blue",color),pch=c("o",rep(NA,length(lwds))),
                 cex=0.9,xjust=1,yjust=0.3,horiz=TRUE,x.intersp=0.5)
        }
        par(xpd=FALSE)
        
      }
      if(calc_ylim) {
        tmp_func<-function(x,region,name){
          return(x[region,,name])
        }
        if(is.list(measured_data)){
          vector_measured_data<-unlist(lapply(measured_data,tmp_func,region=region,name=data_name))
        } else {
          vector_measured_data<-as.vector(measured_data)
        }
        if(is.list(magpie_data)){
          vector_magpie_data<-unlist(lapply(magpie_data,tmp_func,region=region,name=data_name))
        } else {
          vector_magpie_data<-as.vector(magpie_data)
        }
        ylim <- c(min(vector_measured_data,vector_magpie_data,na.rm=TRUE),max(vector_measured_data,vector_magpie_data,na.rm=TRUE))
        if(is.infinite(ylim[1])) ylim[1] <- 0
        if(is.infinite(ylim[2])) ylim[2] <- 1
      }
      j <- 0
      par(mar=c(1.5,1.5,1,1))
      plot(NULL,NULL,ylim=ylim,xlim=xlim,xlab="",ylab="")
      
      grid()
      for(nmagpie in 1:length(magpie_data)) {     
        vector_magpie_data <- as.vector(magpie_data[[nmagpie]][region,,data_name]) 
        j <- j+1
        lines(vector_magpie_data~years_magpie[[nmagpie]],col=color[j],lty=ltys[j],lwd=2)
      }    
      cols<-c("blue","green","black","red","grey","yellow")
      j<-1
      for(nmeasured in 1:length(measured_data)) {      
        if(!is.null(lowpass.iter)){
          lowpass_vector_measured_data <- lowpass(as.vector(measured_data[[nmeasured]][region,,data_name]), i=lowpass.iter)
          tmp <- years_measured[[nmeasured]]<min(unlist(years_magpie))
          lines(lowpass_vector_measured_data[tmp]~years_measured[[nmeasured]][tmp],col=cols[j],lty=1,lwd=2)
        }
        vector_measured_data <- as.vector(measured_data[[nmeasured]][region,,data_name])       
        points(vector_measured_data~years_measured[[nmeasured]],col=cols[j],pch=1,cex=1)
        j<-j+1
      }
      lines(rep(min(unlist(years_magpie)),2),ylim+c(-100,100),col="#332c29",lty=2)
      legend("topleft",region,bg="white")
      if(lp$is_left[i]) {
        if(is.list(ylab)) { 
          tmp <- ylab
          names(tmp) <- paste("a",1:length(tmp),sep="")
          for(ii in (length(tmp)+1):20) tmp[paste("a",ii,sep="")] <- ""    
          ylab <- substitute(a1*a2*a3*a4*a5*a6*a7*a8*a9**a10*a11*a12*a13*a14*a15*a16*a17*a18*a19*a20,tmp)  
        }
        mtext(ylab,side=2,line=2.5,cex=1.1*cex)
      }
      if(lp$is_bottom[i]) mtext("year",side=1,line=2.5, cex=1.1*cex)
      
      if(i%%lp$maxgraphics==0 | i==length(regions)) { #page finished
        if(file_type=="pdf") dev.off()
      } 
    }
  }
}