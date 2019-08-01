#' get_layout_parameters
#' 
#' Estimates the layout parameters when multiple plots shall be plotted on one
#' page
#' 
#' 
#' @usage get_layout_parameters(ngraphics, nrow=2, ncol=5, header=FALSE,
#' design=ifelse(nrow>ncol,"vertical","horizontal"), verbose=FALSE)
#' @param ngraphics Number of Graphics which shall be plotted
#' @param nrow Number of graphics per row
#' @param ncol Number of graphics per column
#' @param header adds an additional plot windows at the top of the layout
#' matrix (can be used for titel and/or legend)
#' @param design Horizontal or vertical Page design
#' @param verbose decides, if additional information about the settings should
#' be reported or not
#' @return Creates a list with basic design parameters.
#' return_data<-list(layout_matrix,page_width,page_height,graphics_per_page,n_pages,design,is_left,is_right,is_bottom,is_top)
#' layout_matrix returns a matrix which corresponds to the arrangement of
#' graphics on the page. Example: 1,2,3 4,5,6
#' 
#' @author Benjamin Bodirsky, Jan Philipp Dietrich
#' @seealso \code{\link{comparisonplot},\link{histoplot}}
#' @examples
#' \dontrun{
#' layout_parameters<-get_layout_parameters(15, nrow=2, ncol=5, design="horizontal", verbose=TRUE) 
#' pdf(file="output.pdf",width=layout_parameters$page_width,height=layout_parameters$page_height)
#' layout(layout_parameters$layout_matrix)
#' layout(layout_parameters[[1]])  
#'       for(i in 1:15) 
#'       {
#'          plot((1:10))
#'       }                                                            
#' dev.off()
#'       }
#' @export
#' 
#################################
#### Data Visualisation      ####
#################################
#provides functions for fast, nice (or both) visualisation of MAGPIE Objects

# Version 1: histoplot, get_layout_parameters, comparisonplot and scratchplot added
# Version 1.1: histoplot lowpass filter added bb
# Version 1.2: scratch_plot area diagramm for added values
# Version 1.3: improved histoplot and get_layout_parameters (jpd)
# Version 1.31: added cex as parameter for histoplot (jpd)
# Version 1.32: small bugfixes in histoplot
# Version 1.33: further bugfix in histoplot (always first linetype used) (jpd)
# Version 1.34: scratch_plot: Legend externalized, height can be vector of 2 (min, max)


get_layout_parameters<-function(ngraphics, nrow=2, ncol=5, header=FALSE, design=ifelse(nrow>ncol,"vertical","horizontal"), verbose=FALSE)
{
  maxgraphics <- nrow*ncol
  npages<-ceiling(ngraphics/maxgraphics)
  if(ngraphics==0){stop("No Data selected to be layouted.")}
  
  is_left   <- 1:ngraphics%%ncol==1 #vector indicating, which plots are on the left side of a page
  is_right  <- (1:ngraphics+1)%%ncol==1 #same for the right side of a page
  is_top    <- (1:ngraphics-1)%%maxgraphics<ncol #same for top
  is_bottom <- (1:ngraphics-1)%%maxgraphics>maxgraphics-1-ncol #same for bottom
  is_bottom[length(is_bottom)+(-(ncol-1):0)] <- TRUE  #last elements are always at the bottom

                    
  if (design=="vertical") {
      page_width<-8
      page_height<-12
    } else if (design=="horizontal"){
      page_width<-12
      page_height<-8
    } else {stop("Invalid Design setting")}

  layout_matrix<-matrix(c(1:maxgraphics),nrow=nrow,ncol=ncol,byrow=TRUE)
  layout_heights <- rep(1,nrow(layout_matrix))
  
  if(header) {
    layout_matrix <- rbind(rep(1,ncol(layout_matrix)),layout_matrix+1)
    layout_heights <- c(round(sum(layout_heights)/14,1),layout_heights)
  }

  return_data<-list(layout_matrix=layout_matrix,layout_heights=layout_heights,page_width=page_width,page_height=page_height,maxgraphics=maxgraphics,n_pages=npages,design=design,is_left=is_left,is_right=is_right,is_top=is_top,is_bottom=is_bottom)
  if(verbose) print(return_data)
  return(return_data)
}
