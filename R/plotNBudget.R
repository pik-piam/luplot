#' @title plotNbudget
#' @description Plots Nitrogen Budgets for Cropland soils on regional or country levels.
#'
#' @param nitrogenbudget A magpie object with a nitrogen budget
#' @param type "bar" or "area" plot
#' @return Stacked bar or area plot
#' @author Benjamin Leon Bodirsky
#' @examples
#' 
#' \dontrun{ 
#' library(moinput)
#' countries<-c("USA","CHN","NGA")
#' nitrogen_budget<-calcOutput("NitrogenBudget",aggregate = FALSE)
#' plotNbudget(nitrogenbudget=nitrogen_budget[countries,,],type="area")
#' plotNbudget(nitrogenbudget=colSums(nitrogen_budget),type="bar")
#' 
#' }
#' @export
#' @importFrom ggplot2 ggplot aes_ geom_bar facet_grid geom_area scale_fill_manual



plotNbudget<-function(nitrogenbudget,type="bar"){
  #if (countries!="regions"){
    #mb<-calcOutput("NitrogenBudget",aggregate = FALSE)[countries,,]
  #  mb<-nitrogenbudget[countries,,]
  #} else {
    #mb<-calcOutput("NitrogenBudget")
  #}
  
  mb<-nitrogenbudget
  #guarantee the right order
  mb<-mb[,,c("som","seed",                  
             "fixation_crops","fixation_freeliving","manure","manure_stubble_grazing",
             "ag_recycling","bg_recycling","ag_ash","deposition",            
             "fertilizer","balanceflow","harvest","ag","bg","surplus")]
  
  mb[,,c("harvest","ag","bg","surplus")]<- -mb[,,c("harvest","ag","bg","surplus")]
  x.plot <- as.ggplot(mb)
  x_pos <- x.plot
  x_neg <- x.plot
  x_pos$Value[x_pos$Value<0] <- 0
  x_neg$Value[x_neg$Value>=0] <- 0
  
  p <- ggplot(x.plot, 
              aes_(x=~Year,
                  y = ~Value,
                  fill=~Data1)) 

  
  if (type=="bar"){
    if (any(x_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_bar(data=x_pos,stat = "identity",colour="black",position="stack")+ facet_grid(~ Region)
    if (any(x_neg$Value <  0,na.rm=TRUE)) p <- p + geom_bar(data=x_neg,stat = "identity",colour="black",position = "stack")+ facet_grid(~ Region)
  } else if (type=="area"){
    if (any(x_pos$Value >= 0,na.rm=TRUE)) p <- p + geom_area(data=x_pos,stat = "identity",colour="black",position="stack")+ facet_grid(~ Region)
    if (any(x_neg$Value <  0,na.rm=TRUE)) p <- p + geom_area(data=x_neg,stat = "identity",colour="black",position = "stack")+ facet_grid(~ Region)
    
  }
  
  p <- p + scale_fill_manual(values=c("#664200","#ffffb3",
                                      "#0066ff","#33cccc","#cc6600","#cc9900",
                                      "#ccff33","#99cc00","#cccccc","#66ffff",
                                      "#ff0000","#ffffff",
                                      "#ffcc00","#00ff00","#00cc00","#ff00ff"
                                      ))
  p
  return(p)
}