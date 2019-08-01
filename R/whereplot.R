#' whereplot
#' 
#' Plots the results of a logical question on a mpa
#' 
#' 
#' @usage whereplot(x)
#' @param x A logical statement with a magpie object on country resolution
#' @return A map. Green is true, red is false, orange is true and false in the
#' same country, purple is NA, and striped indicates a country that includes
#' some NAs.
#' @author Benjamin Leon Bodirsky
#' @examples
#' \dontrun{
#' data(population_magpie)
#'  test<-population_magpie
#'  dimnames(test)[[1]]<-c("AFG","DEU","FRA","EGY","IND","IDN","RUS","CHN","USA","YEM")
#'  whereplot(test>500)
#'  }
#' @importFrom magclass where new.magpie
#' @export

whereplot<-function(x){
  a<-where(x)
  
  out_true<-a$true$regions
  out_false<-a$false$regions
  out_na<-a$na$regions
  out_true_false <- intersect(out_true,out_false)
  out_countries<-union(out_true,union(out_false,out_na))
  
  out<-new.magpie(out_countries,years = NULL,names = c("value","na"),fill = NA)
  out[out_na,,"value"]<-4
  out[out_true,,"value"]<-1
  out[out_false,,"value"]<-3
  out[out_true_false,,"value"]<-2
  out[,,"na"]<-0
  out[out_na,,"na"]<-1

  plotcountrymap(out,catMethod=c(0,1,2,3,4),
                 colourPalette=c('green','orange','red','mediumpurple1'),
                 hatching=TRUE)


  return()
}