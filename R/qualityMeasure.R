#' qualityMeasure
#' 
#' Function that returns several efficiency indices of model performance wrt
#' observed data
#' 
#' 
#' @usage qualityMeasure(pd, od, wt, measures=c("Willmott",
#' "Nash Sutcliffe"),p_value=FALSE)
#' @param pd Predicted data. Format required: numeric vector.
#' @param od Observed data. Must be the same format and length as \code{pd}.
#' @param wt A vector of weights. The same length as \code{pd} and \code{od}.
#' Missing weights are interprated as all weights equal to 1.
#' @param measures A vector of supported statistical measures. Currently
#' available are: "Willmott", "Willmott refined", "Nash Sutcliffe", "RMSE",
#' "MAE", "Pearson","Kendall","Spearman".
#' @param p_value p_value=TRUE,reports significance level of Pearson, Kendall
#' and Spearmann coefficients.
#' @return Returns a numeric vector with calculated measures. Names of vector
#' elements are the measures applied.
#' @author Misko Stevanovic,Xiaoxi Wang
#' @seealso \code{\link{performancePlot}}
#' @examples
#' 
#' \dontrun{
#' od <- rnorm(1000)
#' pd <- rnorm(1000)
#' 
#' qualityMeasure(pd,od)
#' qualityMeasure(pd,od,measures=c("RMSE","Pearson","Willmott refined"))
#' }
#' @export
#' @importFrom stats weighted.mean cor cor.test 
#' @importFrom graphics axis polygon
#' @importFrom Hmisc rcorr
#' 


qualityMeasure <- function(pd, od, wt, measures=c("Willmott","Nash Sutcliffe"),p_value=FALSE){
  # Function that returns several efficiency indices of model performance wrt observed data
  # Version 1.0 -- Misko Stevanovic
  
	indices <- rep(NA,length(measures))
	names(indices) <- measures
  
  for(i in 1:length(measures)){
    if(!(measures[i] %in% c("Willmott","Willmott refined","Nash Sutcliffe","RMSE","MAE","Pearson","Kendall","Spearman")))
      stop(paste(measures[i])," measure is not currently supported. Check your spelling.")
  }
	
	# Willmott index of agreement	
	if("Willmott" %in% measures){
		index <- 1-sum((pd-od)^2) / sum((abs(pd-weighted.mean(od,wt))+abs(od-weighted.mean(od,wt)))^2)
		indices["Willmott"] <- index
	}
		
	# Refined Willmott index
	if("Willmott refined" %in% measures){
		if(sum(abs(pd-od)) <= 2*sum(abs(od-weighted.mean(od,wt)))){
			index <- 1 - sum(abs(pd-od)) / (2*(sum(abs(od-weighted.mean(od,wt)))))
		} else{
			index <- (2*(sum(abs(od-weighted.mean(od,wt))))) / sum(abs(pd-od)) - 1
			}
		indices["Willmott refined"] <- index
	}
	
	# Nash-Sutcliffe
	if("Nash Sutcliffe" %in% measures){
		index <- 1 - sum(((pd-od)^2)) / sum(((od-weighted.mean(od,wt))^2)) 
		indices["Nash Sutcliffe"] <- index
	}
	
	# Root mean squere error
	if("RMSE" %in% measures){
		index <- sqrt(sum((pd-od)^2)/(nrow(pd)*ncol(pd))) 
		indices["RMSE"] <- index
	}
	
	# Mean absolute error
	if("MAE" %in% measures){
		index <- sum(abs(pd-od))/(nrow(pd)*ncol(pd))
		indices["MAE"] <- index
	}
	
	# Pearson correlation coefficient
	if("Pearson" %in% measures){
		index <- cor(od,pd,use="everything",method="pearson")
		indices["Pearson"] <- index
		if(p_value){
		  pVal <- cor.test(od,pd,method="pearson")
		  pVal <- round(pVal$p.value,3)
		  indices["Pearson|p-value"] <- pVal
		}
		
	}
	
	if("Kendall" %in% measures){
	  index <- cor(od,pd,use="everything",method="kendall")
	  indices["Kendall"] <- index
	  if(p_value){
	    pVal <- cor.test(od,pd,method="kendall")
	    pVal <- round(pVal$p.value,3)
	    indices["Kendall|p-value"] <- pVal
	  }
	}
	
	if("Spearman" %in% measures){
	  index <- cor(od,pd,use="everything",method="spearman")
	  indices["Spearman"] <- index
	  if(p_value){
	    pVal <- cor.test(od,pd,method="spearman")
	    pVal <- round(pVal$p.value,3)
	    indices["Spearman|p-value"] <- pVal
	  }
	}
	
	indices <- round(indices,2)
	return(indices)
}