#' performancePlot
#' 
#' Creates scatter plots for comparison of two data sets and reports various
#' efficiency measures
#' 
#' The format of \code{pdata} and \code{odata} supported are \code{matrix},
#' \code{magpie} and \code{vector}. Comparison of one \code{matrix} and one
#' \code{magpie} object is allowed, but with the same row and column names. For
#' the \code{magpie} objects it compares just the first year in the \code{year}
#' dimension.  The comparison is performed columnwise.
#' 
#' @usage performancePlot(pdata, odata, fname=NULL, weight=NULL, measures=NULL,
#' p_value=FALSE,qnt=NULL, w=8.5, h=11, mfrow=c(1,1), f_siz=0.6, na.rm=FALSE,
#' ...)
#' @param pdata Predicted data (e.g. MAgPIE output)
#' @param odata Observed data (e.g. FAO)
#' @param fname Name of the pdf output file. If the \code{fname} is
#' \code{NULL}, the pdf file will not be created, but the created plots will be
#' previewed in the R graphic device.
#' @param weight A matrix of weights. Must have the same row and column names
#' as the data objects. Can be unspecified in order to obtain the unweighted
#' mean.
#' @param measures A vector of efficiency measures that are wanted to be
#' calculated and reported on the plots. Currently available measures are:
#' \code{"Willmott", "Willmott refined", "Nash Sutcliffe", "RMSE", "MAE",
#' "Pearson","Kendall","Spearman"}
#' @param p_value p_value=TRUE,reports significance level of Pearson, Kendall
#' and Spearmann coefficients.
#' @param qnt A parameter for labeling the points on the plot. It will label
#' all the points that are greater than the percentile given with this
#' parameter. If \code{qnt=NULL} no point will be labeled. If \code{qnt="0\%"},
#' all labels are put in the plot. If the data has no names (row names), leave
#' \code{qnt=NULL}.
#' @param w Width of the pdf document.
#' @param h Height of the pdf document.
#' @param mfrow A vector of the form \code{c(nr,nc)}. Subsequent figures will
#' be drown in an \code{nr}-by-\code{nc} array on the device by rows.
#' @param f_siz The size of font for point labels and reported indices on the
#' plots.
#' @param na.rm If \code{TRUE}, removes all the \code{NA}, \code{NaN} or
#' \code{Inf} values in both corresponding sets.
#' @param ... Additional arguments to be passed to the \code{plot()} function
#' @importFrom graphics abline legend
#' @importFrom stats quantile
#' @author Misko Stevanovic,Xiaoxi Wang
#' @seealso \code{\link{qualityMeasure}}
#' @examples
#' 
#' 	\dontrun{
#' 	performancePlot(magpie_prod,fao_prod,qnt="98%",mfrow=c(3,2),fname="test",
#' 	measures=c("Willmott","Pearson"))
#' 	performancePlot(magpie_prod,fao_prod)
#' 	performancePlot(magpie_prod, fao_prod, fname="MAgPIE_FAO_production", 
#' 	w=8.5, h=12, mfrow=c(3,2), f_siz=0.7, na.rm=TRUE, xlab=paste("MAgPIE",
#' 	"production, mio.ton DM"),ylab=paste("FAO","production, mio.ton DM")
#' 	,col="blue")
#' 	}
#' 
## Creates scatter plots for comparison of two data sets and reports various efficiency measures
## Version 1.01 - Misko Stevanovic
## 1.01: vector objects implemented and some bugs fixed
#' @export
#' @importFrom magclass is.magpie


performancePlot <- function( 
							pdata,					    # predicted data (e.g. magpie)
							odata,				      # observed data (e.g. fao)
							fname=NULL,					# name of the pdf output file; also a switch between creating the pdf and plotting in the R environment
							weight=NULL,				# matrix of weights; can be omitted in order to get the unweighted mean
							measures=NULL,			# available measures: "Willmott", "Willmott refined", "Nash Sutcliffe", "RMSE", "MAE", "Pearson","Kendall","Spearman"
							p_value=FALSE,       # p_value=TRUE, reports significance level of Pearson, Kendall and Spearmann coefficients
							qnt=NULL,						# quantile for the point labels to be put on plots
							w=8.5,							# width, to function pdf()
							h=11,								# height, to function pdf()
							mfrow=c(1,1),				# splits the plotting screen
							f_siz=0.6,					# the font size on the graph
							na.rm=FALSE,				# removes corresponding rows in both datasets if at least one is NA, NaN or Inf
							...)								# additional arguments to be passed to the plot() function
							{

   
	# transforming the objects 
	if(xor(is.vector(pdata), is.vector(pdata))){
		stop("Both data objects have to be vectors!")
	}
	
	if(is.vector(pdata) & is.vector(odata)) vec <- TRUE
	else vec <- FALSE
	
  tmp <- list(pdata, odata)
  for(i in 1:length(tmp)){
    if(is.magpie(tmp[[i]])){
      tmp[[i]] <- as.array(tmp[[i]])
      tmp[[i]] <- as.matrix(tmp[[i]][,1,])
    }
  }
  pdata <- tmp[[1]]
  odata <- tmp[[2]]
  rm(tmp)
  
	if(!vec){
		if(all(dimnames(pdata)[[2]] != dimnames(odata)[[2]])){
			stop("Column names for both datasets must be the same!")
		}
	}
	
	## create plotting environment	
	if(!is.null(fname)){
		pdf(file=ifelse(TRUE, paste(fname,".pdf",sep=""), paste("name","%0.3d.pdf",sep="")), width=w, height=h, onefile=TRUE)
	}
	par(mfrow=mfrow)
	
	## data preparation and plotting
	if(vec){
		cmp.names <- ""
	}
	else cmp.names <- dimnames(pdata)[[2]]
	for(cx in cmp.names){
		if(vec){ 
			pd <- pdata
			od <- odata
		}
		else{
			pd <- pdata[,cx]
			od <- odata[,cx]
		}
		
		if(all(is.na(pd)) | all(is.na(od)) | all(is.infinite(pd)) | all(is.infinite(od))){
			print(paste("Vector",cx,"has all the NA or Inf values. This data vector will not be plotted."))
			next
		}
		## Remove NA, NaN and Inf values
		if(na.rm){
			na_omit <- c(which(is.na(as.numeric(pd))), which(is.na(as.numeric(od))), which(is.infinite(as.numeric(pd))), which(is.infinite(as.numeric(od))))
			na_omit <- unique(na_omit)
			if(length(na_omit!=0)){
				pd <- pd[-na_omit]
				od <- od[-na_omit]
			}
		}
		
		if(is.null(weight))	wt <- rep(1,length(od))
		else	wt <- weight[names(od),cx]
			
		drange <- max(range(pd),range(od))
		drange <- drange+0.3*drange
		
		## Plot
		if(vec) plot(od,pd,xlim=c(0,drange),ylim=c(0,drange),...)
    else plot(od,pd,xlim=c(0,drange),ylim=c(0,drange),main=paste(cx),...)
				
		if(!is.null(qnt)){
			quant <- which( pd >= quantile(pd, probs=seq(0,1,0.01))[qnt] | od >= quantile(od,probs=seq(0,1,0.01))[qnt])	
			if( (length(names(pd)[quant])!=0 | length(names(od)[quant])!=0) | (length(names(pd)[quant])!=NA | length(names(od)[quant])!=NA) & !all(pd==0) & !all(od==0)){
				text(pd[quant],od[quant],labels=names(pd)[quant],pos=4,cex=f_siz)
			}
		}
		
		abline(0,1)
		
		if(!is.null(measures)){
			indices <- qualityMeasure(pd, od, wt, measures,p_value=p_value)
			ind_box <- NULL
			for(i in 1:length(indices)){
				ind_box <- c(ind_box, paste(names(indices)[i], as.numeric(indices[i]), sep=": "))
			}
			legend(x="bottomright",ind_box,cex=f_siz)
		}
	}
	if(!is.null(fname)) dev.off()
}