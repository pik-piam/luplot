#' @title correlationDataSets
#' @description Plots 2D density plots to evaluate the relationship between two datasets.
#' It also returns the R-square for the correlation
#'
#' @export
#'
#' @param x First data set (x axis) (observed) as magpie object with one or more items in 3rd dimension
#' @param y Second data set (y axis) (predicted) as magpie object with one or more items in 3rd dimension
#' @param title title of plot
#' @param xlab x axis title
#' @param ylab y axis title
#' @param limx limits y axis (if NULL, fitting limits are calculated within the function)
#' @param limy limits x axis (if NULL, fitting limits are calculated within the function)
#' @param bins number of bins in histogram
#' @param folder path in which to save the plots. if "." is used, it is saved 
#' in the current working directory. If NULL, no plots are saved.
#' @param file name of file
#' @param statFont font size r2-MAE label
#' @param breaks breaks of the legend. It can be a vector, waiver()=the ones from the transformation, NULL for no breaks
#' @param nrows number of rows in pdf file where plots are printed
#' @param ncols number of columns in pdf file where plots are printed
#' @param axisFont Font size of text of axis of the correlation plot
#' @param axisTitleFont Font size of title of axis of the correlation plot
#' @param TitleFontSize Font size of title of correlation plot
#' @param legendTitleFont Font size of the title of the legend
#' @param legendTextFont Font size of legend
#' @param table Conditional to include table with statistics in the output. TRUE (includes it), FALSE (it doesn't)
#' @param stat Conditional to include R2 and MAE on the fiure. TRUE (includes it), FALSE (it doesn't)
#' @author Edna Molina Bacca
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 ggplot aes_ xlim ylim scale_fill_gradientn coord_fixed geom_abline geom_vline geom_hline
#' labs theme geom_bin2d annotate
#' @importFrom RColorBrewer brewer.pal
#' @importFrom magclass setNames
#' @importFrom gridExtra marrangeGrob
#' @importFrom utils write.csv2
#'
#' @examples
#' \dontrun{
#' x <- plotCorrHist2D(x, y, folder = ".")
#' }
#'
plotCorrHist2D <- function(x, y, title = NULL, xlab = "x", ylab = "y", bins = 40, limx=NULL, limy=NULL,
                           folder = NULL, file = "", breaks = waiver(),nrows=2, ncols=2, axisFont=13,
                           axisTitleFont=13,TitleFontSize=15,legendTitleFont=12,legendTextFont=10,
                           statFont=4, table=FALSE, stat=TRUE) {


  getNames(x) <- gsub(x = getNames(x), pattern = "\\.", replacement = "_")
  getNames(y) <- gsub(x = getNames(y), pattern = "\\.", replacement = "_")


  if (!all(getCells(x) == getCells(y))) stop("Data sets don't have the same regional resolution")
  years <- if (!is.null(intersect(getYears(x), getYears(y)))) intersect(getYears(x, as.integer = TRUE),
                                                                        getYears(y, as.integer = TRUE)) else
                                                                          stop("No common years between data sets")
  names <- if (!is.null(intersect(getNames(x), getNames(y)))) intersect(getNames(x),
                                                                        getNames(y)) else
                                                                          stop("No common items between data sets")

  plotMagpie <- luplot::as.ggplot(mbind(setNames(x[, years, names], paste0(getNames(x), "_x")),
                                setNames(y[, years, names],
                                         paste0(getNames(x), "_y"))))[, c("Cell", "Region", "Year", "Data1", "Value")]
  plotMagpie <- reshape(plotMagpie, direction = "wide", idvar = c("Cell", "Region", "Year"), timevar = "Data1")

  plots <- list()
  corr <- c("Year","Variable", "r2", "MAE", "Bias", "error per unit")

  rf <- colorRampPalette(rev(brewer.pal(11, "RdYlBu")))
  r <- rf(32) # color palette

  for (na in names) {
    for (ye in years) {


      data <- plotMagpie[plotMagpie$Year == ye, ]

      valueX <- as.character(paste0("Value.", na, "_x"))
      valueY <- as.character(paste0("Value.", na, "_y"))

      limMin <- min(min(data[, valueX]), min(data[, valueY]))
      limMax <- max(max(data[, valueX]), max(data[, valueY]))

      limx1 <- if (is.null(limx)) c(limMin - limMax / 7, limMax + limMax / 7) else limx
      limy1 <- if (is.null(limy)) c(limMin - limMax / 7, limMax + limMax / 7) else limy

      labelX <- limx1[2] - limx1[2] / 8
      labelY <- limMin + limMax / 4
      labelY2 <- limMin + limMax / 10


      year <- as.character(ye)


      r2 <- round(cor(data[, valueX], data[, valueY])^2, 3)
      mae <- luplot::qualityMeasure(pd = data[, valueY], od = data[, valueX], measures = "MAE", p_value = FALSE)
      relativeError <- (data[, valueY] - data[, valueX]) / data[, valueX]
      relativeError[!is.finite(relativeError)] <- NA
      bias <- sum(relativeError * 100, na.rm = TRUE) / length(relativeError[is.finite(relativeError)])
      absoluteError <- abs(data[, valueY] - data[, valueX])
      error <- round(sum(absoluteError) / sum(data[, valueX]), 3)

      all <- t(c(year,na, r2, mae, bias,error))
  

      corr <- rbind(corr, all)
      tag <- paste0(na, "-", year)

      plots[[tag]] <- ggplot(data, aes_string(x = valueX, y = valueY)) + theme_bw()
      plots[[tag]] <- plots[[tag]] + geom_bin2d(bins = bins) + coord_fixed(ratio = 1) +
        geom_abline(intercept = 0, slope = 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
      plots[[tag]] <- plots[[tag]] + labs(x = xlab, y = ylab, title = paste0(title, " ", "(", na, "-", year, ")")) +
        scale_fill_gradientn(colours = r, trans = "log", breaks = breaks)
      plots[[tag]] <- plots[[tag]] + theme(axis.text.x = element_text(color = "grey20", size = axisFont),
                                           axis.title.x = element_text(color = "grey20", size = axisTitleFont),
                                           axis.text.y = element_text(color = "grey20", size = axisFont),
                                           axis.title.y = element_text(color = "grey20", size = axisTitleFont),
                                           plot.title = element_text(size = TitleFontSize, face = "bold"),
                                           legend.text = element_text(size = legendTextFont),
                                           legend.title = element_text(size = legendTitleFont),
                                           legend.background = element_blank())

      plots[[tag]] <- if(stat) plots[[tag]] + annotate("label", size = statFont, x = labelX, y = labelY,
                                              label = paste0("R2 = ", r2), hjust = 1) else plots[[tag]]
      plots[[tag]] <- if(stat) plots[[tag]] + annotate("label", size = statFont, x = labelX, y = labelY2,
                                              label = paste0("MAE = ", mae), hjust = 1) else plots[[tag]]
      plots[[tag]] <- if (!is.null(limx)) plots[[tag]] + scale_x_continuous(limits = limx) else plots[[tag]] + scale_x_continuous(limits = limx1)
      plots[[tag]] <- if (!is.null(limy)) plots[[tag]] + scale_y_continuous(limits = limy) else plots[[tag]] + scale_y_continuous(limits = limy1)
      
    }
  }
  
    if (!is.null(folder)) {
      if (folder != "." && !dir.exists(folder)) dir.create(folder)
      if(is.null(file)) stop("Please, write a file name")
      ml<-gridExtra::marrangeGrob(plots, nrow = 2, ncol = 2)
      ggsave(paste0(folder,file,".pdf"), ml)
      write.csv2(corr,paste0(folder, file, "_stats.csv"))
         }

  out <- if (table) list(plots,corr) else plots
  return(out)
}
