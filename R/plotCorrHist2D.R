#' @title correlationDataSets
#' @description Plots 2D density plots to evaluate the relationship between two datasets.
#' It also returns the R-square for the correlation
#'
#' @export
#'
#' @param x First data set (x axis) as magpie object with one or more items in 3rd dimension
#' @param y Second data set (y axis) as magpie object with one or more items in 3rd dimension
#' @param title title of plot
#' @param xlab x axis title
#' @param ylab y axis title
#' @param bins number of bins in histogram
#' @param fontLabel font size R2-MAE label
#' @param folder path in which to save the plots. if "." is used, it is saved in the current working directory.
#' @param breaks breaks of the legend. It can be a vector, waiver()=the ones from the transformation, NULL for no breaks
#' @author Edna Molina Bacca
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 ggplot aes_ xlim ylim scale_fill_gradientn coord_fixed geom_abline geom_vline geom_hline
#' labs theme geom_bin2d annotate
#' @importFrom RColorBrewer brewer.pal
#' @importFrom magclass setNames
#'
#' @examples
#' \dontrun{
#' x <- plotCorrHist2D(x, y, folder = ".")
#' }
#'
plotCorrHist2D <- function(x, y, title = NULL, xlab = "x", ylab = "y", bins = 40, folder = NULL, fontLabel = 5.5, breaks=waiver()) {


  getNames(x)<-gsub(x=getNames(x),pattern="\\.",replacement="_")
  getNames(y)<-gsub(x=getNames(y),pattern="\\.",replacement="_")


  if (!all(getCells(x) == getCells(y))) stop("Data sets don't have the same regional resolution")
  years <- if (!is.null(intersect(getYears(x), getYears(y)))) intersect(getYears(x, as.integer = TRUE),
                         getYears(y, as.integer = TRUE)) else stop("No common years between data sets")
  names <- if (!is.null(intersect(getNames(x), getNames(y)))) intersect(getNames(x),
                         getNames(y)) else stop("No common items between data sets")

  plotMagpie <- as.ggplot(mbind(setNames(x[, years, names], paste0(getNames(x), "_x")),
            setNames(y[, years, names], paste0(getNames(x), "_y"))))[, c("Cell", "Region", "Year", "Data1", "Value")]
  plotMagpie <- reshape(plotMagpie, direction = "wide", idvar = c("Cell", "Region", "Year"), timevar = "Data1")

  plots <- list()
  corr <- c("Year", "R2", "MAE", "Willmott refined","Bias")

  rf <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  r <- rf(32) # color palette

  for (na in names) {
  for (ye in years) {


      data <- plotMagpie[plotMagpie$Year == ye, ]

      ValueX <- as.character(paste0("Value.", na, "_x"))
      ValueY <- as.character(paste0("Value.", na, "_y"))

      limMin <- min(min(data[, ValueX]), min(data[, ValueY]))
      limMax <- max(min(data[, ValueX]), max(data[, ValueY]))

      limx <- c(limMin -limMax/10, limMax + limMax/10 )
      limy <- c(limMin -limMax/10, limMax + limMax/10 )

      labelX <- (max(limx) * 1 / 5)
      labelY <- (max(limy) * 5 / 6)

      year <- as.character(ye)


      R2 <- round(cor(data[, ValueX], data[, ValueY])^2, 3)
      mae <- qualityMeasure(pd = data[, ValueX], od = data[, ValueY], measures = "MAE", p_value = FALSE)
      will <- qualityMeasure(pd = data[, ValueX], od = data[, ValueY], measures = "Willmott refined", p_value = FALSE)
      bias <- sum(data[, ValueX]-data[, ValueY])/length(data[, ValueX])
      All <- t(c(year, R2, mae, will,bias))

      corr <- rbind(corr, All)
      tag <- paste0(na, "-", year)

      plots[[tag]] <- ggplot(data, aes_string(x = ValueX, y = ValueY)) + theme_bw()
      plots[[tag]] <- plots[[tag]] + geom_bin2d(bins = bins) + coord_fixed(ratio = 1) +
                      geom_abline(intercept = 0, slope = 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
      plots[[tag]] <- plots[[tag]] + labs(x = xlab, y = ylab, title = paste0(title, " ", "(", na, "-", year, ")")) +
                      scale_fill_gradientn(colours = r,trans="log",breaks=breaks)
      plots[[tag]] <- plots[[tag]] + theme(axis.text.x = element_text(color = "grey20", size = 18),
                                       axis.title.x = element_text(color = "grey20", size = 18),
                                       axis.text.y = element_text(color = "grey20", size = 18),
                                       axis.title.y = element_text(color = "grey20", size = 18),
                                       plot.title = element_text(size = 20, face = "bold"),
                                       legend.text = element_text(size = 16),
                                       legend.title = element_text(size = 17),
                                       legend.background = element_blank())

      plots[[tag]] <- plots[[tag]] + annotate("text", size = fontLabel, x = labelX, y = labelY,
                                              label = paste0("R2 = ", R2))
      plots[[tag]] <- plots[[tag]] + annotate("text", size = fontLabel, x = labelX, y = labelY - labelY /10,
                                              label = paste0("MAE = ", mae))
      plots[[tag]] <- plots[[tag]] + scale_x_continuous(limits = limx) +
                                     scale_y_continuous(limits = limy)

      if (!is.null(folder)) {

      if (folder != "." & !dir.exists(folder)) dir.create(folder)

        png(filename = (paste0(folder, "/", tag, ".png")), width = 600,
            height = 600)
        print(plots[[tag]])
        dev.off()


      }
    }

  }

  return(list(plots, corr))
}
