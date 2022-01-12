#' @title correlationDataSets
#' @description Plots a 2D density plot to evaluate the relationship between two variables.
#' It also returns the R-square for the correlation
#'
#' @export
#'
#' @param x First data set (x axis) as magpie object with only one item in 3rd dimension
#' @param y Second data set (y axis) as magpie object with only one item in 3rd dimension
#' @param title title of plot
#' @param xlab x axis title
#' @param ylab y axis title
#' @param labelX position of R2 and MAE text in x
#' @param labelY position of R2 and MAE text in Y
#' @param xlim boundaries for the x axis
#' @param ylim boundaries for the y axis
#' @param bins number of bins in histogram
#' @param folder folder in which to save the plots
#' @author Edna Molina Bacca
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 ggplot aes_ xlim ylim scale_fill_gradientn coord_fixed geom_abline geom_vline geom_hline labs theme geom_bin2d annotate
#' @importFrom RColorBrewer brewer.pal
#' @importFrom magclass setNames
#'
#' @examples
#' \dontrun{
#' x <- plotCorrHist2D(gdx)
#' }
#'
plotCorrHist2D <- function(x, y, title = NULL, xlab = "x", ylab = "y", labelX = 0, labelY = 0, xlim = NULL, ylim = NULL, bins = 100, folder = NULL) {

  cells <- if (all(getCells(x) == getCells(y))) getCells(x) else stop ("Data sets don't have the same regional resolution")
  years <- if (!is.null(intersect(getYears(x), getYears(y)))) intersect(getYears(x, as.integer = TRUE), getYears(y, as.integer = TRUE)) else stop ("No common years between data sets")
  names <- if (!is.null(intersect(getNames(x), getNames(y)))) intersect(getNames(x), getNames(y)) else stop ("No common items between data sets")

  plotMagpie <- as.ggplot(mbind(setNames(x[, years, names], paste0("x")), setNames(y[, years, names], paste0("y"))))[, c("Cell", "Region", "Year", "Data1", "Value")]
  plotMagpie <- reshape(plotMagpie, direction = "wide", idvar = c("Cell", "Region", "Year"), timevar = "Data1")

  plots <- list()
  corr <- c("Year", "R2", "MAE", "Willmott refined")

  rf <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
  r <- rf(32) # color palette

  for (ye in years) {

      data <- plotMagpie[plotMagpie$Year == ye, ]

      if (is.null(xlim) | is.null(ylim)) {
      limMin <- min(min(data[, "Value.x"]), min(data[, "Value.y"]))
      limMax <- max(min(data[, "Value.x"]), max(data[, "Value.y"]))
      }

      limx <- if (!is.null(xlim)) c(xlim[1] - 1, xlim[2] + 1) else c(limMin - 1, limMax + 1)
      limy <- if (!is.null(ylim)) c(xlim[1] - 1, ylim[2] + 1) else c(limMin - 1, limMax + 1)

      labelX <- if (labelX > 0) labelX else max(limx) * 3 / 4
      labelY <- if (labelY > 0) labelY else max(limy) * 1 / 3

      year <- as.character(ye)


      R2 <- round(cor(data[, "Value.x"], data[, "Value.y"])^2, 3)
      mae <- qualityMeasure(pd = data[, "Value.x"], od = data[, "Value.y"], measures = "MAE", p_value = FALSE)
      will <- qualityMeasure(pd = data[, "Value.x"], od = data[, "Value.y"], measures = "Willmott refined", p_value = FALSE)
      All <- t(c(year, R2, mae, will))

      corr <- rbind(corr, All)

      plots[[year]] <- ggplot(data, aes_string(x = "Value.x", y = "Value.y")) + theme_bw() + xlim(limx[1], limx[2]) + ylim(limy[1], limy[2])
      plots[[year]] <- plots[[year]] + geom_bin2d(bins = bins) + coord_fixed(ratio = 1) + geom_abline(intercept = 0, slope = 1) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
      plots[[year]] <- plots[[year]] + labs(x = xlab, y = ylab, title = paste0(title, " ", "(", year, ")")) + scale_fill_gradientn(colours = r, trans = "log")
      plots[[year]] <- plots[[year]] + theme(axis.text.x = element_text(color = "grey20", size = 20),
                                       axis.title.x = element_text(color = "grey20", size = 22),
                                       axis.text.y = element_text(color = "grey20", size = 20),
                                       axis.title.y = element_text(color = "grey20", size = 22),
                                       plot.title = element_text(size = 26, face = "bold"),
                                       legend.text = element_text(size = 12),
                                       legend.title = element_text(size = 14),
                                       legend.background = element_blank(),
                                       legend.box.background = element_rect(colour = "black"))

      plots[[year]] <- plots[[year]] + annotate("text", x = labelX, y = labelY, label = paste0("R2 = ", R2))
      plots[[year]] <- plots[[year]] + annotate("text", x = labelX, y = labelY - labelY / 5, label = paste0("MAE = ", mae))



  }

  return(list(plots, corr))
}
