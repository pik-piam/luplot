#' deco_plot
#'
#' Plot for decomposition analysis.
#' Use function deco in library magpie4 to create the necessary object.
#'
#' @param x magpie object with decomposition data (e.g. from the function magpie4:::deco)
#' @param color color vector
#' @return a ggplot object
#'
#' @importFrom grDevices gray.colors
#' @importFrom ggplot2 ggplot aes geom_bar geom_point ggplot labs scale_fill_manual
#' @importFrom quitte as.quitte
#' @importFrom magclass getSets<-
#' @author Benjamin Leon Bodirsky, Ina Neher
#' @export
deco_plot <- function(x, color = gray.colors(6)) { # nolint: object_name_linter.
  getSets(x) <- c("region", "year", "Decomposition")
  legendNames <- getNames(x)
  x <- x[, , rev(legendNames)]
  color <- rev(color)[seq_along(legendNames)]

  x1 <- as.quitte(x[, , 1:(length(getNames(x)) - 1)])
  x2 <- as.quitte(x[, , length(getNames(x))])
  class(x1) <- setdiff(class(x1), "data.table")
  class(x2) <- setdiff(class(x2), "data.table")
  x1$region <- factor(x1$region, levels = getItems(x, 1.1))
  x2$region <- factor(x2$region, levels = getItems(x, 1.1))

  years <- getYears(x, as.integer = TRUE)
  yeardiff <- unique(years[2:length(years)] - years[1:(length(years) - 1)])
  if (length(yeardiff) > 1) {
    stop("timesteps have to have equal length")
  }

  x1$period <- x1$period - yeardiff / 2
  x2$period <- x2$period - yeardiff / 2

  p <- ggplot()
  p <- p + geom_bar(data = x1, size = 1, stat = "identity", position = "stack",
                    aes_string(x = "period", y = "value", fill = "Decomposition"))
  p <- p + geom_line(data = x2, aes_string(x = "period", y = "value"), color = "white", size = 1.2)
  p <- p + geom_line(data = x2, aes_string(x = "period", y = "value"), color = color[length(legendNames)], size = 0.8)
  p <- p + scale_x_continuous(minor_breaks = c(years[1] - yeardiff, years))
  p <- p + theme(panel.grid.minor.x =  element_line(colour = "grey", size = 0.25),
                 panel.grid.major.x =  element_line(colour = "#666666ff", size = 0.5))
  p <- p + facet_wrap(~region, ncol = 2)
  p <- p + scale_fill_manual(values = color)
  p <- p + labs(y = "percent/timestep", x = "year")
  return(p)
}
