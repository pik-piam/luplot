#' @title plotKaya
#' @description Creates barplot of the change in a target variable and its decomposition into drivers over time. The
#' target variable is shown as points, the drivers as bars. The plot can be faceted by region and/or scenario. As input,
#' the function needs a MAgPIE object with the kaya decomposition, as provided by kayaLaspeyres().
#'
#' @export
#'
#' @param data MAgPIE object with kaya decomposition as provided by kayaLaspeyres().
#' @param scenario Scenario to plot. If NULL, all scenarios are plotted. Function cannot work with multiple scenarios
#' and regions (one needs to be specified or just one to start with). Default is NULL.
#' @param region Region to plot. If NULL, all regions are plotted. Function cannot work with multiple scenarios and
#' regions (one needs to be specified or just one to start with). Default is NULL.
#' @param unit Unit of y-axis. Needs to be either percentage (if decomposition has been done in relative terms), or
#' the unit of the target variable. Default is percentage.
#' @param startYear Year to start plotting. Default is 2000.
#' @param addVline Add vertical line at specified year, can be used to visualize change between historic and scenario
#' driven timesteps. Default is NULL.
#' @param title Title of the plot. If NULL, a default title is created based on the target variable. If "off", no title
#' is shown. If a different string is provided, this string is used as title. Default is NULL.
#' @param reducedYlabel Logical. If TRUE, the y-axis label is reduced to the unit of the target variable. If FALSE, the
#' y-axis label contains the unit and a description of the variable. Default is FALSE.
#' @param labels Logical. If FALSE, axis labels are removed. Default is TRUE.
#' @param legend Logical. If FALSE, legend is removed. Default is TRUE.
#' @param lemonLegend Logical. If TRUE, legend is repositioned to an empty facet of the plot if available. Default is
#' TRUE.
#' @param colors Vector of colors for the drivers. If NULL, the default RColorBrewer palette "Set2" is used. Default is
#' NULL.
#' @param localTheme ggplot2 theme to use for the plot. Default is theme_bw().
#' @param printPlot Logical. If TRUE, the plot is printed. Default is TRUE.
#' @param filename If not NULL, the plot is saved as a png file with the provided filename. Default is NULL.
#'
#' @return The function returns a ggplot object.
#' @author Debbora Leip
#' @examples
#' \dontrun{
#' data <- new.magpie(cells_and_regions = c("EUR", "SSA", "USA", "LAM", "IND", "OAS"),
#'                    years = c(2000, 2005, 2010),
#'                    names = as.vector(outer(c("Area", "Population", "Supply"),
#'                                            c("SSP1", "SSP2"), paste, sep = ".")),
#'                    sets = c("Region", "Year", "Variable", "Scenario"), fill = runif(108))
#' kaya <- kayaLaspeyres(data)
#' plotKaya(kaya, scenario = "SSP1")
#' plotKaya(kaya, region = "EUR")
#' plotKaya(kaya, region = "EUR", scenario = "SSP1")
#' }
#'
plotKaya <- function(data, scenario = NULL, region = NULL, unit = "%",
                     startYear = 2000, addVline = NULL, title = NULL,
                     reducedYlabel = FALSE, labels = TRUE,
                     legend = TRUE, lemonLegend = TRUE,
                     colors = NULL, localTheme = theme_bw(),
                     printPlot = TRUE, filename = NULL) { # nolint: cyclocomp_linter
  # if only one scenario, add proxy scenario dimension
  if (length(getItems(data, dim = 3, split = TRUE)) == 1) {
    data <- add_dimension(data, dim = 3.2, add = "scenario")
  }

  # check if scenario or region is chosen
  if (length(getCells(data)) > 1 && length(getNames(data, dim = 1)) > 1 &&
        is.null(scenario) && is.null(region)) stop("Choose either a scenario or a region")

  # variable names
  breaks <- getNames(data, dim = 1)[-1]
  targetName <- getNames(data, dim = 1)[1]

  # reduce data to chosen region, years, scenario
  years <- getYears(data, as.integer = TRUE)
  years <- years[years > startYear]
  data <- data[, years, ]

  if (!is.null(region)) data <- data[region, , ]
  if (!is.null(scenario)) data <- data[, , scenario]

  # check number of subplots and calculate number of rows and columns
  numScenarios <- max(length(getCells(data)), length(getNames(data, dim = 2)))
  if (numScenarios <= 4) {
    cols <- numScenarios
    rows <- 1
  } else {
    cols <- ceiling(sqrt(numScenarios))
    rows <- ifelse(cols * (cols - 1) >= numScenarios, cols - 1, cols)
  }

  # convert data to ggplot format
  target <- data[, , getNames(data, dim = 1)[1], drop = TRUE]
  data <- data[, , getNames(data, dim = 1)[1], invert = TRUE]

  targetPl <- as.ggplot(target)[, -5]
  colnames(targetPl) <- c("Region", "Year", "Scenario", "Value")
  targetPl$Variable <- paste0("Change in ", targetName)

  dataPl <- as.ggplot(data)[, -6]
  colnames(dataPl) <- c("Region", "Year", "Drivers", "Scenario", "Value")

  # shift years for bar plot
  .shiftYears <- function(df) {
    years <- sort(unique(df$Year))
    intervals <- years[2:length(years)] - years[1:(length(years) - 1)]
    intervals <- c(intervals[1], intervals)
    newYears <- years - intervals / 2
    newCol <- df$Year
    for (i in seq_along(years)) newCol[df$Year == years[i]] <- newYears[i]
    df$Year <- newCol
    return(df)
  }

  targetPl <- .shiftYears(targetPl)
  dataPl <- .shiftYears(dataPl)

  # create plot
  if (is.null(colors)) {
    if (length(breaks) > 8) stop("Too many drivers for default color palette, please provide colors manually.")
    colors <- RColorBrewer::brewer.pal(n = length(breaks), name = "Set2")
  }
  yearTicks <- getYears(data, as.integer = TRUE)
  yearTicks <- c(2 * yearTicks[1] - yearTicks[2], yearTicks)
  pl <- ggplot(mapping = aes_string(x = "Year", y = "Value"))  +
    geom_bar(data = dataPl, stat = "identity", aes_string(fill = "Drivers"), alpha = 1) +
    localTheme + theme(panel.spacing.x = unit(1, "lines"), panel.spacing.y = unit(1, "lines")) +
    geom_point(data = targetPl, aes_string(shape = "Variable")) +
    scale_fill_manual(values = colors,
                      name = "Drivers",
                      breaks = breaks) +
    scale_x_continuous(breaks = yearTicks)


  # make facets
  if (length(getCells(data)) > 1) {
    pl <- pl +  facet_wrap("Region", scales = "free", nrow = rows, ncol = cols)
  }
  if (length(getNames(data, dim = 2)) > 1) {
    pl <- pl +  facet_wrap("Scenario", scales = "fixed", nrow = rows, ncol = cols)
  }

  # add labels
  ylabel <- paste0("Change in ", targetName, " per timestep,\n", unit)
  if (isTRUE(reducedYlabel)) ylabel <- unit
  if (is.null(title)) title <- paste0("Change over time and decomposition to\nmain drivers of ", targetName)
  pl <- pl + labs(y = ylabel,
                  title = title)
  if (title == "off") pl <- pl + labs(title = "")
  if (isFALSE(labels)) pl <- pl + theme(axis.title.y = element_blank(), axis.title.x = element_blank())

  # add vertical line
  if (!is.null(addVline)) pl <- pl + geom_vline(xintercept = addVline, size = 0.5, colour = "gray70")

  # position legend
  if (isFALSE(legend)) pl <- pl + theme(legend.position = "none")
  if (!requireNamespace("lemon", quietly = TRUE)) {
    if (isTRUE(lemonLegend)) warning("Package 'lemon' not available, legend cannot be repositioned.")
    lemonLegend <- FALSE
  }
  if (isTRUE(lemonLegend)) {
    if (rows * cols > numScenarios) pl <- lemon::reposition_legend(pl, position = "center",
                                                                   panel = paste0("panel-", cols, "-", rows))
  }

  # print and/or save plot
  if (isTRUE(printPlot)) print(pl)
  if (!is.null(filename)) ggsave(paste0(filename, ".png"), pl, width = cols * 5, height = rows * 4)

  return(pl)
}
