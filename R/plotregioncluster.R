#' @title plotregionscluster
#' @description Plots world map showing world regions and cluster.
#'
#' @param x data frame with 59199 or 67420 cells in MAgPIE order 
#'          that contains regional and cluster information.
#'          For 59199 cells: format of cluster dimension \code{region.cluster}
#'          where \code{region} is the name of the region
#'          and \code{cluster} is the number of the cluster.
#'          For 67420 cells: cell dimension with coordinate and country name information
#'          and format of cluster dimension \code{region.cluster}
#' @param cells "magpiecell" for 59199 cells and "lpjcell" for 67420 cells
#' @seealso \code{\link{plotspam}}
#' @importFrom data.table data.table
#' @importFrom ggplot2 scale_fill_manual theme_linedraw scale_alpha_continuous geom_tile
#' @importFrom mip plotstyle
#' @examples
#' x <- paste(rep(paste0("REG", 1:5), each = 12000), rep(1:25, each = 2400), sep = ".")[1:59199]
#' plotregionscluster(x)
#' @export

plotregionscluster <- function(x, cells = "magpiecell") {

  # Internal function that prepares data frame for plot
  .prepData <- function(x, cells) {

    if (cells == "magpiecell") {

      # add lon/lat information
      if (!is.vector(x)) {
        x <- x$cluster
      }
      x <- data.table(cbind(mag05cells, cluster = x))

      # add region column
      x$region <- sub("\\..*$", "", x$cluster)

    } else if (cells == "lpjcell") {
      # create data frame with lon-lat information
      x$coords <- gsub(".{1}$", "", gsub("[A-Z]", "", x$cell))
      x$lat <- as.numeric(gsub("p", ".", gsub(".*\\.", "", x$coords)))
      x$lon <- as.numeric(gsub("p", ".", gsub("\\..*", "", x$coords)))

    } else {
      stop("Please select cellular resolution for cluster map
            in the plotregioncluster() function")
    }

    # convert cluster and region columns to factors
    x$region  <- as.factor(x$region)
    x$cluster <- as.factor(x$cluster)

    z <- data.frame(reg = as.factor(sub("\\..*$", "", levels(x$cluster))),
                    cluster = as.numeric(sub("^.*\\.", "", levels(x$cluster))))
    ncluster <- NULL
    for (r in levels(z$reg)) {
      ncluster[r] <- sum(z$reg == r)
      z$cluster[z$reg == r] <- round(seq(from = 1 / ncluster[r], to = 1, by = 1 / ncluster[r]), 2)
    }
    x$alpha <- z$cluster[x$cluster]

    # legend
    legendText <- paste0(paste(levels(x$region), ncluster[levels(x$region)], sep = " ("), ")")
    names(legendText) <- levels(x$region)
    attr(x, "legend_text") <- legendText

    return(x)
  }

  # extract world map
  simpleWorldDataframe <- NULL
  data("world", envir = environment(), package = "luplot")

  x <- .prepData(x, cells = cells)

  # plot map of regions and clusters 
  map <- ggplot(x, aes_(~lon, ~lat)) +
         geom_polygon(data = simpleWorldDataframe,
                      aes_(~long, ~lat, group = ~group, fill = ~hole),
                      fill = "lightgrey") +
         geom_tile(aes_(fill = ~region, alpha = ~alpha)) +
         geom_path(data = simpleWorldDataframe,
                   aes_(~long, ~lat, group = ~group, fill = NULL),
                   color = "grey10", size = 0.2) +
         scale_alpha_continuous(range = range(x$alpha)) +
         scale_fill_manual(values = mip::plotstyle(levels(x$region)),
                           labels = attr(x, "legend_text")) +
         guides(fill = guide_legend(title = "Region (number of cluster)",
                                    nrow = 2),
                alpha = "none") +
         theme_linedraw() +
         theme(aspect.ratio = 0.6,
               legend.position = c(0.01, 0.01),
               legend.justification = c(0, 0),
               legend.background = element_rect(color = "black",
                                                fill = "white",
                                                linewidth = 1,
                                                linetype = "solid"))
  return(map)
}
