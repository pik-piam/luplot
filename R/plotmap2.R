#' Function to plot maps using ggplot2
#'
#' Plots maps MAgPIE objects
#'
#' Makes use of ggplot2 map plotting capabilites
#'
#' @usage plotmap2(data,file=NULL,title="World map",legend_range=NULL,
#' legendname="Cell share",lowcol="grey95",midcol="orange",
#' highcol="darkred",midpoint=0.5, facet_grid="Year~Data1", nrow=NULL, ncol=NULL,
#' scale=2,breaks=TRUE,labs=TRUE,borders=TRUE,MAgPIE_regions=FALSE,
#' axis_text_col="black",legend_discrete=FALSE,legend_breaks=NULL,
#' show_percent=FALSE, sea=TRUE,land_colour="white",
#' legend_height=2,legend_width=NULL,text_size=12,
#' legend_position="right",facet_style="default",plot_height=10,plot_width=20)
#' @param data MAgPIE object
#' @param file File name the output should be written to using ggsave
#' @param title Title of the map
#' @param legend_range Legend range. Vector with minimum and maximum.
#' @param legendname Name of the legend
#' @param lowcol color for low values
#' @param midcol color for medium values
#' @param highcol color for high values
#' @param midpoint Value indicating where the transformation from lowcol to
#' highcol takes place.
#' @param facet_grid facets formula used in ggplot2 with default "Year~Data1". Falls back to facet_wrap for "Data1".
#' @param nrow number rows for facet_wrap
#' @param ncol number columns for facet_wrap
#' @param scale scaling of ggplot2 object before saving to file
#' @param breaks TRUE or FALSE
#' @param labs TRUE or FALSE
#' @param borders TRUE or FALSE
#' @param MAgPIE_regions TRUE or FALSE, if TRUE data is ignored
#' @param axis_text_col color of axis text and ticks; default value: "black"
#' @param legend_discrete TRUE or FALSE, discrete or continous legend
#' @param legend_breaks NULL (automatically) or vector with breaks
#' @param show_percent TRUE or FALSE, percent values for discrete lengeds
#' @param sea TRUE or FALSE, blue sea or just white background
#' @param land_colour Background colour for all land area. Defaults to white.
#' @param legend_height NULL or legend height in cm
#' @param legend_width NULL or legend width in cm
#' @param text_size text size of axis, legend and title
#' @param legend_position right (default), left, top, bottom or none
#' @param facet_style style of facets, default or paper
#' @param plot_height plot height in cm
#' @param plot_width plot width in cm
#' @author Florian Humpenoeder, David M Chen
#' @seealso \code{\link{plotmap}}
#' @examples
#' # plotmap2(data)
#' #' #@importFrom  RColorBrewer
#' @export
#' @importFrom ggplot2 ggplot aes_ geom_polygon scale_fill_manual waiver geom_raster scale_fill_gradient2
#' scale_fill_gradient theme coord_cartesian ggtitle element_rect element_line scale_x_continuous scale_y_continuous
#' geom_path element_text element_blank guides guide_colorbar ggsave unit
#' @importFrom grDevices colorRampPalette
#' @importFrom utils head tail
#' @importFrom mstools toolGetMappingCoord2Country


plotmap2 <- function(data, file = NULL, title = "World map", legend_range = NULL, legendname = "Cell share", # nolint: object_name_linter
                     lowcol = "grey95", midcol = "orange", highcol = "darkred", midpoint = 0.5,
                     facet_grid = "Year~Data1", nrow = NULL, ncol = NULL, scale = 2, breaks = TRUE, labs = TRUE, # nolint: object_name_linter
                     borders = TRUE, MAgPIE_regions = FALSE, axis_text_col = "black", legend_discrete = FALSE, # nolint: object_name_linter
                     legend_breaks = NULL, show_percent = FALSE, sea = TRUE, land_colour = "white", # nolint: object_name_linter
                     legend_height = 2, legend_width = NULL, text_size = 12, legend_position = "right", # nolint: object_name_linter
                     facet_style = "default", plot_height = 10, plot_width = 20) { # nolint: object_name_linter
  wrld_simpl_df <- NULL # nolint: object_name_linter
  data("world", envir = environment(), package = "luplot")
  if (MAgPIE_regions) {
    facet_grid <- NULL # nolint: object_name_linter
    map <- ggplot(wrld_simpl_df, aes_(~long, ~lat)) +
      geom_polygon(aes_(group = ~group, fill = ~magpie)) +
      scale_fill_manual("MAgPIE\nregion", values = c("purple3", "red3", "hotpink", "cyan3", "goldenrod1", "gray44",
                                                     "#8C5400FF", "darkorange2", "royalblue3", "green4"),
                        na.value = "white")
  } else {
    if (!is.list(data)) {
      temp <- data
      data <- list()
      data[["default"]] <- temp
    }
    if (is.null(legend_range)) {
      midpoint <- max(unlist(lapply(data, max, na.rm = TRUE))) * midpoint
    } else {
      data <- lapply(data, function(x) {
        x[which(x < legend_range[1])] <- legend_range[1]
        x[which(x > legend_range[2])] <- legend_range[2]
        return(x)
      })
    }
    if (any(unlist(lapply(data, function(x) return(is.null(attr(x, "coordinates"))))))) {
      if (length(getItems(data[[1]], dim = 1) == 67420)) {
        mapping <- toolGetMappingCoord2Country()
        mapping <- setNames(do.call(rbind.data.frame, strsplit(unlist(mapping[, "coords"]), "\\.")),
                            c("lon", "lat"))
        mapping <- apply(mapping, 2, function(y) as.numeric(gsub("p", "\\.", y)))
        data <- lapply(data, function(x) {
          attr(x, "coordinates") <- mapping
          return(x)
        })
      } else if (length(getItems(data[[1]], dim = 1) == 59199)) {
        data <- lapply(data, function(x) {
          attr(x, "coordinates") <- getCoordinates(degree = TRUE)
          return(x)
        })
      }
      warning(paste("Missing coordinates in attributes for at least one MAgPIE object.",
                    "Added coordinates in default MAgPIE cell order."))
    }


    data <- as.ggplot(data, asDate = FALSE)
    if (legend_discrete) {
      if (is.null(legend_breaks)) {
        data$Breaks <- as.character(data$Value)
        # replace NA's with a value that is not contained in data yet
        if (any(is.na(data$Breaks))) {
          data$Breaks[is.na(data$Breaks)] <- "No data"
        }
        data$Breaks <- as.factor(data$Breaks)
        legendLabels <- levels(data$Breaks)
      } else {
        tmp <- as.vector(data$Value)
        tmp[] <- length(legend_breaks) + 1
        legendLabels <- rep("", length(legend_breaks) + 1)
        legendLabels[length(legend_breaks) + 1] <- paste(">", legend_breaks[length(legend_breaks)])
        for (i in length(legend_breaks):2) {
          tmp[which(as.vector(data$Value) <= legend_breaks[i])] <- i
          legendLabels[i] <- paste(legend_breaks[i - 1], "-", legend_breaks[i])
        }
        tmp[which(as.vector(data$Value) < legend_breaks[1])] <- 1
        tmp[which(is.na(as.vector(data$Value)))] <- NA
        legendLabels[1] <- paste("<", legend_breaks[1])
        legendLabels <- legendLabels[as.numeric(rev(levels(as.factor(tmp))))]
        tmpchar <- as.character(tmp)
        tmpchar[is.na(tmpchar)] <- "No data"
        levels <- rev(levels(as.factor(tmp)))
        if ("No data" %in% tmpchar) {
          legendLabels <- c(legendLabels, "No data")
          levels <- c(levels, "No data")
        }
        data$Breaks <- factor(tmpchar, levels = levels)
      }

      if ("No data" %in% legendLabels) {
        colours <- c(colorRampPalette(c(highcol, midcol, lowcol))(length(legendLabels) - 1), "grey")
      } else {
        colours <- colorRampPalette(c(highcol, midcol, lowcol))(length(legendLabels))
      }

      if (show_percent) {
        tmp <- table(data$Breaks)
        if ("No data" %in% names(tmp)) tmp <- tmp[-which(names(tmp) == "No data")]
        percent <- round(tmp / sum(tmp) * 100, 1)
        percent <- paste("(", percent, "%)", sep = "")
        legendLabels[which(legendLabels != "No data")] <- paste(legendLabels[which(legendLabels != "No data")],
                                                                percent, sep = "     ")
      }

    }
    if (!is.null(legend_breaks)) {
      labels <- c(bquote("" <= .(head(legend_breaks, 1))), legend_breaks[2:(length(legend_breaks) - 1)],
                  bquote("" >= .(tail(legend_breaks, 1))))
    } else {
      legend_breaks <- waiver() # nolint: object_name_linter
      labels <- waiver()
    }
    map <- ggplot(data, aes_(~x, ~y)) + geom_polygon(data = wrld_simpl_df,
                                                     aes_(~long, ~lat, group = ~group, fill = ~hole),
                                                     fill = land_colour)
    if (is.null(data$Breaks)) {
      if (!is.null(midcol)) {
        map <- map + geom_raster(aes_(fill = ~Value)) +
          scale_fill_gradient2(name = legendname, low = lowcol, mid = midcol, high = highcol,
                               midpoint = midpoint, limits = legend_range, breaks = legend_breaks,
                               labels = labels, na.value = "grey")
      } else {
        map <- map + geom_raster(aes_(fill = ~Value)) +
          scale_fill_gradient(name = legendname, low = lowcol, high = highcol, limits = legend_range,
                              breaks = legend_breaks, labels = labels, na.value = "grey")
      }
    } else {
      map <- map + geom_raster(aes_(fill = ~Breaks)) +
        scale_fill_manual(name = legendname, values = colours, labels = legendLabels, na.value = "yellow")
    }
    if (!is.null(legend_height)) map <- map + theme(legend.key.height = unit(legend_height, "cm"))
    if (!is.null(legend_width)) map <- map + theme(legend.key.width = unit(legend_width, "cm"))
  }
  map <- map +
    coord_cartesian(xlim = c(-180, 180), ylim = c(-58, 86)) +
    theme(aspect.ratio = 0.5) +
    ggtitle(title)
  if (sea) {
    map <- map + theme(panel.background = element_rect(fill = "lightsteelblue2"))
  } else {
    map <- map + theme(panel.background = element_rect(fill = "white", colour = "black")) +
      theme(panel.grid.major = element_line(colour = "grey80"), panel.grid.minor = element_line(colour = "grey90"))
  }
  if (!is.null(facet_grid)) {
    if (substr(facet_grid, 1, 1) == "~") map <- map + facet_wrap(facet_grid, nrow = nrow, ncol = ncol)
    else map <- map + facet_grid(facet_grid)
  }
  if (breaks) {
    map <- map + scale_x_continuous(breaks = c(-90, 0, 90)) +
      scale_y_continuous(breaks = c(-66, -38, -23, 0, 23, 38, 66))
  } else {
    map <- map + scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL)
  }
  if (labs) map <- map + labs(x = "Longitude", y = "Latitude")
  else map <- map + labs(y = NULL, x = NULL)
  if (borders) map <- map + geom_path(data = wrld_simpl_df, aes_(~long, ~lat, group = ~group, fill = NULL),
                                      color = "grey10", size = 0.1)
  if (!is.null(axis_text_col)) map <- map + theme(axis.text = element_text(colour = axis_text_col),
                                                  axis.ticks = element_line(colour = axis_text_col))
  map <- map + theme(panel.grid.minor = element_line(colour = "white"),
                     plot.title = element_text(size = text_size + 4, face = "bold", vjust = 1.5),
                     legend.position = legend_position, legend.title = element_text(size = text_size, face = "bold"),
                     legend.text = element_text(size = text_size),
                     axis.title.y = element_text(angle = 90, size = text_size, vjust = 0.3),
                     axis.text.y = element_text(size = text_size - 2),
                     axis.title.x = element_text(size = text_size, vjust = -0.3),
                     axis.text.x = element_text(size = text_size - 2, vjust = 0.5))
  if (facet_style == "default") {
    map <- map + theme(strip.text.x = element_text(size = text_size - 1),
                       strip.text.y = element_text(size = text_size - 1))
  } else if (facet_style == "paper") {
    map <- map + theme(strip.text.x = element_text(size = text_size, face = "bold"),
                       strip.text.y = element_text(size = text_size, face = "bold")) +
      theme(strip.background = element_blank())
  }
  if (legend_position %in% c("top", "bottom")) {
    map <- map + guides(fill = guide_colorbar(title.position = "top")) + theme(legend.box.just = "left")
  }
  if (!is.null(file)) {
    ggsave(file, map, scale = scale, limitsize = FALSE, units = "cm", height = plot_height, width = plot_width)
  } else {
    return(map)
  }
}
