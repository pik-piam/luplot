% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotmap.R
\name{plotmap}
\alias{plotmap}
\title{Function to plot maps}
\usage{
plotmap(x, map_limits_x = c(-180, 180), map_limits_y = c(-60, 90),
legend_discrete = FALSE, legend_range = c(0, 1), legend_colours =
terrain.colors(1000)[1000:1], legend_fontsize = 2, legend_title = "Legend",
legend_factor = 1, legend_show_first_break = TRUE, legend_xy = c(-155, -10),
legend_margin = 10, legend_horizontal = FALSE, legend_width =
3,legend_outliers = c(NULL,NULL), main = NULL, title_fontsize = 3, texts =
NULL, cex.corr = 0.5, show_world_contours = TRUE, show_country_contours =
TRUE, contours_width = 1, contours_resolution = 1, show_tropic = FALSE,
show_water = TRUE, water_color = "#ADD8E6")
}
\arguments{
\item{x}{map or MAgPIE object}

\item{map_limits_x}{x range in Degree of the map that should be plotted}

\item{map_limits_y}{y range in Degree of the map that should be plotted}

\item{legend_discrete}{TRUE -> discrete legend, FALSE -> continuous legend}

\item{legend_range}{Vector containing lower and upper bound of values that
should be plotted}

\item{legend_colours}{A vector of colors that should be used for plotting}

\item{legend_fontsize}{(only discrete) legend font size}

\item{legend_title}{(only discrete) legend title}

\item{legend_factor}{(only discrete) correction factor for legend values}

\item{legend_show_first_break}{(only discrete) should the first break
printed in the legend?}

\item{legend_xy}{(only discrete) coordinates of the legend on map}

\item{legend_margin}{(only continuous) legend margins}

\item{legend_horizontal}{(only continuous) If TRUE the legend is printed is
on the right side, otherwise below the map}

\item{legend_width}{(only continuous) legend width}

\item{legend_outliers}{colors for values <= and >= the legend-range. if
null, legend_range[1] and legend_range[2] are used for outliers.}

\item{main}{map title}

\item{title_fontsize}{title font size}

\item{texts}{A list of 4n vectors of the structure c(x,y,text,font size)
with coordinates x and y, a text and a font size, e.g. c(0,0,blablub,0.5)}

\item{cex.corr}{General correction of all cex settings}

\item{show_world_contours}{Switch to show continent contours}

\item{show_country_contours}{Switch to show country contours}

\item{contours_width}{contours width}

\item{contours_resolution}{Detailedness of contours. 1 means that any
available data point is printed, 2 that every second is printed and so on.}

\item{show_tropic}{Switch to show tropics}

\item{show_water}{Switch to show water}

\item{water_color}{Water color}
}
\description{
Plots maps of map objects or MAgPIE objects
}
\details{
This function is the successor of the magpie_mapping toolset. In the
conversion process some functionality was removed whereas other
functionality was added, therefore the new function is not fully compatible
to the old implementation
}
\examples{
#plotmap(x)
}
\seealso{
\code{\link{magpie2map}}
}
\author{
Jan Philipp Dietrich
}
