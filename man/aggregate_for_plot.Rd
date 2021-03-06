% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggregate_for_plot.R
\name{aggregate_for_plot}
\alias{aggregate_for_plot}
\title{aggregate_for_plot}
\usage{
aggregate_for_plot(plotdata,
dimension="region",region_plotorder=NULL)
}
\arguments{
\item{plotdata}{An MAgPIE object}

\item{dimension}{by "region", "name" or "year"}

\item{region_plotorder}{allows to change the order of the regions by giving
a vector with the order}
}
\value{
A MAgPIE object of the same dimensions as plotdata
}
\description{
Aggregates Data for an area chart. The value of all underlying
regions/years/timesteps are added to the value of the current
region/year/timestep.
}
\examples{
\dontrun{
data(population_magpie)
#no aggregation
scratch_plot(population_magpie[,,2])
#aggregation
scratch_plot(aggregate_for_plot(population_magpie[,,2],region_plotorder=c(10,1:9)))
}
}
\author{
Benjamin Bodirsky
}
