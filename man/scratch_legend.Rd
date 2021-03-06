% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scratch_legend.R
\name{scratch_legend}
\alias{scratch_legend}
\title{scratch_legend}
\usage{
scratch_legend(plotdata, large=FALSE, color=NULL,
hatching_density=NULL, hatching_angle=NULL, lty=NULL,add=FALSE,pch=NULL)
}
\arguments{
\item{plotdata}{An MAgPIE object to be visualized}

\item{large}{TRUE: font-size for postdocs :P}

\item{color}{colors of the different legend items}

\item{hatching_density}{for area graphs}

\item{hatching_angle}{for area graphs}

\item{lty}{vector of line-types, see par, only used for line-charts}

\item{add}{"region_name" makes an area-chart out of the MAgPIE object, with
the value of regions and names being plotted on top of each other.  "region"
makes an area-chart out of the MAgPIE object, with the value of only regions
being plotted on top of each other, while the name-slices are plotted in
front of each other with a transparency of 50 percent.}

\item{pch}{vector of dot-types, see par, only used for line-charts}
}
\value{
visualisation of legend
}
\description{
Function to create a nice legend for a plot. It was taken out of
scratch_plot so it can be used by other plot functions too.
}
\examples{
\dontrun{
data(population_magpie)
  scratch_plot(population_magpie,legend=FALSE)
  scratch_legend(population_magpie)
}
}
\seealso{
\code{\link{scratch_plot}}
}
\author{
Benjamin Bodirsky
}
