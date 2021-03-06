% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comparisonplot.R
\name{comparisonplot}
\alias{comparisonplot}
\title{comparisonplot}
\usage{
comparisonplot(measured_data, magpie_data, compare_categories=FALSE,
main="Comparison", ylab="", design="horizontal",
output_folder="",legend.text=c("Measured Data","Magpie Output"),
min_height=NULL, max_height=NULL)
}
\arguments{
\item{measured_data}{An MAgPIE object}

\item{magpie_data}{Another MAgPIE object}

\item{compare_categories}{With FALSE each name-column of both datasets is
compared in a seperate plot, and each bar represents a region. With TRUE,
each region is plotted in a seperate plot, while each bar represents a
name-column.}

\item{main}{Title appering at the head of the page}

\item{ylab}{a title for the y axis}

\item{design}{see get_layout_parameters() for further details}

\item{output_folder}{Outputfolder of pdf file}

\item{legend.text}{Legende for the description of the different datasets}

\item{min_height}{Minimum Height of the y-axis. If NULL, heighest value is
taken.}

\item{max_height}{Maximum Height of the y-axis. If NULL, zero is taken.}
}
\value{
Compares to sets of historical data in the form of MAgPIE objects.
Dimensions are time and value. Regions and the name of the data columns have
to be the same if data shall be compared.
}
\description{
Compares two Magpie objects of the same structure with a barplot
}
\examples{
\dontrun{
data(population_magpie)
 #Comparison requires two objects with same dimension names!
# Suppose the A2 dataset would be Magpie-Output, while the B1 would be the real data
magpie_output<-population_magpie[,1:4,1]
dimnames(magpie_output)[[3]]<-c("Population")
real_data<-population_magpie[,1:3,2]
dimnames(real_data)[[3]]<-c("Population")
# Note that only the timeslots where plotted where information on both datasets existed!
comparisonplot(measured_data=real_data,magpie_data=magpie_output,compare_categories=FALSE)
}
}
\seealso{
\code{\link{histoplot},\link{scratch_plot},\link{get_layout_parameters}}
}
\author{
Benjamin Bodirsky
}
