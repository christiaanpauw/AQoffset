% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/raster_dist_sum.R
\name{raster_dist_sum}
\alias{raster_dist_sum}
\title{Raster Distribution Summary}
\usage{
raster_dist_sum(s, minmax = FALSE, trim = 1/100, meanonly = FALSE)
}
\arguments{
\item{s}{A raster stack}

\item{minmax}{Logical. Do you want minimum and maximum}

\item{trim}{Proportion of data excluded from range}

\item{meanonly}{logical Export only the mean}
}
\value{
A raster
}
\description{
Summarise a many layered raster stack into a 7 point summary 
stack. It gives the the following percentiles: 1%, 25%, median, 75% and 99%
and mean, standard deviation, and inter quartile range and arbitrary user specified 
range. Optionally provides minimum and maximum
}

