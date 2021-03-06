% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_exceed.R
\name{count_exceed}
\alias{count_exceed}
\title{Count Exceed}
\usage{
count_exceed(s, pol = NULL, min = 0, max = 100, by = 10, knip = FALSE)
}
\arguments{
\item{s}{A raster or raster stack.}

\item{pol}{A pollutant name as a character string that matches a layer
in the raster stack s.}

\item{min}{Numeric. Value of the first cut point.}

\item{max}{Numeric. Value of the maximum cut point.}

\item{by}{Numeric. Value that specifies the increment of the sequence.}

\item{knip}{Logical. Create a new extent with NA-only columns cut out if TRUE.}
}
\value{
A raster
}
\description{
Count excedences of user- provided values in raster
}

