% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_impact_table.R
\name{make_impact_table}
\alias{make_impact_table}
\title{make_impact_table}
\usage{
make_impact_table(base, proj, def, pmvar = "pm", impname = "Deficit")
}
\arguments{
\item{base}{Raster}

\item{proj}{Raster}

\item{def}{Raster}

\item{pmvar}{Character Name of pollutant to select. Must match in names(base) and names(proj)}
}
\description{
Convenience function to make a table showing difference between
  baseline and project scenario
}

