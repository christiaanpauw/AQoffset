\name{AQoffset-package}
\alias{AQoffset-package}
\alias{AQoffset}
\docType{package}
\title{
Makes calculation needed for air quality offsetting
}
\description{
Uses rasterised outputs of dispersion models or other air quality models and population data to calculate air quality impacts and the inpact of offsets
}
\details{
\tabular{ll}{
Package: \tab AQoffset\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-10\cr
License: \tab GPL \cr
}

}
\author{
Christiaan. J. Pauw

Maintainer: Christiaan. J. Pauw
}
\references{
Air Quality Offsets Protocol
}

\keyword{ Air Quality Offset }


\examples{
# not run
kwaza_API <- rasterAPI(all_base, idpos = 1, polpos = 2, cyclepos = 5)
swi_base <- SWI(all_base, pop = people)
}
