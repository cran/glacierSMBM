\name{debrisThickness_30m}
\alias{debrisThickness_30m}
\docType{data}
\title{
Data: Debris thickness (30m)
}
\description{
Distributed debris thickness at the Liligo Glacier (Karakoram, Pakistan)
}
\usage{data(debrisThickness_30m)}
\format{An object of class \code{'RasterLayer'}.}
\details{
Dataset: Landsat 5

Date: 2011-08-10

Pixel resolution: 30 m

Unit: m

Projection: UTM 43 N

Note: The debris thickness distribution was derived from satellite-based land surface temperatures and meteorological data using the function \code{\link{debrisThicknessPhy}} (for more information see Groos et al., submitted).
}
\source{
\href{https://earthexplorer.usgs.gov/}{USGS EarthExplorer}
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\examples{
data(debrisThickness_30m)
plot(debrisThickness_30m)
}
\keyword{datasets}