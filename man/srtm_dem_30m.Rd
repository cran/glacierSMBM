\name{srtm_dem_30m}
\alias{srtm_dem_30m}
\docType{data}
\title{
Data: SRTM DEM (30m)
}
\description{
Digital elevation model of the Liligo Glacier (Karakoram, Pakistan)
}
\usage{data(srtm_dem_30m)}
\format{An object of class \code{'RasterLayer'}.}
\details{
Dataset: SRTM DEM

Pixel resolution: 30m

Unit: m

Projection: UTM 43 N
}
\source{
\href{https://earthexplorer.usgs.gov/}{USGS EarthExplorer}
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\examples{
data(srtm_dem_30m)
plot(srtm_dem_30m)
}
\keyword{datasets}
