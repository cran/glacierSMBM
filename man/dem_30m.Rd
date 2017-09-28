\name{dem_30m}
\alias{dem_30m}
\docType{data}
\title{
Data: DEM (30m)
}
\description{
Digital elevation model of the Liligo Glacier (Karakoram, Pakistan)
}
\usage{data(dem_30m)}
\format{An object of class \code{'RasterLayer'}.}
\details{
Dataset: The High Asia Refined Analysis (TU Berlin, Chair of Climatology)

Pixel resolution: 10 km

Unit: m

Projection: UTM 43 N

Note: The original dataset was resampled to a spatial resolution of 30 m using the function \code{\link{resample}}.
}
\source{
\href{http://www.klima-ds.tu-berlin.de/har/}{High Asia Refined Analysis}
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\examples{
data(dem_30m)
plot(dem_30m)
}
\keyword{datasets}
