\name{airTemperature_30m_hourly}
\alias{airTemperature_30m_hourly}
\docType{data}
\title{
Data: Air temperature (30m, hourly)
}
\description{
Distributed air temperature at the Liligo Glacier (Karakoram, Pakistan)
}
\usage{data(airTemperature_30m_hourly)}
\format{An object of class \code{'RasterLayer'}.}
\details{
Dataset: The High Asia Refined Analysis (TU Berlin, Chair of Climatology)

Date: 2011-08-15

Temporal resolution: hourly

Pixel resolution: 10 km

Unit: K

Projection: UTM 43 N

Note: The original dataset was interpolated to a spatial resolution of 30 m using the function \code{\link{interpolateAirT}}.
}
\source{
\href{http://www.klima-ds.tu-berlin.de/har/}{High Asia Refined Analysis}
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.

Maussion, F., Scherer, D., Moelg, T., Collier, E., Curio, J., and Finkelnburg, R. (2014). Precipitation Seasonality and Variability over the Tibetan Plateau as Resolved by the High Asia Reanalysis. Journal of Climate 27, 1910-1927.
}
\examples{
data(airTemperature_30m_hourly)
plot(airTemperature_30m_hourly)
}
\keyword{datasets}
