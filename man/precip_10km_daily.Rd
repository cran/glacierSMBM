\name{precip_10km_daily}
\alias{precip_10km_daily}
\docType{data}
\title{
Data: Precipitation (10km, daily)
}
\description{
Distributed precipitation at the Liligo Glacier (Karakoram, Pakistan)
}
\usage{data(precip_10km_daily)}
\format{An object of class \code{'RasterLayer'}.}
\details{
Dataset: The High Asia Refined Analysis (TU Berlin, Chair of Climatology)

Date: 2011-08-15

Temporal resolution: daily

Pixel resolution: 10 km

Unit: m

Note: Projection: UTM 43 N
}
\source{
\href{http://www.klima-ds.tu-berlin.de/har/}{High Asia Refined Analysis}
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.

Maussion, F., Scherer, D., Moelg, T., Collier, E., Curio, J., and Finkelnburg, R. (2014). Precipitation Seasonality and Variability over the Tibetan Plateau as Resolved by the High Asia Reanalysis. Journal of Climate 27, 1910-1927.
}
\examples{
data(precip_10km_daily)
plot(precip_10km_daily)
}
\keyword{datasets}
