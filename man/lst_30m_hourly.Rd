\name{lst_30m_hourly}
\alias{lst_30m_hourly}
\docType{data}
\title{
Data: Land surface temperature (30m)
}
\description{
Distributed land surface temperature at the Liligo Glacier (Karakoram, Pakistan)
}
\usage{data(lst_30m_hourly)}
\format{An object of class \code{'RasterLayer'}.}
\details{
Dataset: Landsat 5

Date: 2011-08-10

Pixel resolution: 30 m

Unit: K

Projection: UTM 43 N

Note: The distributed land surface temperature was derived from thermal band information of a Landsat 5 image (for more information see Groos et al., submitted).
}
\source{
\href{https://earthexplorer.usgs.gov/}{USGS EarthExplorer}
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\examples{
data(lst_30m_hourly)
plot(lst_30m_hourly)
}
\keyword{datasets}
