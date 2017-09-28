\name{firnMask_30m}
\alias{firnMask_30m}
\docType{data}
\title{
Data: Firn mask (30m)
}
\description{
Firn and snow cover at the Liligo Glacier (Karakoram, Pakistan)
}
\usage{data(firnMask_30m)}
\format{An object of class \code{'RasterLayer'}.}
\details{
Dataset: Landsat 5

Date: 2011-08-10

Pixel resolution: 30 m

1 = firn, 0 = no firn

Projection: UTM 43 N

Note: The firn and snow cover distribution was derived from a Landsat 5 image (for more information see Groos et al., submitted).
}
\source{
\href{https://earthexplorer.usgs.gov/}{USGS EarthExplorer}
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\examples{
data(firnMask_30m)
plot(firnMask_30m)
}
\keyword{datasets}
