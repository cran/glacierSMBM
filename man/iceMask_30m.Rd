\name{iceMask_30m}
\alias{iceMask_30m}
\docType{data}
\title{
Data: Ice mask (30m)
}
\description{
Bare ice at the Liligo Glacier (Karakoram, Pakistan)
}
\usage{data(iceMask_30m)}
\format{An object of class \code{'RasterLayer'}.}
\details{
Dataset: Landsat 5

Date: 2011-08-10

Pixel resolution: 30 m

1 = bare ice, 0 = ice covered by debris, firn or snow

Projection: UTM 43 N

Note: The bare ice distribution was derived from a Landsat 5 image (for more information see Groos et al., submitted).
}
\source{
\href{https://earthexplorer.usgs.gov/}{USGS EarthExplorer}
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\examples{
data(iceMask_30m)
plot(iceMask_30m)
}
\keyword{datasets}
