\name{glacierMask_30m}
\alias{glacierMask_30m}
\docType{data}
\title{
Data: Glacier mask (30m)
}
\description{
Glacier outlines of the Liligo Glacier (Karakoram, Pakistan)
}
\usage{data(glacierMask_30m)}
\format{An object of class \code{'RasterLayer'}.}
\details{
Dataset: Landsat 5

Date: 2011-08-10

Pixel resolution: 30 m

1 = glacier, 0 = no glacier

Projection: UTM 43 N

Note: The glacier outlines were derived from a set of high-resolution satellite images (for more information see Minora et al., 2013; Groos et al., submitted).
}
\source{
\href{https://earthexplorer.usgs.gov/}{USGS EarthExplorer}
}
\references{
Minora, U., Bocchiola, D., D'Agata, C., Maragno, D., Mayer, C., Lambrecht, A., Mosconi, B., Vuillermoz, E., Senese, A., Compostella, C., et al. (2013). 2001-2010 glacier changes in the Central Karakoram National Park: a contribution to evaluate the magnitude and rate of the "Karakoram anomaly." The Cryosphere Discussions 7, 2891-2941.

Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\examples{
data(glacierMask_30m)
plot(glacierMask_30m)
}
\keyword{datasets}
