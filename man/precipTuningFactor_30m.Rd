\name{precipTuningFactor_30m}
\alias{precipTuningFactor_30m}
\docType{data}
\title{
Data: Precipitation tuning (30m)
}
\description{
Distributed precipitation tuning data for the Liligo Glacier (Karakoram, Pakistan)
}
\usage{data(precipTuningFactor_30m)}
\format{An object of class \code{'RasterLayer'}.}
\details{
Values: <1 = precipitation decrease, 1 = default, >1 = precipitation increase
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\examples{
data(precipTuningFactor_30m)
plot(precipTuningFactor_30m)
}
\keyword{datasets}
