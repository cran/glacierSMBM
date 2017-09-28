\name{glacierSMBM-package}
\alias{glacierSMBM-package}
\docType{package}
\title{
  Package: Glacier Surface Mass Balance Model
}
\description{
A fully distributed glacier surface mass balance model developed for the simulation of accumulation and ablation processes on debris-free as well as debris-covered glaciers.
}
\details{
  \tabular{ll}{
    Package: \tab glacierSMBM\cr
    Type: \tab Package\cr
    Version: \tab 0.1\cr
    Date: \tab 2017-09-26\cr
    License: \tab GPL (>= 3)\cr
    Depends: \tab methods, raster, sp, udunits2\cr
  }
}
\author{
    Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
    
    Christoph Mayer
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\keyword{ package }
\seealso{
\code{\link{glacierSMBM}}

\code{\link{inputGlacierSMBM-class}}

\code{\link{glacialMelt}}, \code{\link{snowMelt}}, \code{\link{iceMelt}},

\code{\link{debrisCoveredIceMelt}}, \code{\link{snowFall}},
}
\examples{
# Load the provided RasterLayer objects as exemplary
# input for the function
data(airTemperature_30m_daily, airDensity_30m_daily,
    netRad_30m_daily, glacierMask_30m, iceMask_30m, firnMask_30m,
    debrisMask_30m, debrisThickness_30m, precipTuningFactor_30m,
    snowFall_30m_daily, package = "glacierSMBM")
# Individual RasterLayer objects should be loaded or
# created using the function raster()

# create a three-day virtual meteorological data set
AirT <- stack(airTemperature_30m_daily,
    airTemperature_30m_daily * 0.99,
    airTemperature_30m_daily * 1.01)
NetRad <- stack(netRad_30m_daily,netRad_30m_daily * 0.99,
    netRad_30m_daily * 1.01)
Snowfall <- stack(snowFall_30m_daily, snowFall_30m_daily * 2,
    snowFall_30m_daily * 0.3)

# create a new object of class "inputGlacierSMBM" which is
# requested as input for the glacier surface mass balance model
InputGlacierSMBM <- new("inputGlacierSMBM")

# Add the required data and information to the respective
# slots of the new object (for additional setting options read
# the help section of class "inputGlacierSMBM")
# Create a numeric vector containing date and time of
# the meteorological input data
InputGlacierSMBM@date <- seq.POSIXt(ISOdate(2011,8,15),
    ISOdate(2011,8,17), "days")
InputGlacierSMBM@decimalPlaces <- 4
InputGlacierSMBM@airT <- AirT
InputGlacierSMBM@airDensity <- stack(airDensity_30m_daily)
InputGlacierSMBM@netRad <- NetRad
InputGlacierSMBM@snowfall <- Snowfall
InputGlacierSMBM@glacierMask <- stack(glacierMask_30m)
InputGlacierSMBM@iceMask <- stack(iceMask_30m)
InputGlacierSMBM@firnMask <- stack(firnMask_30m)
InputGlacierSMBM@debrisMask <- stack(debrisMask_30m)
InputGlacierSMBM@debrisThickness <- stack(debrisThickness_30m)
InputGlacierSMBM@disTuningFacPrecip <- stack(precipTuningFactor_30m)

# Calculate glacier surface mass balance using standard settings,
# but suppress to write any output
InputGlacierSMBM@writeOutput <- rep(0, 17)

\dontrun{
output <- glacierSMBM(inputGlacierSMBM = InputGlacierSMBM)

# Plot output
plot(output, main = "glacier surface mass balance",
    legend.args=list(text='Mass balance (m d-1)', side=3,
    line=1.5), col = colorRampPalette(c("darkred", "red",
    "blue"))(100))

}

# Calculate glacier surface mass balance using modified settings
# Change thermal conductivity and wind speed applied in the
# implemented function "debrisCoveredIceMelt"
InputGlacierSMBM@thermalConductivity <- 1.5
InputGlacierSMBM@windSpeed <- 5

\dontrun{
output <- glacierSMBM(inputGlacierSMBM = InputGlacierSMBM)

# Plot output
plot(output, main = "glacier surface mass balance",
    legend.args=list(text='Mass balance (m d-1)', side=3,
    line=1.5), col = colorRampPalette(c("darkred", "red",
    "blue"))(100))
}
}