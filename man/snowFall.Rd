\name{snowFall}
\alias{snowFall}
\title{
Function: Snowfall model
}
\description{
A simple model to derive snowfall from precipitation and air temperature.
}
\usage{
snowFall(airT, precip, glacierMask,
    snowTransTempThreshold = 274.15, tuningFacPrecip = 1,
    disTuningFacPrecip = stack(), tuningFacAirT = 1,
    disTuningFacAirT = stack(), decimalPlaces = 4,
    outType = "mean", writeOutput = FALSE,
    outputName = "snowfall", tmpCreate = FALSE,
    tmpDir = "", outDir = "", ... )
}
\arguments{
  \item{airT}{
  An object of class \code{'RasterStack'}. Distributed air temperature (Kelvin or Celsius). For every time step.
}
  \item{precip}{
	An object of class \code{'RasterStack'}. Distributed precipitation (m). For every time step.
}
  \item{glacierMask}{
  An object of class \code{'RasterStack'}. Glacier area (1 = glacier, 0 = no glacier). Stationary or for every time step.
}
  \item{snowTransTempThreshold}{
  An object of class \code{'numeric'}. Temperature threshold (same unit as \code{'airT'}) for the transition from rain- to snowfall (default = 274.15 Kelvin).
}
  \item{tuningFacPrecip}{
	An object of class \code{'numeric'}. General precipitation tuning factor (<1 = snowfall decrease, 1 = default, >1 = snowfall increase).
}
  \item{disTuningFacPrecip}{
  An object of class \code{'numeric'}. Distributed precipitation tuning factor
  
  (\code{tuningFacPrecip}). Stationary or for every time step.
}
  \item{tuningFacAirT}{
  An object of class \code{'numeric'}. General air temperature tuning factor (<1 = temperature decrease, 1 = default, >1 = temperature increase).
}
  \item{disTuningFacAirT}{
	An object of class \code{'RasterStack'}. Distributed air temperature tuning factor (\code{'tuningFacAirT'}). Stationary or for every time step.
}
  \item{decimalPlaces}{
  An object of class \code{'numeric'}. Number of decimal places (default = 4).
}
  \item{outType}{
	An object of class \code{'character'}. Type of output to be returned by the function: "mean" (default) snowfall or "sum".
}
  \item{writeOutput}{
	An object of class \code{'logical'}. Determines whether the ouput shall be exported as \code{'RasterLayer'} (TRUE) or not (FALSE, default).
}
  \item{outputName}{
	An object of class \code{'character'}. File name for the output \code{'RasterLayer(s)'} (default = "dcIceMelt").
}
  \item{tmpCreate}{
  An object of class \code{'logical'}. Determines whether a temporary directory should be used (TRUE) or not (FALSE, default). Recommendend if large datasets are processed.
}
  \item{tmpDir}{
	An object of class \code{'character'}. Directory where processing files can be temporarily stored if \code{'tmpCreate'} = TRUE.
}
  \item{outDir}{
	An object of class \code{'character'}. Directory for the output files if \code{'writeOutput'} = TRUE.
}
  \item{\dots}{
  Further arguments.
}
}
\details{
A simple empirical air temperature threshold (e.g. 1 degree Celsius) is applied to derive snowfall from precipitation (e.g. Rohrer and Braun, 1994). Snowfall is defined as precipitation below the temperature threshold. For temperatures above the threshold, no precipitation is considered since refreezing and percolation processes have yet not been implemented in \code{'glacierSMBM'}. For more information see Groos et al. (submitted).
}
\value{
An object of class \code{'RasterLayer'} returning the calculated snowfall distribution (m timestep-1).
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.

Rohrer, M., and Braun, L. (1994). Long-Term Records of Snow Cover Water Equivalent in the Swiss Alps 2. Simulation. Nordic Hydrology 25, 65-78.
}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\note{
The following input variables are the requested minimum to run the model:
\itemize{
  \item \code{'airT'} (for every time step)
  \item \code{'precip'} (for every time step)
  \item \code{'glacierMask'} (stationary or for every time step)
  \item \code{'snowTransTempThreshold'} (parameter)

}
A default value (constant in space and time) is given for the additional arguments like \code{'tuningFacPrecip'} or \code{'tuningFacAirT'}. If desired, the default parameters can be modified. Furthermore, there is the option to pass distributed values (stationary or for every time step) instead of general values using the related 'dis*'-arguments like \code{'disTuningFacPrecip'} or \code{disTuningFacAirT}. In this case, the general parameter is ignored.

File format of written ouput: GeoTIFF.
}
\seealso{
\code{\link{glacialMelt}}
}
\examples{
# Load the provided RasterLayer objects as exemplary
# input for the function
# Individual RasterLayer objects should be loaded or
# created using the function raster()
data(airTemperature_30m_daily, precip_30m_daily,
    glacierMask_30m, package = "glacierSMBM")

# Include RasterLayer in RasterStack
AirTemperature_30m_daily <- stack(airTemperature_30m_daily)
Precip_30m_daily <- stack(precip_30m_daily)
GlacierMask_30m <- stack(glacierMask_30m)

# Calculate snow fall from air temperature and total
# precipitation using standard settings
output <- snowFall(airT = AirTemperature_30m_daily,
    precip = Precip_30m_daily, glacierMask = GlacierMask_30m)

# Plot output
plot(output, main = "snow fall",
    legend.args=list(text='Snowfall (m)', side=3, line=1.5))

# Calculate snow fall from air temperature and total
# precipitation using modified settings (e.g. snow transition
# is changed from 1 to 0 degree Celsius) and general
# precipitation tuning factor is set from 1 to 0.8)
output <- snowFall(airT = AirTemperature_30m_daily,
    precip = Precip_30m_daily, glacierMask = GlacierMask_30m,
    tuningFacPrecip = 0.8, snowTransTempThreshold = 273.15)

# Plot output
plot(output, main = "snow fall",
    legend.args=list(text='Snowfall (m)', side=3, line=1.5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
