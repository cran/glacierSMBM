\name{snowMelt}
\alias{snowMelt}
\title{
Function: Snow melt model
}
\description{
A simple model to calculate snow melt based on empirical melting factors.
}
\usage{
snowMelt(airT, netRad, glacierMask, snowMask,  tUnit = "K",
    snowTMF = 45*10^-4, disSnowTMF = stack(),
    snowRMF = 0.53*10^-4, disSnowRMF = stack(),
    tuningFacAirT = 1, disTuningFacAirT = stack(),
    decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
    outputName = "snowMelt", tmpCreate = FALSE,
    tmpDir = "", outDir = "", ... )
}
\arguments{
  \item{airT}{
  An object of class \code{'RasterStack'}. Distributed air temperature (Kelvin or degree Celsius). For every time step.
}
  \item{netRad}{
  An object of class \code{'RasterStack'}. Distributed net radiation (W m-2). For every time step.
}
  \item{glacierMask}{
  An object of class \code{'RasterStack'}. Glacier area (1 = glacier, 0 = no glacier). Stationary or for every time step.
}
  \item{snowMask}{
	An object of class \code{'RasterStack'}. Area of supraglacial snow or firn (1 = snow or firn, 0 = no snow or firn). Stationary or for every time step.
}
  \item{tUnit}{
  An object of class \code{'character'}. Unit ("K" = Kelvin, "C" = degree Celsius) of air temperature (default = "K").
}
  \item{snowTMF}{
	An object of class \code{'numeric'}. Temperature melting factor (m K-1 timestep-1) of snow (default = 45*10^-4).
}
  \item{disSnowTMF}{
  An object of class \code{'RasterStack'}. Distributed temperature melting factor (m K-1 timestep-1) of snow. Stationary or for every time step.
}
  \item{snowRMF}{
	An object of class \code{'numeric'}. Radiative melting factor (m K-1 timestep-1) of snow (default = 0.53*10^-4).
}
  \item{disSnowRMF}{
  An object of class \code{'RasterStack'}. Distributed radiative temperature melting factor (m K-1 timestep-1) of snow. Stationary or for every time step.
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
	An object of class \code{'character'}. Type of output to be returned by the function: "mean" (default) snow melt or "sum".
}
  \item{writeOutput}{
	An object of class \code{'logical'}. Determines whether the ouput shall be exported as \code{'RasterLayer'} (TRUE) or not (FALSE, default).
}
  \item{outputName}{
	An object of class \code{'character'}. File name for the output \code{'RasterLayer(s)'} (default = "snowMelt").
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
An enhanced degree-day model (e.g. Hock, 2003, 2005; Pellicciotti & alii, 2005) is applied to quantify glacier mass loss ascribed to melted snow and/or firn using empirical temperature (TMF) and radiative melting factors (RMF). Fore more information please refer to the examples below or the original publication (Groos et al., submitted, Equation 10)
}
\value{
An object of class \code{'RasterLayer'} returning the calculated spatial distribution of snow melt (e.g. in m d-1, depending on \code{'tmpRes'}).
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.

Hock, R. (2003). Temperature index melt modelling in mountain areas. Journal of Hydrology 282, 104-115.

Hock, R. (2005). Glacier melt: a review of processes and their modelling. Progress in Physical Geography 29, 362-391.

Pellicciotti F., Brock B., Strasser U., Burlando P., Funk M. and Corripio J. (2005). An enhanced temperature-index glacier melt model including the shortwave radiation balance: development and testing for Haut Glacier d'Arolla, Switzerland. Journal of Glaciology, 51, 573-587.
}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\note{
The following input variables are the requested minimum to run the model:
\itemize{
  \item \code{'airT'} (for every time step)
  \item \code{'netRad'} (for every time step)
  \item \code{'glacierMask'} (stationary or for every time step)
  \item \code{'snowMask'} (stationary or for every time step)
}
A default value (constant in space and time) is given for each additional argument like \code{'snowTMF'} or \code{'snowRMF'}. If desired, the default parameters can be modified. Furthermore, there is the option to pass distributed values (stationary or for every time step) instead of general values using the related 'dis*'-arguments like \code{'disSnowTMF'} or \code{'disSnowRMF'}. In this case, the general parameter is ignored.

File format of written ouput: GeoTIFF.
}
\seealso{
\code{\link{glacialMelt}}, \code{\link{iceMelt}}, \code{\link{debrisCoveredIceMelt}}
}
\examples{

# Load the provided RasterLayer objects as exemplary
# input for the function
data(airTemperature_30m_daily, netRad_30m_hourly,
    glacierMask_30m, firnMask_30m, package = "glacierSMBM")
# Individual RasterLayer objects should be loaded or
# created using the function raster()

# Include RasterLayer in RasterStack
AirTemperature_30m_daily <- stack(airTemperature_30m_daily * 1.01)
NetRad_30m_hourly <- stack(netRad_30m_hourly)
GlacierMask_30m <- stack(glacierMask_30m)
FirnMask_30m <- stack(firnMask_30m)

# Calculate snow melt using standard settings
output <- snowMelt(airT = AirTemperature_30m_daily,
    netRad = NetRad_30m_hourly, glacierMask = GlacierMask_30m,
    snowMask = FirnMask_30m)

# Plot output
plot(output, main = "snow melt",
    legend.args=list(text='Snow melt (m d-1)', side=3, line=1.5))

# Calculate snow melt using modified setting (e.g. air
# temperature in degree Celsius instead of Kelvin; changes
# melting factors)
# Therefore exemplarily convert temperature from kelvin to
# degree Celsius
airTcelsius <- subset(AirTemperature_30m_daily, 1) - 273.15

# Include RasterLayer in RasterStack
airTcelsius <- stack(airTcelsius)

output <- snowMelt(airT = airTcelsius,
    netRad = NetRad_30m_hourly, glacierMask = GlacierMask_30m,
    tUnit = "C", snowMask = FirnMask_30m, snowTMF = 75*10^-4,
    snowRMF = 1.2*10^-4)

# Plot output
plot(output, main = "snow melt",
    legend.args=list(text='Snow melt (m d-1)', side=3, line=1.5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
