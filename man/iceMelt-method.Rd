\name{iceMelt-method}
\docType{methods}
\alias{iceMelt-method}
\alias{iceMelt,RasterStack,RasterStack,RasterStack,RasterStack-method}
\title{
Method: Ice melt model
}
\description{
A simple model to calculate ice melt based on empirical melting factors.
}
\details{
An enhanced degree-day model (e.g. Hock, 2003, 2005; Pellicciotti & alii, 2005) is applied to quantify glacier mass loss ascribed to melted ("bare") ice using empirical temperature (TMF) and radiative melting factors (RMF). Fore more information please refer to the examples below or the original publication (Groos et al., submitted, Equation 10)
}
\value{
An object of class \code{'RasterLayer'} returning the calculated spatial distribution of ice melt (e.g. in m d-1, depending on \code{'tmpRes'}).
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
  \item \code{'iceMask'} (stationary or for every time step)
}
A default value (constant in space and time) is given for each additional argument like \code{'iceTMF'} or \code{'iceRMF'}. If desired, the default parameters can be modified. Furthermore, there is the option to pass distributed values (stationary or for every time step) instead of general values using the related 'dis*'-arguments like \code{'disIceTMF'} or \code{'disIceRMF'}. In this case, the general parameter is ignored.

File format of written ouput: GeoTIFF.
}
\seealso{
\code{\link{glacialMelt}}, \code{\link{snowMelt}}, \code{\link{debrisCoveredIceMelt}}
}
\examples{
# Load the provided RasterLayer objects as exemplary
# input for the function
data(airTemperature_30m_daily, netRad_30m_hourly,
    glacierMask_30m, iceMask_30m, package = "glacierSMBM")
# Individual RasterLayer objects should be loaded or
# created using the function raster()

# Include RasterLayer in RasterStack
AirTemperature_30m_daily <- stack(airTemperature_30m_daily)
NetRad_30m_hourly <- stack(netRad_30m_hourly)
GlacierMask_30m <- stack(glacierMask_30m)
IceMask_30m <- stack(iceMask_30m)

# Calculate ice melt using standard settings
output <- iceMelt(airT = AirTemperature_30m_daily,
    netRad = NetRad_30m_hourly, glacierMask = GlacierMask_30m,
    iceMask = IceMask_30m)

# Plot output
plot(output, main = "ice melt",
    legend.args=list(text='Ice melt (m d-1)', side=3, line=1.5))

# Calculate ice melt using modified setting (e.g. air temperature
# in degree Celsius instead of Kelvin; changes melting factors)
# Therefore exemplarily convert temperature from kelvin to celsius
airTcelsius <- subset(AirTemperature_30m_daily, 1) - 273.15

# Include RasterLayer in RasterStack
airTcelsius <- stack(airTcelsius)

output <- iceMelt(airT = airTcelsius, netRad = NetRad_30m_hourly,
    glacierMask = GlacierMask_30m, tUnit = "C",
    iceMask = IceMask_30m, iceTMF = 75*10^-4, iceRMF = 1.2*10^-4)

# Plot output
plot(output, main = "ice melt",
    legend.args=list(text='Ice melt (m d-1)', side=3, line=1.5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
