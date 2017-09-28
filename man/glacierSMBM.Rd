\name{glacierSMBM}
\alias{glacierSMBM}
\title{
Function: Glacier surface mass balance model
}
\description{
A model consisting of physical and statistical functions to calculate the surface mass balance of individual or multiple debris-free and debris-covered glaciers.
}
\usage{
glacierSMBM(inputGlacierSMBM, ...)
}
\arguments{
  \item{inputGlacierSMBM}{
	An object of class \code{'inputGlacierSMBM'}. Required parameters and variables to calculate glacier surface mass balances. Fore more information see \code{\link{inputGlacierSMBM-class}}.
}
  \item{\dots}{
  Optional arguments should be also addressed in \code{'inputGlacierSMBM'}.
}
}
\details{
Glacier surface mass balances are quantified by calculating the difference between the glacier mass gain ascribed to deposited snow and the mass loss associated with melting of bare an debris-covered ice as well as supraglacial snow and firn. The initial glacier outlines and surface conditions can be defined in \code{'glacierMask'}, \code{'iceMask'}, \code{'firnMask'}, \code{'snowHeight'}, \code{'debrisMask'} and \code{'debrisThickness'} (for more information see \code{\link{inputGlacierSMBM-class}}). Accumulation and ablation is iteratively modelled for every discrete time step (e.g. month, day or hour). The function \code{\link{snowFall}} is applied to quantify accumulation for every pixel and time step (note: mass input from redistributed snow has not yet been implemented). Ablation does only occur in the model at air temperatures above freezing since the complex process of percolation, retention and refreezing of meltwater is (so far) neglected. The extent and height of supraglacial snow changes with time due to the interplay of snow accumulation (\code{\link{snowFall}}) and snow melt (\code{\link{snowMelt}}). Ablation of debris-free (\code{\link{iceMelt}}) or debris-covered glacier ice (\code{\link{debrisCoveredIceMelt}}) sets in as soon as the overlying snow pack is completely melted and takes place as long as a pixel is snow-free. For more information please refer to the applied functions, the examples below or the original publication (Groos et al., submitted).
}
\value{
The potential model output consists of sixteen individual \code{'RasterLayers'} and a summarising table:
\enumerate{
  \item Accumulation rate (for every time step).
  \item Accumulation sum (for the modelling period).
  \item Glacial ablation rate (for every time step).
  \item Glacial ablation sum (for the modelling period).
  \item Ice melt rate (for every time step).
  \item Ice melt sum (for the modelling period).
  \item Debris ice melt rate (for every time step).
  \item Debris ice melt sum (for the modelling period).
  \item Firn melt rate (for every time step).
  \item Firn melt sum (for the modelling period).
  \item Snow melt rate (for every time step).
  \item Snow melt sum (for the modelling period).
  \item Snow cover (for every time step).
  \item Snow height (for the modelling period).
  \item Glacier surface mass balance (for every time step).
  \item Glacier surface mass balance (for the modelling period).
  \item Output table consisting of the spatial average of the fourteen variables above for every time step.
}
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\note{
The following input variables in \code{\link{inputGlacierSMBM-class}} are the requested minimum to run the model:
\itemize{
  \item \code{'date'} (date and time of each \code{RasterLayer} in the following \code{RasterStacks}.)
  \item \code{'airT'} (for every time step)
  \item \code{'netRad'} (for every time step)
  \item \code{'snowfall'} or \code{'precip'} (for every time step)  
  \item \code{'airDensity'} (stationary or for every time step)
  \item \code{'glacierMask'} (stationary or for every time step)
  \item \code{'iceMask'} (stationary or for every time step)  
  \item \code{'firnMask'} (stationary or for every time step)
  \item \code{'debrisMask'} (stationary or for every time step)
  \item \code{'debrisThickness'} (stationary or for every time step)
}
If the investigated glacier or glacier area is debris-free or snow-free, pass a \code{'RasterLayer'} with the extent of \code{'glacierMask'} and all values = 0 (zero means no snow or no debris, respectively) to the respective \code{'RasterStack'} (e.g. \code{'debrisMask'} or \code{'firnMask'}).

If \code{'inRadSW'} and \code{'inRadLW'} are provided instead of \code{'netRad'}, the energy-balance at the atmosphere-debris interface is calculated taking the \code{'debrisAlbedo'} and \code{'thermalEmissivity'} of the debris layer into account.

A default value (constant in space and time) is given for each additional argument in \code{\link{inputGlacierSMBM-class}} like \code{'windSpeed'}, \code{'relativeHumidity'} and \code{'thermalConductivity'}. If desired, the default parameters can be modified. Furthermore, there is the option to pass distributed values (stationary or for every time step) instead of general values using the related 'dis*'-arguments like \code{'disWindSpeed'}, \code{'disRelativeHumidity'} and \code{'disThermalConductivity'}. In this case, the general parameter is ignored.

File format of written ouput: GeoTIFF: GeoTIFF (1-16) and text file (17).
}
\seealso{
\code{\link{glacialMelt}}, \code{\link{snowMelt}}, \code{\link{iceMelt}},
\code{\link{debrisCoveredIceMelt}}, \code{\link{snowFall}},
\code{\link{inputGlacierSMBM-class}}
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
