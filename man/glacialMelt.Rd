\name{glacialMelt}
\alias{glacialMelt}
\title{
Function: Ablation model
}
\description{
A model to calculate melting of supraglacial snow and firn, bare glacier ice and debris covered glacier ice.
}
\usage{
glacialMelt(airT, airDensity, netRad, glacierMask, iceMask,
    snowMask, debrisMask, debrisThickness, inRadSW = stack(),
    inRadLW = stack(), disTuningFacAirT = disTuningFacAirT,
    disIceTMF = stack(), disSnowTMF = stack(),
    disIceRMF = stack(), disSnowRMF = stack(),
    disRelativeHumidity = stack(), disWindSpeed = stack(),
    disDebrisAlbedo = stack(), disThermalConductivity = stack(),
    disThermalEmissivity = stack(),
    disSurfaceRoughnessHeight = stack(),
    disFrictionVelocity = stack(),
    disVolumeFractionDebrisInIce = stack(),
    disDebrisAirRatio = stack(), disDragCoefficient = stack(),
    disIceDensity = stack(), outType = "mean",
    writeOutput = c(0, 0, 0, 0), outputName = "glacialMelt",
    tmpCreate = FALSE, tmpDir = "", outDir = "", ...)
}
\arguments{
  \item{airT}{
  An object of class \code{'RasterStack'}. Distributed air temperature (Kelvin or degree Celsius). For every time step.
}
  \item{airDensity}{
  An object of class \code{'RasterStack'}. Distributed air density (kg m-3). Stationary or for every time step.
}
  \item{netRad}{
  An object of class \code{'RasterStack'}. Distributed net radiation (W m-2). For every time step.
}
  \item{glacierMask}{
  An object of class \code{'RasterStack'}. Glacier area (1 = glacier, 0 = no glacier). Stationary or for every time step.
}
  \item{iceMask}{
  An object of class \code{RasterStack}. Area of bare glacier ice (1 = bare ice, 0 = no bare ice). Stationary or for every time step.
}
  \item{snowMask}{
  An object of class \code{RasterStack}. Area of supraglacial snow or firn (1 = snow or firn, 0 = no snow or firn). Stationary or for every time step.
}
  \item{debrisMask}{
	An object of class \code{'RasterStack'}. Area of debris covered glacier ice (1 = debris, 0 = no debris). Stationary or for every time step.
}
  \item{debrisThickness}{
	An object of class \code{'RasterStack'}. Distributed supraglacial debris thickness (m). Stationary or for every time step.
}
  \item{inRadSW}{
  An object of class \code{'RasterStack'}. Distributed incoming shortwave radiation (W m-2).
  Optional in the function \code{'debrisCoveredIceMelt'} instead of \code{'netRad'}. For every time step.
}
  \item{inRadLW}{
  An object of class \code{'RasterStack'}. Distributed incoming longwave radiation (W m-2).
  Optional in the function \code{'debrisCoveredIceMelt'} instead of \code{'netRad'}. For every time step.
}
  \item{disTuningFacAirT}{
  An object of class \code{'RasterStack'}. Distributed air temperature tuning factor (\code{'tuningFacAirT'}). Stationary or for every time step.
}
  \item{disIceTMF}{
  An object of class \code{RasterStack}. Distributed temperature melting factor (m K-1 timestep-1) of ice. Stationary or for every time step.
}
  \item{disSnowTMF}{
  An object of class \code{RasterStack}. Distributed temperature melting factor (m K-1 timestep-1) of snow. Stationary or for every time step.
}
  \item{disIceRMF}{
  An object of class \code{RasterStack}. Distributed radiative temperature melting factor (m K-1 timestep-1) of ice. Stationary or for every time step.
}
  \item{disSnowRMF}{
  An object of class \code{RasterStack}. Distributed radiative temperature melting factor (m K-1 timestep-1) of snow. Stationary or for every time step.
}
  \item{disRelativeHumidity}{
  An object of class \code{'RasterStack'}. Distributed relative humidity (0-1) at measurement height. Stationary or for every time step.
}
  \item{disWindSpeed}{
  An object of class \code{'RasterStack'}. Distributed wind speed (m s-1) at measurement height. Stationary or for every time step.
}
  \item{disDebrisAlbedo}{
  An object of class \code{'RasterStack'}. Distributed albedo (0-1) of the debris. Stationary or for every time step.
}
  \item{disThermalConductivity}{
  An object of class \code{'RasterStack'}. Distributed thermal conductivity (W m-1 K-1) of the debris layer. Stationary or for every time step.
}
  \item{disThermalEmissivity}{
  An object of class \code{'RasterStack'}. Distributed thermal emissivity (0-1) of the debris layer. Stationary or for every time step.
}
  \item{disSurfaceRoughnessHeight}{
  An object of class \code{'RasterStack'}. Distributed surface roughness height (m) of the debris layer. Stationary or for every time step.
}
  \item{disFrictionVelocity}{
  An object of class \code{'RasterStack'}. Distributed friction velocity (m s-1) of the debris layer. Stationary or for every time step.
}
  \item{disVolumeFractionDebrisInIce}{
  An object of class \code{'RasterStack'}. Distributed volume fraction (0-1) of debris in the ice body. Stationary or for every time step.
}
  \item{disDebrisAirRatio}{
  An object of class \code{'RasterStack'}. Distributed ratio of the debris surface area to the volume of air (m-1) in the debris layer. Stationary or for every time step.
}
  \item{disDragCoefficient}{
  An object of class \code{'RasterStack'}. Distributed drag coefficient (m-1) of the debris layer. Stationary or for every time step.
}
  \item{disIceDensity}{
  An object of class \code{'RasterStack'}. Distributed density (kg m-3) of ice. Stationary or for every time step.
}
  \item{outType}{
  An object of class \code{'character'}. Type of output to be returned by the function: "mean" (default) snow melt or "sum".
}
  \item{writeOutput}{
	An object of class \code{'logical'}. Determines which of the four outputs (default = none) shall be exported as \code{'RasterLayer'} (1) and which not (0). For more information see section values.
}
  \item{outputName}{
  An object of class \code{'character'}. File name for the output \code{'RasterLayer(s)'} (default = "glacialMelt").
}
  \item{tmpCreate}{
  An object of class \code{'logical'}. Determines whether a temporary directory should be used (TRUE) or not (FALSE, default). Recommendend if large datasets are processed.
}
  \item{tmpDir}{
	An object of class \code{'character'}. Directory where processing files can be temporarily stored if \code{'tmpCreate'} = TRUE.
}
  \item{outDir}{
	An object of class \code{'character'}. Directory for the output files if \code{'writeOutput'} = 1.
}
  \item{\dots}{
	Further arguments passed to the embedded functions \code{\link{iceMelt}}, \code{\link{snowMelt}} and \code{\link{debrisCoveredIceMelt}}.
}
}
\details{
Glacier ablation comprises melting of bare and debris-covered ice as well as melting of supraglacial snow and firn. 
Depending on the glacier surface classification as defined in \code{'iceMask'}, \code{'snowMask'} and \code{'debrisMask'}, the melt rate for each pixel is quantified using the functions \code{\link{iceMelt}}, \code{\link{snowMelt}} and \code{\link{debrisCoveredIceMelt}}. For more information please refer to the applied melting functions, the examples below or the original publication (Groos et al., submitted, Equations 10-16).
}
\value{
An object of class \code{'RasterStack'} consisting of four individual \code{'RasterLayers'}:

\enumerate{
  \item Melt rate of supraglacial snow and firn as calculated by \code{\link{snowMelt}}
  \item Melt rate of bare ice as calculated by \code{\link{iceMelt}}
  \item Melt rate of debris covered ice as calculated by \code{\link{debrisCoveredIceMelt}}
  \item Sum of 1-3
}

To access the individual \code{'RasterLayers'}, use \code{\link[raster]{subset}}.

If more than one \code{'RasterLayer'} is stored in the input \code{'RasterStacks'}, the output \code{'RasterStack'} returns the "mean" or "sum" (depending on \code{'outType'}) of snow melt, ice melt and debris covered ice melt.
}
\references{
Collier, E., Moelg, T., Maussion, F., Scherer, D., Mayer, C., and Bush, A.B.G. (2013). High-resolution interactive modelling of the mountain glacier-atmosphere interface: an application over the Karakoram. The Cryosphere Discussions 7, 103-144.

Evatt, G.W., Abrahams, D., Heil, M., Mayer, C., Kingslake, J., Mitchell, S.L., Fowler, A.C., and Clark, C.D. (2015). Glacial melt under a porous debris layer. Journal of Glaciology 61, 825-836.

Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.

Hock, R. (2003). Temperature index melt modelling in mountain areas. Journal of Hydrology 282, 104-115.

Hock, R. (2005). Glacier melt: a review of processes and their modelling. Progress in Physical Geography 29, 362-391.
}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\note{
The following input variables are the requested minimum to run the model:
\itemize{
  \item \code{'airT'} (for every time step)
  \item \code{'netRad'} (for every time step)
  \item \code{'airDensity'} (stationary or for every time step)
  \item \code{'glacierMask'} (stationary or for every time step)
  \item \code{'iceMask'} (stationary or for every time step)  
  \item \code{'snowMask'} (stationary or for every time step)
  \item \code{'debrisMask'} (stationary or for every time step)
  \item \code{'debrisThickness'} (stationary or for every time step)
}
If \code{'inRadSW'} and \code{'inRadLW'} are provided instead of \code{'netRad'}, the energy-balance at the atmosphere-debris interface is calculated taking the \code{'debrisAlbedo'} and \code{'thermalEmissivity'} of the debris layer into account.

A default value (constant in space and time) is given for each additional argument like \code{'windSpeed'}, \code{'relativeHumidity'} and \code{'thermalConductivity'}. If desired, the default parameters can be modified. Furthermore, there is the option to pass distributed values (stationary or for every time step) instead of general values using the related 'dis*'-arguments like \code{'disWindSpeed'}, \code{'disRelativeHumidity'} and \code{'disThermalConductivity'}. In this case, the general parameter is ignored.

File format of written ouput: GeoTIFF.
}
\seealso{
\code{\link{snowMelt}}, \code{\link{iceMelt}}, \code{\link{debrisCoveredIceMelt}}
}
\examples{
# Load the provided RasterLayer objects as exemplary
# input for the function
data(airTemperature_30m_daily, netRad_30m_daily,
    airDensity_30m_daily, glacierMask_30m, iceMask_30m,
    firnMask_30m, debrisMask_30m, debrisThickness_30m,
    package = "glacierSMBM")
# Individual RasterLayer objects should be loaded or
# created using the function raster()

# Include RasterLayer in RasterStack
AirTemperature_30m_daily <- stack(airTemperature_30m_daily * 1.01)
NetRad_30m_daily <- stack(netRad_30m_daily)
AirDensity_30m_daily <- stack(airDensity_30m_daily)
GlacierMask_30m <- stack(glacierMask_30m)
IceMask_30m <- stack(iceMask_30m)
FirnMask_30m <- stack(firnMask_30m)
DebrisThickness_30m <- stack(debrisThickness_30m)
DebrisMask_30m <- stack(debrisMask_30m)

\dontrun{
# Calculate ablation using standard settings
output <- glacialMelt(airT = AirTemperature_30m_daily,
    netRad = NetRad_30m_daily, airDensity = AirDensity_30m_daily,
    glacierMask = GlacierMask_30m, iceMask = IceMask_30m,
    snowMask = FirnMask_30m, debrisMask = DebrisMask_30m,
    debrisThickness = DebrisThickness_30m)

# Plot output
plot(output, main = c("snow melt", "bare ice melt",
    "debris covered ice melt","total ablation"),
    legend.args=list(text='Melt (m d-1)', side=3, line=1.5))
}

# Calculate ablation using modified setting (e.g. input
# temperature in celsius instead of kelvin and
# increased thermal conductivity)

# Therefore exemplarily convert temperature from
# kelvin to degree celsius
airTcelsius <- AirTemperature_30m_daily - 273.15

# Include RasterLayer in RasterStack
airTcelsius <- stack(airTcelsius)

\dontrun{
output <- glacialMelt(airT = airTcelsius, netRad =
    NetRad_30m_daily, airDensity <- AirDensity_30m_daily,
    glacierMask = GlacierMask_30m, iceMask = IceMask_30m,
    snowMask = FirnMask_30m, debrisMask = DebrisMask_30m,
    debrisThickness = DebrisThickness_30m, tUnit = "C",
    thermalConductivity = 1.5)

# Plot output
plot(output, main = c("snow melt", "bare ice melt",
    "debris covered ice melt","total ablation"),
    legend.args=list(text='Melt (m d-1)', side=3, line=1.5))
}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
