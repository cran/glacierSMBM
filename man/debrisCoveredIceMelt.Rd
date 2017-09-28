\name{debrisCoveredIceMelt}
\alias{debrisCoveredIceMelt}
\title{
Function: Sub-debris ice melt model
}
\description{
An energy balance model to calculate glacial melt under a porous debris layer.
}
\usage{
debrisCoveredIceMelt(airT, airDensity, glacierMask, debrisMask,
    debrisThickness, inRadSW = stack(), inRadLW = stack(),
    netRad = stack(), tUnit = "K", tuningFacAirT = 1,
    disTuningFacAirT = stack(), tmpRes = "d", measurementHeight=2,
    relativeHumidity = 0.73, disRelativeHumidity = stack(),
    windSpeed = 2, disWindSpeed = stack(), debrisAlbedo = 0.07,
    disDebrisAlbedo = stack(), thermalConductivity = 0.585,
    disThermalConductivity = stack(), thermalEmissivity = 0.95,
    disThermalEmissivity = stack(), surfaceRoughnessHeight = 0.01,
    disSurfaceRoughnessHeight = stack(), frictionVelocity = 0.16,
    disFrictionVelocity = stack(), volumeFractionDebrisInIce = 0.01,
    disVolumeFractionDebrisInIce = stack(), debrisAirRatio = 188,
    disDebrisAirRatio = stack(),dragCoefficient = 5,
    disDragCoefficient = stack(), iceDensity = 900,
    disIceDensity = stack(), decimalPlaces = 4, outType = "mean",
    writeOutput = FALSE, outputName = "dcIceMelt", tmpCreate =
    FALSE, tmpDir = "", outDir = "", ... )
}
\arguments{
  \item{airT}{
	An object of class \code{'RasterStack'}. Distributed air temperature (Kelvin or degree Celsius). For every time step.
}
  \item{airDensity}{
	An object of class \code{'RasterStack'}. Distributed air density (kg m-3). Stationary or for every time step.
}
  \item{glacierMask}{
	An object of class \code{'RasterStack'}. Glacier area (1 = glacier, 0 = no glacier). Stationary or for every time step.
}
  \item{debrisMask}{
	An object of class \code{'RasterStack'}. Area of debris covered glacier ice (1 = debris, 0 = no debris). Stationary or for every time step.
}
  \item{debrisThickness}{
	An object of class \code{'RasterStack'}. Distributed supraglacial debris thickness (m). Stationary or for every time step.
}
  \item{inRadSW}{
  An object of class \code{'RasterStack'}. Distributed incoming shortwave radiation (W m-2). For every time step.
}
  \item{inRadLW}{
  An object of class \code{'RasterStack'}. Distributed incoming longwave radiation (W m-2). For every time step.
}
  \item{netRad}{
  An object of class \code{'RasterStack'}. Distributed net radiation (W m-2). For every time step. Optional instead of \code{'inRadSW'} and \code{'inRadLW'}.
}
  \item{tUnit}{
  An object of class \code{'character'}. Unit ("K" = Kelvin, "C" = degree Celsius) of air temperature (default = "K").
}
  \item{tuningFacAirT}{
	An object of class \code{'numeric'}. General air temperature tuning factor (<1 = temperature decrease, 1 = default, >1 = temperature increase).
}
  \item{disTuningFacAirT}{
	An object of class \code{'RasterStack'}. Distributed air temperature tuning factor (\code{tuningFacAirT}). Stationary or for every time step.
}
  \item{tmpRes}{
  An object of class \code{'character'}. Time aggregation (temporal resolution) of the input variables (default = "d"). "y" = year, "w" = week, "d" = day, "h" = hour, "s" = second.
}
  \item{measurementHeight}{
  An object of class \code{'numeric'}. Height (m) of meteorological measurements (default = 2).
}
  \item{relativeHumidity}{
  An object of class \code{'numeric'}. Relative humidity (0-1) at measurement height (default = 0.73).
}
  \item{disRelativeHumidity}{
  An object of class \code{'RasterStack'}. Distributed relative humidity (0-1) at measurement height. Stationary or for every time step.
}
  \item{windSpeed}{
	An object of class \code{'numeric'}. Wind speed (m s-1) at measurement height (default = 2).
}
  \item{disWindSpeed}{
  An object of class \code{'RasterStack'}. Distributed wind speed (m s-1) at measurement height. Stationary or for every time step.
}
  \item{debrisAlbedo}{
  An object of class \code{'numeric'}. Albedo (0-1) of the debris (default = 0.07).
}
  \item{disDebrisAlbedo}{
  An object of class \code{'RasterStack'}. Distributed albedo (0-1) of the debris. Stationary or for every time step.
}
  \item{thermalConductivity}{
	An object of class \code{'numeric'}. Thermal conductivity (W m-1 K-1) of the debris layer (default = 0.585).
}
  \item{disThermalConductivity}{
  An object of class \code{'RasterStack'}. Distributed thermal conductivity (W m-1 K-1) of the debris layer. Stationary or for every time step.
}
  \item{thermalEmissivity}{
  An object of class \code{'numeric'}. Thermal emissivity (0-1) of the debris layer (default = 0.95).
}
  \item{disThermalEmissivity}{
  An object of class \code{'RasterStack'}. Distributed thermal emissivity (0-1) of the debris layer. Stationary or for every time step.
}
  \item{surfaceRoughnessHeight}{
	An object of class \code{'numeric'}. Surface roughness height (m) of the debris layer (default = 0.01).
}
  \item{disSurfaceRoughnessHeight}{
  An object of class \code{'RasterStack'}. Distributed surface roughness height (m) of the debris layer. Stationary or for every time step.
}
  \item{frictionVelocity}{
	An object of class \code{'numeric'}. Friction velocity (m s-1) of the debris layer (default = 0.16).
}
  \item{disFrictionVelocity}{
  An object of class \code{'RasterStack'}. Distributed friction velocity (m s-1) of the debris layer. Stationary or for every time step.
}
  \item{volumeFractionDebrisInIce}{
	An object of class \code{'numeric'}. Volume fraction (0-1) of debris in the ice body (default = 0.01).
}
  \item{disVolumeFractionDebrisInIce}{
  An object of class \code{'RasterStack'}. Distributed volume fraction (0-1) of debris in the ice body. Stationary or for every time step.
}
  \item{debrisAirRatio}{
	An object of class \code{'numeric'}. Ratio of the debris surface area to the volume of air (m-1) in the debris layer (default = 188).
}
  \item{disDebrisAirRatio}{
  An object of class \code{'RasterStack'}. Distributed ratio of the debris surface area to the volume of air (m-1) in the debris layer. Stationary or for every time step.
}
  \item{dragCoefficient}{
	An object of class \code{'numeric'}. Drag coefficient (m-1) of the debris layer (default = 5).
}
  \item{disDragCoefficient}{
  An object of class \code{'RasterStack'}. Distributed drag coefficient (m-1) of the debris layer. Stationary or for every time step.
}
  \item{iceDensity}{
	An object of class \code{'numeric'}. Density (kg m-3) of ice (default = 900).
}
  \item{disIceDensity}{
  An object of class \code{'RasterStack'}. Distributed density (kg m-3) of ice. Stationary or for every time step.
}
  \item{decimalPlaces}{
	An object of class \code{'numeric'}. Number of decimal places (default = 4).
}
  \item{outType}{
	An object of class \code{'character'}. Type of output to be returned by the function: "mean" (default) sub-debris ice melt or "sum".
}
  \item{writeOutput}{
	An object of class \code{'logical'}. Determines whether the ouput shall be exported as \code{RasterLayer} (TRUE) or not (FALSE, default).
}
  \item{outputName}{
	An object of class \code{'character'}. File name for the output \code{RasterLayer(s)} (default = "dcIceMelt").
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
The impact of supraglacial debris on the melting process of underlying ice depends on the thickness of the debris layer itself. A thin dust layer of several centimeters enhances ice melt due to increased radiative absorption, whereas a thick debris cover (>4-5 cm) isolates and reduces ablation (e.g. Mihalcea et al. 2006, 2008; Mayer et al., 2010). Since the relationship between debris thickness and ice melt is non-linear, simple melting factors (also known as "degree-day factors") are not applicable. An energy-balance model developed by Evatt et al. (2015, Equations 41-46) is therefore applied to calculate glacial melt under a porous debris layer (for detailed information please refer to Groos et al. (submitted, Equations 11-16).

The sub-debris ice melt model consideres the following energy fluxes:

\itemize{
  \item shortwave energy flux
  \item longwave energy flux
  \item sensible heat flux
  \item heat flux due to evaporation at the debris-ice interface
  \item latent heat flux due to melting
  \item heat flux within the debris layer
}
}
\value{
An object of class \code{'RasterLayer'} returning the calculated spatial distribution of sub-debris ice melt (e.g. in m d-1, depending on \code{'tmpRes'}).
}
\references{
Evatt, G.W., Abrahams, D., Heil, M., Mayer, C., Kingslake, J., Mitchell, S.L., Fowler, A.C., and Clark, C.D. (2015). Glacial melt under a porous debris layer. Journal of Glaciology 61, 825-836.

Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.

Mayer, C., Lambrecht, A., Mihalcea, C., Belo, M., Diolaiuti, G., Smiraglia, C., and Bashir, F. (2010). Analysis of Glacial Meltwater in Bagrot Valley, Karakoram. Mountain Research and Development 30, 169-177.

Mihalcea, C., Mayer, C., Diolaiuti, G., Lambrecht, A., Smiraglia, C., and Tartari, G. (2006). Ice ablation and meteorological conditions on the debris-covered area of Baltoro glacier, Karakoram, Pakistan. Annals of Glaciology 43, 292-300.

Mihalcea, C., Mayer, C., Diolaiuti, G., D'Agata, C., Smiraglia, C., Lambrecht, A., Vuillermoz, E., and Tartari, G. (2008). Spatial distribution of debris thickness and melting from remote-sensing and meteorological data, at debris-covered Baltoro glacier, Karakoram, Pakistan. Annals of Glaciology 48, 49-57.

}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\note{
The following input variables are the requested minimum to run the model:
\itemize{
  \item \code{'airT'} (for every time step)
  \item \code{'inRadSW'} and \code{'inRadLW'} or \code{'netRad'} (for every time step)
  \item \code{'airDensity'} (stationary or for every time step)
  \item \code{'glacierMask'} (stationary or for every time step)
  \item \code{'debrisMask'} (stationary or for every time step)
  \item \code{'debrisThickness'} (stationary or for every time step)
}
If \code{'inRadSW'} and \code{'inRadLW'} are provided instead of \code{'netRad'}, the energy-balance at the atmosphere-debris interface is calculated taking the \code{'debrisAlbedo'} and \code{'thermalEmissivity'} of the debris layer into account.

A default value (constant in space and time) is given for each additional argument like \code{'windSpeed'}, \code{'relativeHumidity'} and \code{'thermalConductivity'}. If desired, the default parameters can be modified. Furthermore, there is the option to pass distributed values (stationary or for every time step) instead of general values using the related 'dis*'-arguments like \code{'disWindSpeed'}, \code{'disRelativeHumidity'} and \code{'disThermalConductivity'}. In this case, the general parameter is ignored.

File format of written ouput: GeoTIFF.
}
\seealso{
\code{\link{glacialMelt}}, \code{\link{snowMelt}}, \code{\link{iceMelt}}
}
\examples{
# Load the provided RasterLayer objects
# as exemplary input for the function
data(glacierMask_30m, debrisThickness_30m, debrisMask_30m,
    airTemperature_30m_daily, airDensity_30m_daily,
    netRad_30m_daily, package = "glacierSMBM")
# Individual RasterLayer objects should be loaded
# or created using the function raster()

# Include RasterLayers in RasterStack
GlacierMask <- stack(glacierMask_30m)
DebrisThickness <- stack(debrisThickness_30m)
DebrisMask <- stack(debrisMask_30m)
AirTemperature <- stack(airTemperature_30m_daily)
AirDensity <- stack(airDensity_30m_daily)
NetRad <- stack(netRad_30m_daily)

# Calculate ice melt under a porous debris layer
# using standard settings
output <- debrisCoveredIceMelt(airT = AirTemperature,
    netRad = NetRad, airDensity = AirDensity,
    glacierMask = GlacierMask, debrisMask = DebrisMask,
    debrisThickness = DebrisThickness)

# Plot output
plot(output, main = "debris covered ice melt",
    legend.args=list(text='Ice melt (m d-1)', side=3, line=1.5))

# Calculate ice melt under a porous debris layer using modified
# settings (e.g. change numeric values for thermal conductivity
# and temporal resolution)
output <- debrisCoveredIceMelt(airT = AirTemperature,
    netRad = NetRad, airDensity = AirDensity,
    glacierMask = GlacierMask, debrisMask = DebrisMask,
    debrisThickness = DebrisThickness, thermalConductivity = 1.5,
    tmpRes = "h")

# Plot output
plot(output, main = "debris covered ice melt",
    legend.args=list(text='Ice melt (m h-1)', side=3, line=1.5))
}