\name{debrisCoveredIceMelt-method}
\docType{methods}
\alias{debrisCoveredIceMelt-method}
\alias{debrisCoveredIceMelt,RasterStack,RasterStack,RasterStack,RasterStack,RasterStack-method}
\title{
Method: Sub-debris ice melt model
}
\description{
An energy balance model to calculate glacial melt under a porous debris layer.
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