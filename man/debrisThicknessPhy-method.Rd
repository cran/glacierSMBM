\name{debrisThicknessPhy-method}
\docType{methods}
\alias{debrisThicknessPhy-method}
\alias{debrisThicknessPhy,RasterLayer,RasterLayer,RasterLayer,RasterLayer-method}
\title{
Method: Physical debris thickness model
}
\description{
An energy balance model to derive supraglacial debris thickness from surface temperature.
}
\details{
Ice melt rates below a porous debris layer are not only controlled by meteorological conditions, but also the thickness and thermal properties of the layer (e.g. Evatt et al., 2015).
For the modelling of sub-debris ice melt, information on the thickness and spatial variability of the debris layer are necessary.
In-situ measurements of debris thickness are labour-intensive and represent only a small fraction of the glacier surface.
To compensate for that, different energy balance models, which derive debris thickness from remotely sensed glacier surface temperatures, have been developed (e.g. Foster et al., 2012; Rounce & McKinney, 2014; Schauwecker et al. 2015).
The steady-state surface energy balance model of Rounce & McKinney (2014, 1-7) is applied to calculate the supraglacial debris thickness distribution from land surface and meteorological data.
An approximation factor (\code{gRatio}) is used to take the non-linear temperature variation in the debris layer into account.
For more details of the approach please refer to Groos et al. (submitted, Equations 5-7).
}
\value{
An object of class \code{'RasterLayer'} returning the calculated debris thickness distribution (m).
}
\references{
Evatt, G.W., Abrahams, D., Heil, M., Mayer, C., Kingslake, J., Mitchell, S.L., Fowler, A.C., and Clark, C.D. (2015). Glacial melt under a porous debris layer. Journal of Glaciology 61, 825-836.

Foster, L.A., Brock, B.W., Cutler, M.E.J., and Diotri, F. (2012). A physically based method for estimating supraglacial debris thickness from thermal band remote-sensing data. Journal of Glaciology 58, 677-691.

Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.

Rounce, D.R., and McKinney, D.C. (2014). Debris thickness of glaciers in the Everest area (Nepal Himalaya) derived from satellite imagery using a nonlinear energy balance model. The Cryosphere 8, 1317-1329.

Schauwecker, S., Rohrer, M., Huggel, C., Kulkarni, A., Ramanathan, A., Salzmann, N., Stoffel, M., and Brock, B. (2015). Remotely sensed debris thickness mapping of Bara Shigri Glacier, Indian Himalaya. Journal of Glaciology 61, 675-688.

}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\note{
The following input variables are the requested minimum to run the model:
\itemize{
  \item \code{'surfaceTemperature'}
  \item \code{'airT'}
  \item \code{'netRad'}
  \item \code{'airP'}
}

A default value (constant in space and time) is given for each additional argument like \code{'windSpeed'} or \code{'thermalConductivity'}. If desired, the default parameters can be modified. Furthermore, there is the option to pass distributed values (stationary or for every time step) instead of general values using the related 'dis*'-arguments like \code{'disWindSpeed'} or \code{'disThermalConductivity'}. In this case, the general parameter is ignored.

File format of written ouput: GeoTIFF.
}
\seealso{
\code{\link{debrisThicknessEmp}}, \code{\link{debrisCoveredIceMelt}}
}
\examples{
# Load the provided RasterLayer objects as exemplary input for
# the function
data(lst_30m_hourly, airTemperature_30m_hourly, netRad_30m_hourly,
    airPressure_30m_hourly, package = "glacierSMBM")
# Individual RasterLayer objects should be loaded or
# created using the function raster()

# Derive debris thickness from land surface temperature using
# standard settings
output <- debrisThicknessPhy(surfaceTemperature = lst_30m_hourly,
    airT = airTemperature_30m_hourly, netRad = netRad_30m_hourly,
    airP = airPressure_30m_hourly)

# Plot output
plot(output, main = "debris thickness",
    legend.args=list(text='Debris thickness (m)',
    side=3, line=1.5))

# Derive debris thickness from land surface temperature using
# modified settings (e.g. change numeric values for effective
# thermal conductivity and wind speed)

output <- debrisThicknessPhy(surfaceTemperature = lst_30m_hourly,
    airT = airTemperature_30m_hourly, netRad = netRad_30m_hourly,
    airP = airPressure_30m_hourly, windSpeed = 6,
    thermalConductivity = 0.86)

# Plot output
plot(output, main = "debris thickness",
    legend.args=list(text='Debris thickness (m)',
    side=3, line=1.5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line