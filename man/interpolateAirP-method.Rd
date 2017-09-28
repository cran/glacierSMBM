\name{interpolateAirP-method}
\docType{methods}
\alias{interpolateAirP-method}
\alias{interpolateAirP,RasterStack,RasterStack,numeric,RasterLayer-method}
\title{
Method: Air pressure interpolation
}
\description{
A function to interpolate reanalysed air pressure data using the barometric height formula.
}
\details{
Reanalysis data from general circulation models are often the only comprehensive and consistent source of meteorlogical data in remote mountain environments. However, they mostly do not resolve the complex topography and the impacts on air temperature and pressure distribution. Air pressure in high elevations is therefore overestimated and in the valleys underestimated. This function uses the barometric height formula with the height difference between a resampled reanalysis DEM and a high-resolution DEM as input to apply a vertical correction to the reanalysed air pressure distribution. For more details please refer to the given examples or the original publication (Groos et al., submitted).
}
\value{
An object of class \code{'RasterLayer'} returning the interpolated air pressure distribution (Pa).
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.
}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\note{
File format of written ouput: GeoTIFF.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{resampleStack}}, \code{\link{interpolateAirT}}
}
\examples{
# Load the provided RasterLayer objects as exemplary
# input for the function
data(airPressure_10km_daily, airTemperature_30m_daily,
    dem_30m, srtm_dem_30m, package = "glacierSMBM")
# Individual RasterLayer objects should be loaded or
# created using the function raster()

# Calculate difference between SRTM1 DEM and
# resampled reanalysis DEM
DEM_Diff <- dem_30m - srtm_dem_30m

# Interpolate the air pressure obtained from a reanalysis data
# set using a lapse rate and the altitude difference of the
# resampled DEM and high resolution DEM (e.g. SRTM1)
output <- interpolateAirP(airP = stack(airPressure_10km_daily),
    airT = stack(airTemperature_30m_daily), lapseRate = 0.007,
    demDiff = DEM_Diff)

# Plot output
plot(airPressure_10km_daily, main = "air pressure (10km)",
    legend.args=list(text='Pressure (Pa)', side=3, line=1.5))
plot(output, main = "interpolated air pressure (30m)",
    legend.args=list(text='Pressure (Pa)', side=3, line=1.5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
