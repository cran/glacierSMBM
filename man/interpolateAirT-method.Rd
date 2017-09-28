\name{interpolateAirT-method}
\docType{methods}
\alias{interpolateAirT-method}
\alias{interpolateAirT,RasterStack,numeric,RasterLayer-method}
\title{
Method: Air temperature interpolation
}
\description{
A function to interpolate reanalysed air temperature data using (empirical) lapse rates and a high-resolution Digital Elevation Model.
}
\details{
Reanalysis data from general circulation models are often the only comprehensive and consistent source of meteorlogical data in remote mountain environments. However, they mostly do not resolve the complex topography and the impacts on air temperature and pressure distribution. Air temperatures in the accumulation areas are therefore generally too warm and those in the valleys too cold. This function uses (empirical) lapse rates and the height difference between a resampled reanalysis DEM and a high-resolution DEM to apply a vertical correction to the reanalysed air temperature distribution. For more details please refer to the given examples or the original publication (Groos et al., submitted, Equation 8).
}
\value{
An object of class \code{'RasterLayer'} returning the interpolated air temperature distribution (in Kelvin or degree Celsius, depending on the input).
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
\code{\link{resampleStack}}, \code{\link{interpolateAirP}}
}
\examples{
# Load the provided RasterLayer objects as exemplary
# input for the function
data(airTemperature_10km_daily, dem_30m, srtm_dem_30m,
    package = "glacierSMBM")
# Individual RasterLayer objects should be loaded or
# created using the function raster()

# Calculate difference between SRTM1 DEM and resampled
# reanalysis DEM
DEM_Diff <- dem_30m - srtm_dem_30m

# Interpolate the temperature obtained from a reanalysis data set
# using a lapse rate and the altitude difference of the resampled
# reanalysis DEM and a high resolution DEM (e.g. SRTM1)
output <- interpolateAirT(airT = stack(airTemperature_10km_daily),
    lapseRate = 0.007, demDiff = DEM_Diff)

# Plot output
plot(airTemperature_10km_daily, main = "initial air
    temperature (10km)", legend.args=list(text='Temperature (K)',
    side=3, line=1.5))
plot(output, main = "interpolated air temperature (30m)",
    legend.args=list(text='Temperature (K)', side=3, line=1.5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
