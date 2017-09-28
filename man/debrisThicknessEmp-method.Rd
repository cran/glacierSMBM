\name{debrisThicknessEmp-method}
\docType{methods}
\alias{debrisThicknessEmp-method}
\alias{debrisThicknessEmp,numeric-method}
\title{
Method: Empirical debris thickness model
}
\description{
A simple empirical model to derive supraglacial debris thickness from land surface temperature.
}
\details{
The spatial distribution and thickness of supraglacial debris can be derived from remotely sensed surface temperatures based on an empirical relationship as shown by Mihalcea et al. (2006, 2008a, 2008b).
High surface temperatures are correlated with thick debris, whereas surface temperatures closer to or below the melting point indicate a thin or absent debris layer. 
An exponential function with two fitting parameters (fp) was found to be most suitable to predict debris thickness from surface temperature (Minora et al., 2015; Groos et al., submitted, Equations 3-4):

debrisThickness = exp(fp_1 * surfaceTemperature - fp_2)

A prerequisite for the application of the empirical model is the availability of at least some (in-situ) debris thickness measurements from the study area, since they are required for the calibration of the model (\code{\link{debrisThicknessFit}}).
}
\value{
An object of class \code{'RasterLayer'} or \code{'numeric'} (depending on the input) returning the calculated debris thickness (m).
}
\references{
Groos, A.R., Mayer, C., Smiraglia, C., Diolaiuti, G., and Lambrecht A. (submitted). A first attempt to model region-wide glacier surface mass balances in the Karakoram: findings and future challenges. Geografia Fisica e Dinamica Quaternaria.

Mihalcea, C., Mayer, C., Diolaiuti, G., Lambrecht, A., Smiraglia, C., and Tartari, G. (2006). Ice ablation and meteorological conditions on the debris-covered area of Baltoro glacier, Karakoram, Pakistan. Annals of Glaciology 43, 292-300.

Mihalcea, C., Brock, B.W., Diolaiuti, G., D'Agata, C., Citterio, M., Kirkbride, M.P., Cutler, M.E.J., and Smiraglia, C. (2008a). Using ASTER satellite and ground-based surface temperature measurements to derive supraglacial debris cover and thickness patterns on Miage Glacier (Mont Blanc Massif, Italy). Cold Regions Science and Technology 52, 341-354.

Mihalcea, C., Mayer, C., Diolaiuti, G., D'Agata, C., Smiraglia, C., Lambrecht, A., Vuillermoz, E., and Tartari, G. (2008b). Spatial distribution of debris thickness and melting from remote-sensing and meteorological data, at debris-covered Baltoro glacier, Karakoram, Pakistan. Annals of Glaciology 48, 49-57.

Minora, U., Senese, A., Bocchiola, D., Soncini, A., D'Agata, C., Ambrosini, R., Mayer, C., Lambrecht, A., Vuillermoz, E., Smiraglia, C., et al. (2015). A simple model to evaluate ice melt over the ablation area of glaciers in the Central Karakoram National Park, Pakistan. Annals of Glaciology 56, 202-216.
}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\note{
File format of written ouput: GeoTIFF.
}
\seealso{
\code{\link{debrisThicknessFit}}, \code{\link{debrisThicknessPhy}}
}
\examples{
# Load the provided data set and RasterLayer as exemplary
# input for the function. The values of the data set do not
# represent real field measurements and were only created for
# demonstration purposes 
data(debrisThickness_measured, lst_measured, lst_30m_hourly,
    package = "glacierSMBM")
# Individual data sets or RasterLayers should be loaded using
# the functions read.*() or raster(), respectively

# Calculate the required fitting parameters for the
# function debrisThicknessEmp()
Fitting_Parameters <- debrisThicknessFit(surfaceTemperature =
    lst_measured, debrisThickness = debrisThickness_measured,
    plotOutput = FALSE)

# Derive debris thickness from land surface temperature using
# an empirical model
output <- debrisThicknessEmp(disSurfaceTemperature =
    lst_30m_hourly, fittingParameters = Fitting_Parameters)

# Plot output
plot(output, main = "debris thickness",
    legend.args=list(text='Debris thickness (m)',
    side=3, line=1.5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line