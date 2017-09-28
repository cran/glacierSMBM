\name{debrisThicknessFit}
\alias{debrisThicknessFit}
\title{
Function: Debris thickness fitting
}
\description{
A function to fit remotely sensed surface temperatures to measured debris thickness.
}
\usage{
debrisThicknessFit(surfaceTemperature, debrisThickness,
    plotOutput = FALSE)
}
\arguments{
  \item{surfaceTemperature}{
	An object of class \code{'numeric'}. Remotely sensed (or in-situ measured) surface temperature (in K) for the respective sites of debris thickness measurements.
}
  \item{debrisThickness}{
	An object of class \code{'numeric'}. Measured debris thickness (m).
}
  \item{plotOutput}{
	An object of class \code{'logical'}. Determines whether to plot the results (\code{TRUE}) or not (\code{FALSE}, default).
}
}
\details{
The spatial distribution and thickness of supraglacial debris can be derived from remotely sensed surface temperatures based on an empirical relationship as shown by Mihalcea et al. (2006, 2008a, 2008b).
High surface temperatures are correlated with thick debris, whereas surface temperatures closer to or below the melting point indicate a thin or absent debris layer. 
An exponential function with two fitting parameters (fp) was found to be most suitable to predict debris thickness from surface temperature (Minora et al., 2015). To derive debris thickness from surface temperature, an empirical non-linear model (\code{\link{debrisThicknessEmp}}) is applied (Groos et al., submitted). The two fitting parameters of the model are obtained by iteratively comparing measured and modelled debris thickness using varying starting values. Calculated non-linear (weighted) least-squares (\code{\link[stats]{nls}}) serve for the selection of the optimal fitting parameters.
}
\value{
An object of class \code{'numeric'} containing the two fitting parameters.
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
The function \code{\link{extractRasterValues}} may help to extract the respective surface temperature values from a \code{'RasterLayer'} at the locations of debris thickness measurements.
}
\seealso{
\code{\link{debrisThicknessEmp}}, \code{\link[stats]{nls}}
}
\examples{
# Load the provided data set as exemplary input for the function
# The values of the data set do not represent real field
# measurements and were only created for demonstration purposes
data(debrisThickness_measured, lst_measured,
    package = "glacierSMBM")
# Individual data sets should be loaded using the
# functions read.*()

# Calculate the required fitting parameters for the function
# debrisThicknessEmp() and plot the results
output <- debrisThicknessFit(surfaceTemperature = lst_measured,
    debrisThickness = debrisThickness_measured, plotOutput = TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line