\name{resampleStack}
\alias{resampleStack}
\title{
Function: Resample a RasterStack object
}
\description{
\code{'ResampleStack'} transfers values between non matching \code{'RasterLayers'} in terms of origin and spatial resolution.
To adjust the spatial resolution, a bilinear interpolation is applied.
}
\usage{
resampleStack(rasterStack, resampleSettings, decimalPlaces = 4,
    outType = "mean", writeOutput = FALSE,
    outputName = "resample", tmpCreate = FALSE, tmpDir = "",
    outDir = "", ... )
}
\arguments{
  \item{rasterStack}{
	A \code{'RasterStack'} object to be resampled.
}
  \item{resampleSettings}{
	A \code{'RasterLayer'} object with the settings that \code{'RasterStack'} should be resampled to.
}
  \item{decimalPlaces}{
  An object of class \code{'numeric'}. Number of decimal places (default = 4).
}
  \item{outType}{
	An object of class \code{'character'}. Type of output to be returned by the function: "mean" (default) or "sum".
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
	Further arguments passed to the function \code{\link{unitConv}}, which is implemented in \code{resampleStack}. For more information see section details.
}
}
\details{
In addition to the function \code{\link{resample}}, \code{'resampleStack'} provides the export of each resampled \code{'RasterLayer'} as GeoTIFF.

If it is desired to convert the unit (\code{'u1'}) of the \code{'RasterStack'} into a different unit (\code{'u2'}), the arguments (\code{'u1'}) and (\code{'u2'}) (see \code{\link{unitConv}}) can be additionally passed to \code{'rasterStack'}.
}
\value{
An object of class \code{'RasterLayer'} returning the mean or sum (depending on \code{outType}) of the resampled \code{'RasterStack'}.
}

\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\note{
File format of written ouput: GeoTIFF.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{resample}}, \code{\link{unitConv}}
}
\examples{
# Load the provided RasterLayer objects as exemplary
# input for the function
data(airTemperature_30m_daily, precip_10km_daily,
    package = "glacierSMBM")
# Individual RasterLayer objects should be loaded or created
# using the function raster()

# Resample and interpolate a precipitation file with 10 km spatial
# resolution using the parameters of air temperature (30m)
# Additionally, convert from mm h-1 to m d-1
output <- resampleStack(stack(precip_10km_daily),
    airTemperature_30m_daily,1, writeOutput = FALSE,
    u1 = "mm/h", u2 = "mm/d", decimalPlaces = 5)

# Plot output
plot(precip_10km_daily, main = "precipitation (10km)",
    legend.args=list(text='precipitation (m)', side=3, line=1.5))
plot(output, main = "resampled precipitation (30m)",
    legend.args=list(text='precipitation (m)', side=3, line=1.5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
