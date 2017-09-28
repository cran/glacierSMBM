\name{unitConv}
\alias{unitConv}
\title{
Function: Unit conversion of a RasterLayer object
}
\description{
Converts the cell values of a \code{'RasterLayer'} from one unit to another.
}
\usage{
unitConv(rasterLayer, u1, u2, decimalPlaces = 4)
}
\arguments{
  \item{rasterLayer}{
	A \code{'RasterLayer'} object to be converted.
}
  \item{u1}{
	An object of class \code{'character'}. Unit of the input \code{'RasterLayer'}.
}
  \item{u2}{
	An object of class \code{'character'}. Requested unit of the output \code{'RasterLayer'}.
}
  \item{decimalPlaces}{
  An object of class \code{'numeric'}. Number of decimal places (default = 4).
}
}
\details{
This function makes use of \code{\link{ud.convert}} from the package \code{'udunits2'} to calculate a conversion factor which is applied to convert unit \code{'u1'} of a \code{'RasterLayer'} into the requested unit \code{'u2'}.

A list of common units: "kelvin", "celsius", "mm/d", "mm/h", "mm/s", "cm/d", "cm/h", "cm/s", "m/d", "m/h", "m/s".

}
\value{
Returns the converted \code{'RasterLayer'}.
}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\seealso{
\code{\link{ud.convert}}
}
\examples{

# Load the provided RasterLayer objects as exemplary
# input for the function
data(airTemperature_30m_daily, precip_30m_daily,
    package = "glacierSMBM")
# Individual RasterLayer objects should be loaded or
# created using the function raster()

# Convert air temperature from Kelvin to degree Celsius
output <- unitConv(airTemperature_30m_daily, "kelvin", "celsius")

# Plot output
plot(airTemperature_30m_daily, main = "Air Temperature [Kelvin]",
    legend.args=list(text='Temperature (K)', side=3, line=1.5))
plot(output, main = "Air Temperature [Celsius]",
    legend.args=list(text='Temperature (Celsius)',
    side=3, line=1.5))

# Convert precipitation from m d-1 to mm h-1
output <- unitConv(precip_30m_daily, "m/d", "mm/h")

# Plot output
plot(precip_30m_daily, main = "Precipitation [m/d]",
    legend.args=list(text='Precipitation (m/d)',
    side=3, line=1.5))
plot(output, main = "Precipitation [mm/h]",
    legend.args=list(text='Precipitation (mm/h)',
    side=3, line=1.5))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
