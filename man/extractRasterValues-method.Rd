\name{extractRasterValues-method}
\docType{methods}
\alias{extractRasterValues-method}
\alias{extractRasterValues,RasterLayer,matrix-method}
\title{
Method: Extract raster values
}
\description{
A function to extract cell values from a RasterLayer.
}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}
\note{
The \code{\link[raster]{projection}} of the given coordinates and \code{RasterLayer} must be the same.
}
\seealso{
\code{\link[raster]{cellFrom}}, \code{\link[raster]{extract}}
}
\examples{
# Load the provided data set and RasterLayer as exemplary
# input for the function
data(selectedCoordinates, lst_30m_hourly,
    package = "glacierSMBM")
# Individual data sets should be loaded using the
# functions read.*() or raster(), respectively

# Extract the requested cell values of a RasterLayer based
# on the given coordinates
output <- extractRasterValues(rasterLayer = lst_30m_hourly,
    selectedCoordinates = selectedCoordinates)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
