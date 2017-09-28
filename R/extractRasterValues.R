# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: extract raster values
# latest update: 2017-09-26

setGeneric(name = "extractRasterValues",
           def = function(rasterLayer, selectedCoordinates)
           {
             standardGeneric("extractRasterValues")
           }
)

setMethod(f = "extractRasterValues",
          signature = c("RasterLayer", "matrix"),
          definition = function(rasterLayer, selectedCoordinates)
          {
            
            # identify requested cell numbers in raster layer
            cellNr <- cellFromXY(rasterLayer, selectedCoordinates)
            
            # extract requested cell values from raster layer
            rasterValues <- extract(rasterLayer, cellNr)
            
            # return extracted cell values
            return( rasterValues )
            
          }
)
