# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: convert units of a RasterLayer object
# latest update: 2017-09-26

setGeneric(name = "unitConv",
           def = function( rasterLayer, u1, u2, decimalPlaces = 4)
           {
             standardGeneric("unitConv")
           }
)

setMethod(f = "unitConv",
          signature = c( "RasterLayer", "character", "character"),
          definition = function( rasterLayer, u1, u2, decimalPlaces = 4 )
          {
            
            if (u1 == "kelvin" || u2 == "kelvin"){
              
              convFac <- ud.convert(0, u1, u2)
              
              output <- rasterLayer + convFac
              
            }else{
              
              convFac <- ud.convert(1, u1, u2)
              
              output <- rasterLayer * convFac
              
            }
            
            output <- round(output, decimalPlaces)
            
            return(output)
            
          }
            
)