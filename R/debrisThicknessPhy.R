# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: physical debris thickness model
# latest update: 2017-09-26

setGeneric(name = "debrisThicknessPhy",
           def = function(surfaceTemperature, airT, netRad, airP, tUnit = "K", measurementHeight = 2, windSpeed = 2,
                          disWindSpeed = stack(), surfaceRoughnessLength = 0.016, disSurfaceRoughnessLength = stack(),
                          thermalConductivity = 0.96, disThermalConductivity = stack(), gRatio = 2.7, decimalPlaces = 4,
                          writeOutput = FALSE, outputName = "debrisThickness", tmpCreate = FALSE, tmpDir = "",
                          outDir = "" )
           {
             standardGeneric("debrisThicknessPhy")
           }
)

setMethod(f = "debrisThicknessPhy",
          signature = c(rep("RasterLayer", 4)),
          definition = function(surfaceTemperature, airT, netRad, airP, tUnit = "K", measurementHeight = 2, windSpeed = 2,
                                disWindSpeed = stack(), surfaceRoughnessLength = 0.016, disSurfaceRoughnessLength = stack(),
                                thermalConductivity = 0.96, disThermalConductivity = stack(), gRatio = 2.7, decimalPlaces = 4,
                                writeOutput = FALSE, outputName = "debrisThickness", tmpCreate = FALSE, tmpDir = "",
                                outDir = "" )
          {
            
            # define output directory
            if(nchar(outDir) > 0){
              
              setwd(outDir)
              
            }
            
            # optional: create temporary directory for processing large files
            if( tmpCreate == TRUE & tmpDir > 0){
              
              rasterOptions(tmpdir = tmpDir )
              tmpDir(create = TRUE)
              
            }
            
            
            # conversion from kelvin to degrees celsius
            if (tUnit == "K"){
              
              airT <- airT - 273.15
              surfaceTemperature <- surfaceTemperature - 273.15
              
            }
            
            # import wind speed
            if(nlayers(disWindSpeed) == 0){
              
              # use general wind speed if no distributed wind speed is provided
              inputWindSpeed <- windSpeed
              
            }else{
              
              # use distributed wind speed
              inputWindSpeed <- disWindSpeed
              
            }
            
            # import surface roughness length
            if(nlayers(disSurfaceRoughnessLength) == 0){
              
              # use general surface roughness length if no distributed surface roughness length is provided
              inputSurfaceRoughnessLength <- surfaceRoughnessLength
              
            }else{
              
              # use distributed surface roughness length
              inputSurfaceRoughnessLength <- disSurfaceRoughnessLength
              
            }
            
            # import effective thermal conductivity
            if(nlayers(disThermalConductivity) == 0){
              
              # use general effective thermal conductivity if no distributed thermal conductivity is provided
              inputThermalConductivity <- thermalConductivity
              
            }else{
              
              # use distributed effective thermal conductivity
              inputThermalConductivity <- disThermalConductivity
              
            }
            
            # parameters from Rounce & McKinney 2014
            vonKarmanConstant <- 0.41
            gasConstant <- 287.05 # J kg-1 K-1
            pressureMSL <- 101325 # Pa
            specificHeatCapacityAir <- 1010 # J kg-1 K-1
            stefanBoltzmannConstant <- 5.67*10^-8 # W m-2 K-4
            
            # calculating air density
            airRho <- airP / (gasConstant * (airT + 273.15) ) # kg m-3
            
            # a = parameter for calculation of sensible heat flux
            A <- ( vonKarmanConstant^2 ) / ( log( ( measurementHeight / inputSurfaceRoughnessLength ) )^2 )
            
            # calculating sensible heat flux
            H <- airRho * ( airP / pressureMSL ) * specificHeatCapacityAir * A * inputWindSpeed * ( airT - surfaceTemperature )
            
            # calculating debris thickness on inverse energy balance modeling
            debrisThickness <- ( gRatio * ( inputThermalConductivity * surfaceTemperature ) ) / ( netRad + H )
            
            debrisThickness <- reclassify( debrisThickness, c(-Inf, 0, 0) )
            
            # round debris tickness
            debrisThickness <- round(debrisThickness, decimalPlaces)
            
            # write ouput raster
            if (writeOutput == TRUE){
              
              writeRaster(debrisThickness, filename = paste(outputName, ".tif", sep = ""), overwrite = T, NAflag = -99)
              
            }
            
            # return debris thickness
            return( debrisThickness )
            
          }
)