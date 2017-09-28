# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: sub-debris ice melt model
# latest update: 2017-09-26

setGeneric(name = "debrisCoveredIceMelt",
           def = function(airT, airDensity, glacierMask, debrisMask, debrisThickness, inRadSW = stack(), inRadLW = stack(),
                          netRad = stack(), tUnit = "K", tuningFacAirT = 1, disTuningFacAirT = stack(), tmpRes = "d",
                          measurementHeight = 2, relativeHumidity = 0.73, disRelativeHumidity = stack(), windSpeed = 2,
                          disWindSpeed = stack(), debrisAlbedo = 0.07, disDebrisAlbedo = stack(), thermalConductivity = 0.585,
                          disThermalConductivity = stack(), thermalEmissivity = 0.95, disThermalEmissivity = stack(),
                          surfaceRoughnessHeight = 0.01, disSurfaceRoughnessHeight = stack(), frictionVelocity = 0.16,
                          disFrictionVelocity = stack(), volumeFractionDebrisInIce = 0.01, disVolumeFractionDebrisInIce = stack(),
                          debrisAirRatio = 188, disDebrisAirRatio = stack(), dragCoefficient = 5, disDragCoefficient = stack(),
                          iceDensity = 900, disIceDensity = stack(), decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
                          outputName = "dcIceMelt", tmpCreate = FALSE, tmpDir = "", outDir = "", ... )
           {
             standardGeneric("debrisCoveredIceMelt")
           }
)

setMethod(f = "debrisCoveredIceMelt",
          signature = c(rep("RasterStack",5)),
          definition = function(airT, airDensity, glacierMask, debrisMask, debrisThickness, inRadSW = stack(), inRadLW = stack(),
                                netRad = stack(), tUnit = "K", tuningFacAirT = 1, disTuningFacAirT = stack(), tmpRes = "d",
                                measurementHeight = 2, relativeHumidity = 0.73, disRelativeHumidity = stack(), windSpeed = 2,
                                disWindSpeed = stack(), debrisAlbedo = 0.07, disDebrisAlbedo = stack(), thermalConductivity = 0.585,
                                disThermalConductivity = stack(), thermalEmissivity = 0.95, disThermalEmissivity = stack(),
                                surfaceRoughnessHeight = 0.01, disSurfaceRoughnessHeight = stack(), frictionVelocity = 0.16,
                                disFrictionVelocity = stack(), volumeFractionDebrisInIce = 0.01, disVolumeFractionDebrisInIce = stack(),
                                debrisAirRatio = 188, disDebrisAirRatio = stack(), dragCoefficient = 5, disDragCoefficient = stack(),
                                iceDensity = 900, disIceDensity = stack(), decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
                                outputName = "dcIceMelt", tmpCreate = FALSE, tmpDir = "", outDir = "", ... )
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
            
            # defining the temporal resolution (in seconds) of the input variables
            if(tmpRes == "s"){
              
              timeAggregation <- 1
              
            }else if(tmpRes == "h"){
              
              timeAggregation <- 3600
              
            }else if(tmpRes == "d"){
              
              timeAggregation <- 86000
              
            }else if(tmpRes == "w"){
              
              timeAggregation <- 6048000
              
            }else if(tmpRes == "y"){
              
              timeAggregation <- 31557600
              
            }
            
            ##################################
            ### debrisCoveredIceMelt start ###
            ##################################
            
            for( i in 1:nlayers(airT) ){
              
              #########################
              ### data import start ###
              #########################
              
              # import glacier mask
              if(nlayers(glacierMask) == 1){
                
                # use stationary mask if only one is provided
                inputGlacierMask <- glacierMask
                
              }else{
                
                # use updated masks for every time step if available
                inputGlacierMask <- subset(glacierMask, i)
                
              }
              
              # import debris mask
              if(nlayers(debrisMask) == 1){
                
                # use stationary mask if only one is provided
                inputDebrisMask <- debrisMask
                
              }else{
                
                # use updated masks for every time step if available
                inputDebrisMask <- subset(debrisMask, i)
                
              }
              
              # import distributed debris thickness
              if(nlayers(debrisThickness) == 1){
                
                # use stationary distributed debris thickness
                inputDebrisThickness <- debrisThickness
                
              }else{
                
                # use updated debris thickness for every time step if available
                inputDebrisThickness <- subset(debrisThickness, i)
                
              }
              
              # import air temperature tuning factor
              if(nlayers(disTuningFacAirT) == 0){
                
                # use general tuning factor if no distributed tuning factor is provided
                airtTuningFactor <- tuningFacAirT
                
              }else if(nlayers(disTuningFacAirT) == 1){
                
                # use stationary distributed tuning factor
                airtTuningFactor <- disTuningFacAirT * inputGlacierMask
                
              }else{
                
                # use distributed tuning factor for every time step if available
                airtTuningFactor <- subset(disTuningFacAirT, i) * inputGlacierMask
                
              }
              
              # import air temperature
              inputAirT <- ( subset(airT, i) * airtTuningFactor ) * inputGlacierMask
              
              # import short- and longwave or net radiation
              if (nlayers(inRadSW) > 0 && nlayers(inRadLW) > 0){
                
                inputInRadSW <- subset(inRadSW, i) * inputGlacierMask
                inputInRadLW <- subset(inRadLW, i) * inputGlacierMask
                
              }else{
                
                inputNetRad <- subset(netRad, i) * inputGlacierMask
                
              }
              
              # import air density
              if(nlayers(airDensity) == 1){
                
                # use stationary mask if only one is provided
                inputAirDensity <- airDensity * inputGlacierMask
                
              }else{
                
                # use updated air density for every time step if available
                inputAirDensity <- subset(airDensity, i) * inputGlacierMask
                
              }                            
              
              # import relative humidity
              if(nlayers(disRelativeHumidity) == 0){
                
                # use general relative humidity if no distributed relative humditiy is provided
                inputRH <- relativeHumidity
                
              }else if(nlayers(disRelativeHumidity) == 1){
                
                # use stationary distributed relative humidity
                inputRH <- disRelativeHumidity * inputGlacierMask
                
              }else{
                
                # use distributed relative humidity for every time step if available
                inputRH <- subset(disRelativeHumidity, i) * inputGlacierMask
                
              }     
              
              # import wind speed
              if(nlayers(disWindSpeed) == 0){
                
                # use general wind speed if no distributed wind speed is provided
                inputWindSpeed <- windSpeed
                
              }else if(nlayers(disWindSpeed) == 1){
                
                # use stationary distributed wind speed
                inputWindSpeed <- disWindSpeed * inputGlacierMask
                
              }else{
                
                # use distributed wind speed for every time step if available
                inputWindSpeed <- subset(disWindSpeed, i) * inputGlacierMask
                
              }     
              
              # import debris albedo
              if(nlayers(disDebrisAlbedo) == 0){
                
                # use general debris albedo if no distributed debris albedo is provided
                inputDebrisAlbedo <- debrisAlbedo
                
              }else if(nlayers(disDebrisAlbedo) == 1){
                
                # use stationary distributed debris albedo
                inputDebrisAlbedo <- disDebrisAlbedo * inputGlacierMask
                
              }else{
                
                # use distributed debris albedo for every time step if available
                inputDebrisAlbedo <- subset(disDebrisAlbedo, i) * inputGlacierMask
                
              }
              
              # import thermal conductivity
              if(nlayers(disThermalConductivity) == 0){
                
                # use general thermal conductivity if no distributed thermal conductivity is provided
                inputThermalConductivity <- thermalConductivity
                
              }else if(nlayers(disThermalConductivity) == 1){
                
                # use stationary distributed thermal conductivity
                inputThermalConductivity <- disThermalConductivity * inputGlacierMask
                
              }else{
                
                # use distributed thermal conductivity for every time step if available
                inputThermalConductivity <- subset(disThermalConductivity, i) * inputGlacierMask
                
              }
     
              # import thermal emissivity
              if(nlayers(disThermalEmissivity) == 0){
                
                # use general thermal emissivity if no distributed thermal emissivity is provided
                inputThermalEmissivity <- thermalEmissivity
                
              }else if(nlayers(disThermalEmissivity) == 1){
                
                # use stationary distributed thermal emissivity
                inputThermalEmissivity <- disThermalEmissivity * inputGlacierMask
                
              }else{
                
                # use distributed thermal emissivity for every time step if available
                inputThermalEmissivity <- subset(disThermalEmissivity, i) * inputGlacierMask
                
              }
              
              # import surface roughness height
              if(nlayers(disSurfaceRoughnessHeight) == 0){
                
                # use general surface roughness height if no distributed surface roughness height is provided
                inputSRH <- surfaceRoughnessHeight
                
              }else if(nlayers(disSurfaceRoughnessHeight) == 1){
                
                # use stationary distributed surface roughness height
                inputSRH <- disSurfaceRoughnessHeight * inputGlacierMask
                
              }else{
                
                # use distributed surface roughness height for every time step if available
                inputSRH <- subset(disSurfaceRoughnessHeight, i) * inputGlacierMask
                
              }

              # import friction velocity
              if(nlayers(disFrictionVelocity) == 0){
                
                # use general friction velocity if no distributed friction velocity is provided
                inputFrictionVelocity <- frictionVelocity
                
              }else if(nlayers(disFrictionVelocity) == 1){
                
                # use stationary distributed friction velocity
                inputFrictionVelocity <- disFrictionVelocity * inputGlacierMask
                
              }else{
                
                # use distributed friction velocity for every time step if available
                inputFrictionVelocity <- subset(disFrictionVelocity, i) * inputGlacierMask
                
              } 
              
              # import debris volume fraction in ice
              if(nlayers(disVolumeFractionDebrisInIce) == 0){
                
                # use general debris volume fraction in ice if no distributed debris volume fraction in ice is provided
                inputVFDI <- volumeFractionDebrisInIce
                
              }else if(nlayers(disVolumeFractionDebrisInIce) == 1){
                
                # use stationary distributed debris volume fraction in ice
                inputVFDI <- disVolumeFractionDebrisInIce * inputGlacierMask
                
              }else{
                
                # use distributed debris volume fraction in ice for every time step if available
                inputVFDI <- subset(disVolumeFractionDebrisInIce, i) * inputGlacierMask
                
              }
 
              # import debris air ratio
              if(nlayers(disDebrisAirRatio) == 0){
                
                # use general debris air ratio if no distributed debris air ratio is provided
                inputDebrisAirRatio <- debrisAirRatio
                
              }else if(nlayers(disDebrisAirRatio) == 1){
                
                # use stationary distributed debris air ratio
                inputDebrisAirRatio <- disDebrisAirRatio * inputGlacierMask
                
              }else{
                
                # use distributed debris air ratio for every time step if available
                inputDebrisAirRatio <- subset(disDebrisAirRatio, i) * inputGlacierMask
                
              }

              # import drag coefficient
              if(nlayers(disDragCoefficient) == 0){
                
                # use general drag coefficient if no distributed drag coefficient is provided
                inputDragCoefficient <- dragCoefficient
                
              }else if(nlayers(disDragCoefficient) == 1){
                
                # use stationary distributed drag coefficient
                inputDragCoefficient <- disDragCoefficient * inputGlacierMask
                
              }else{
                
                # use distributed drag coefficient for every time step if available
                inputDragCoefficient <- subset(disDragCoefficient, i) * inputGlacierMask
                
              }

              # import ice density
              if(nlayers(disIceDensity) == 0){
                
                # use general ice density if no distributed ice density is provided
                inputIceDensity <- iceDensity
                
              }else if(nlayers(disIceDensity) == 1){
                
                # use stationary distributed ice density
                inputIceDensity <- disIceDensity * inputGlacierMask
                
              }else{
                
                # use distributed ice density for every time step if available
                inputIceDensity <- subset(disIceDensity, i) * inputGlacierMask
                
              }
              
              # required parameters
              specificHeatCapacityAir <- 1000
              latentHeatMeltingIce <- 3.34*10^5
              latentHeatWaterEvaporation <- 2.5*10^6
              waterFreezingTemperature <- 273.15
              stefanBoltzmannConstant <- 5.67*10^-8
              slipVelocity <- inputFrictionVelocity
              windSpeedAttenuationConstant <- ( inputDebrisAirRatio * inputDragCoefficient ) / 4
              specificGasConstantAir <- 287.05
              specificGasConstantWater <- 461.5
              
              #######################
              ### data import end ###
              #######################
              
              
              ######################################
              ### glacial melt calculation start ###
              ######################################

              # convert Kelvin into degree Celsius if unit is K 
              if(tUnit == "K"){
                
                inputAirT <- inputAirT - 273.15
                inputAirT <- reclassify(inputAirT, c(-Inf, -272, 0) )
                
              }
              
              # magnus formula: calculation of saturated vapour pressure (Pa)
              saturatedVapourPressure <- 611.2 * exp( (17.62 * inputAirT) / (243.12 + inputAirT) )
              
              # saturated absolute humidity level (kg m-3)
              saturatedHumidityLevel <- round( saturatedVapourPressure / (specificGasConstantWater * (inputAirT  + 273.15) ), 5)
              
              # calculation of absolute humidity
              absoluteHumidity <- inputRH * saturatedHumidityLevel
              
              # calculation of positive air temperatures
              positiveDegree <- reclassify(inputAirT, c(-Inf, 0, 0) )
              
              # calculation of positive net radiation
              if (nlayers(inRadSW) > 0 && nlayers(inRadLW) > 0){
                
                positiveInRadSW <- reclassify(inputInRadSW, c(-Inf, -0, 0) )
                positiveInRadLW <- reclassify(inputInRadLW, c(-Inf, -0, 0) )
                
              }else{
                
                positiveNetRad <- reclassify(inputNetRad, c(-Inf, -0, 0) )
                
              }
              
              # melting is only considered at pixels where air temperatur is > 0 degree Celsius
              if (nlayers(inRadSW) > 0 && nlayers(inRadLW) > 0){
                
                positiveInRadSW <- positiveInRadSW * reclassify(positiveDegree, c(0, Inf, 1) )
                positiveInRadLW <- positiveInRadLW * reclassify(positiveDegree, c(0, Inf, 1) )
                
              }else{
                
                positiveNetRad <- positiveNetRad * reclassify(positiveDegree, c(0, Inf, 1) )
                
              }
              
              # equation 43
              B <- ( inputAirDensity * specificHeatCapacityAir * ( inputFrictionVelocity^2 ) ) /
                ( inputWindSpeed - ( slipVelocity * (2 - ( exp( windSpeedAttenuationConstant * inputSRH ) ) ) ) )
              
              # equation 41
              if (nlayers(inRadSW) > 0 && nlayers(inRadLW) > 0){
                
                v1 <- ( inRadLW - ( inputThermalEmissivity * stefanBoltzmannConstant * ( waterFreezingTemperature^4 ) ) + ( inRadSW * ( 1 - inputDebrisAlbedo) ) + ( B * inputAirT) ) /
                  ( ( 1 - inputVFDI ) * inputIceDensity * latentHeatMeltingIce)
                
              }else {
                
                v1 <- ( positiveNetRad + ( B * (positiveDegree) ) ) /
                  ( ( 1 - inputVFDI ) * inputIceDensity * latentHeatMeltingIce)
                
              }
              
              # equation 42
              v2 <- ( B + ( 4 * inputThermalEmissivity * stefanBoltzmannConstant * ( waterFreezingTemperature^3 ) ) ) / inputThermalConductivity
              
              # equation 44
              u1 <- ( latentHeatWaterEvaporation * ( inputFrictionVelocity^2 ) * ( saturatedHumidityLevel - absoluteHumidity ) * exp( - windSpeedAttenuationConstant * inputSRH ) ) /
                ( ( 1 - inputVFDI ) * inputIceDensity * latentHeatMeltingIce * slipVelocity )
              
              # equation 45
              u2 <- ( ( ( inputWindSpeed - ( 2 * slipVelocity ) ) )  * exp( - windSpeedAttenuationConstant * inputSRH ) ) / slipVelocity
              
              meltRateDebrisCoveredIce <- ( v1 / ( 1 + ( v2 * inputDebrisThickness) ) ) - ( u1 / ( u2 + ( exp( windSpeedAttenuationConstant * inputDebrisThickness ) ) ) )
              
              # conversion from m s-1 to m per predefined time step (e.g. m h-1 or m d-1)
              meltRateDebrisCoveredIce <- (meltRateDebrisCoveredIce * timeAggregation) * inputDebrisMask
              
              # inaccuracy in formula of Evatt et al. 2015 (produces negative melt rates when both, debris thickness and energy input, are very small)
              meltRateDebrisCoveredIce <- reclassify(meltRateDebrisCoveredIce, c(-Inf, 0, 0) )
              
              # round melt rates
              meltRateDebrisCoveredIce <- round(meltRateDebrisCoveredIce, decimalPlaces)
              
              #######################################
              ### debris ice melt calculation end ###
              #######################################
              
              
              ###############################
              ### output generation start ###
              ###############################
              
              # write output (GeoTIFF)
              if(writeOutput == TRUE){
                
                writeRaster(meltRateDebrisCoveredIce, filename = paste(outputName, "_", i, ".tif", sep = ""), overwrite = T, NAflag = -99)
                
              }
              
              if(i == 1){
                
                output <- meltRateDebrisCoveredIce
                
              }else{
                
                output <- output + meltRateDebrisCoveredIce
                
              }
              
              # if individual temporary folder is used, delete files regularly
              # !!! requires future improvement !!!
              if(tmpCreate == TRUE){
                
                # list temporary files (.gri and .grd)
                tmp_gri_files <- list.files(tmpDir, pattern = ".gri", full.names = TRUE)
                tmp_grd_files <- list.files(tmpDir, pattern = ".grd", full.names = TRUE)
                
                # delete temporary files
                if(length(tmp_gri_files) > 50){
                  
                  unlink(tmp_gri_files[1:(length(tmp_gri_files)-45)])
                  unlink(tmp_grd_files[1:(length(tmp_grd_files)-45)])
                  
                }
                
              }
              
            }
            
            # output as mean or sum
            if (outType == "mean"){
              
              output = output / i
              
            }
            
            if (writeOutput == TRUE){
              
              writeRaster(output, filename = paste(outputName, "_1-", i, "_", outType, ".tif", sep = ""), overwrite = T, NAflag = -99)
              
            }
            
            #############################
            ### output generation end ###
            #############################
            
            # output in m timestep-1
            return(output)
             
          }
          
          ###################
          ### iceMelt end ###
          ###################
)
