# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: ablation model
# latest update: 2017-09-26

setGeneric(name = "glacialMelt",
           def = function(airT, airDensity, netRad, glacierMask, iceMask, snowMask, debrisMask, debrisThickness,
                          inRadSW = stack(), inRadLW = stack(), disTuningFacAirT = disTuningFacAirT, disIceTMF = stack(),
                          disSnowTMF = stack(), disIceRMF = stack(), disSnowRMF = stack(), disRelativeHumidity = stack(),
                          disWindSpeed = stack(), disDebrisAlbedo = stack(), disThermalConductivity = stack(),
                          disThermalEmissivity = stack(), disSurfaceRoughnessHeight = stack(), disFrictionVelocity = stack(),
                          disVolumeFractionDebrisInIce = stack(), disDebrisAirRatio = stack(), disDragCoefficient = stack(),
                          disIceDensity = stack(), outType = "mean", writeOutput = c(0, 0, 0, 0), outputName = "glacialMelt",
                          tmpCreate = FALSE, tmpDir = "", outDir = "", ...)
           {
             standardGeneric("glacialMelt")
           }
)

setMethod(f = "glacialMelt",
          signature = rep("RasterStack", 8),
          definition = function(airT, airDensity, netRad, glacierMask, iceMask, snowMask, debrisMask, debrisThickness,
                                inRadSW = stack(), inRadLW = stack(), disIceTMF = stack(), disSnowTMF = stack(),
                                disIceRMF = stack(), disSnowRMF = stack(), disRelativeHumidity = stack(), disWindSpeed = stack(),
                                disDebrisAlbedo = stack(), disThermalConductivity = stack(), disThermalEmissivity = stack(),
                                disSurfaceRoughnessHeight = stack(), disFrictionVelocity = stack(), disVolumeFractionDebrisInIce = stack(),
                                disDebrisAirRatio = stack(), disDragCoefficient = stack(), disIceDensity = stack(),
                                outType = "mean", writeOutput = c(0, 0, 0, 0), outputName = "glacialMelt", tmpCreate = FALSE,
                                tmpDir = "", outDir = "", ...)
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
            
            # suppress 'R CMD check' NOTE 'no visible binding for global variable'
            output1 <- raster()
            output2 <- raster()
            output3 <- raster()
            output4 <- raster()
            
            # rename input variables and optional arguments (necessary for iteration if nlayers(x) > 1)
            inputAirT <- airT
            inputAirDensity <- airDensity
            inputNetRad <- netRad
            inputGlacierMask <- glacierMask
            inputIceMask <- iceMask
            inputSnowMask <- snowMask
            inputDebrisMask <- debrisMask
            inputDebrisThickness <- debrisThickness
            
            if (!missing(inRadSW) && !missing(inRadLW)){
              
              inputInRadSW <- inRadSW
              inputInRadLW <- inRadLW
              
            }else{
              
              inputInRadSW <- stack()
              inputInRadLW <- stack()
              
            }
            
            if(!missing(disIceTMF)){
              
              inputDisIceTMF <- disIceTMF
              
            }else{
              
              inputDisIceTMF <- stack()
              
            }
            
            if(!missing(disSnowTMF)){
              
              inputDisSnowTMF <- disSnowTMF
              
            }else{
              
              inputDisSnowTMF <- stack()
              
            }
            
            if(!missing(disIceRMF)){
              
              inputDisIceRMF <- disIceRMF
              
            }else{
              
              inputDisIceRMF <- stack()
              
            }
            
            if(!missing(disSnowRMF)){
              
              inputDisSnowRMF <- disSnowRMF
              
            }else{
              
              inputDisSnowRMF <- stack()
              
            }
            
            if(!missing(disTuningFacAirT)){
              
              inputDisTuningFacAirT <- disTuningFacAirT
              
            }else{
              
              inputDisTuningFacAirT <- stack()
              
            }
            
            if(!missing(disRelativeHumidity)){
              
              inputDisRelativeHumidity <- disRelativeHumidity
              
            }else{
              
              inputDisRelativeHumidity <- stack()
              
            }
            
            if(!missing(disWindSpeed)){
              
              inputDisWindSpeed <- disWindSpeed
              
            }else{
              
              inputDisWindSpeed <- stack()
              
            }
            
            if(!missing(disDebrisAlbedo)){
              
              inputDisDebrisAlbedo <- disDebrisAlbedo
              
            }else{
              
              inputDisDebrisAlbedo <- stack()
              
            }
            
            if(!missing(disThermalConductivity)){
              
              inputDisThermalConductivity <- disThermalConductivity
              
            }else{
              
              inputDisThermalConductivity <- stack()
              
            }
            
            if(!missing(disThermalEmissivity)){
              
              inputDisThermalEmissivity <- disThermalEmissivity
              
            }else{
              
              inputDisThermalEmissivity <- stack()
              
            }
            
            if(!missing(disSurfaceRoughnessHeight)){
              
              inputDisSurfaceRoughnessHeight <- disSurfaceRoughnessHeight
              
            }else{
              
              inputDisSurfaceRoughnessHeight <- stack()
              
            }
            
            if(!missing(disFrictionVelocity)){
              
              inputDisFrictionVelocity <- disFrictionVelocity
              
            }else{
              
              inputDisFrictionVelocity <- stack()
              
            }
            
            if(!missing(disVolumeFractionDebrisInIce)){
              
              inputDisVolumeFractionDebrisInIce <- disVolumeFractionDebrisInIce
              
            }else{
              
              inputDisVolumeFractionDebrisInIce <- stack()
              
            }
            
            if(!missing(disDebrisAirRatio)){
              
              inputDisDebrisAirRatio <- disDebrisAirRatio
              
            }else{
              
              inputDisDebrisAirRatio <- stack()
              
            }
            
            if(!missing(disDragCoefficient)){
              
              inputDisDragCoefficient <- disDragCoefficient
              
            }else{
              
              inputDisDragCoefficient <- stack()
              
            }
            
            if(!missing(disIceDensity)){
              
              inputDisIceDensity <- disIceDensity
              
            }else{
              
              inputDisIceDensity <- stack()
              
            }
            
            #########################
            ### glacialMelt start ###
            #########################
            
            for( i in 1:nlayers(airT) ){
              
              
              #########################
              ### data import start ###
              #########################
              
              # import glacier mask
              if(nlayers(inputGlacierMask) > 1){
                
                # use updated masks for every time step if available
                glacierMask <- subset(inputGlacierMask, i)
                glacierMask <- stack(glacierMask)
                
              }else{
                
                # use stationary mask
                glacierMask <- inputGlacierMask
                
              }
              
              # import ice mask
              if(nlayers(inputIceMask) > 1){
                
                # use updated masks for every time step if available
                iceMask <- subset(inputIceMask, i)
                iceMask <- stack(iceMask)
                
              }else{
                
                # use stationary mask
                iceMask <- inputIceMask
                
              }
              
              # import snow mask
              if(nlayers(inputSnowMask) > 1){
                                
                # use updated masks for every time step if available
                snowMask <- subset(inputSnowMask, i)
                snowMask <- stack(snowMask)
                
              }else{
                
                # use stationary mask
                snowMask <- inputSnowMask
                
              }
              
              # import debris mask
              if(nlayers(inputDebrisMask) > 1){
                
                # use updated masks for every time step if available
                debrisMask <- subset(inputDebrisMask, i)
                debrisMask <- stack(debrisMask)
                
              }else{
                
                # use stationary mask
                debrisMask <- inputDebrisMask
                
              }
              
              # import distributed debris thickness
              if(nlayers(inputDebrisThickness) > 1){
                
                # use updated masks for every time step if available
                debrisThickness <- subset(inputDebrisThickness, i)
                debrisThickness <- stack(debrisThickness)
                
              }else{
                
                # use distributed stationary debris thickness
                debrisThickness <- inputDebrisThickness
                
              }            
                           
              # import air temperature
              airT <- subset(inputAirT, i)
              airT <- stack(airT)
              
              # import air density
              if(nlayers(inputAirDensity) > 1){
                
                # use distributed air density for every time step if available
                airDensity <- subset(inputAirDensity, i)
                airDensity <- stack(airDensity)
                
              }else{
                
                # use distributed stationary air density
                airDensity <- inputAirDensity
                
              }
              
              # import short- and longwave and net radiation
              if (!missing(inRadSW) && !missing(inRadLW)){
                
                inRadSW <- subset(inputInRadSW, i)
                inRadSW <- stack(inRadSW)
                inRadLW <- subset(inputInRadLW, i)
                inRadLW <- stack(inRadLW)
                netRad <- subset(inputNetRad, i)
                netRad <- stack(netRad)
                
              }else{
                
                netRad <- subset(inputNetRad, i)
                netRad <- stack(netRad)
                
              }
              
              # import melting factors
              
              # import temperature melting factors
              if(nlayers(inputDisIceTMF) > 1){
                
                # use distributed temperature melting factor for every time step if available
                disIceTMF <- subset(inputDisIceTMF, i)
                disIceTMF <- stack(disIceTMF)
                
              }else{
                
                # use distributed stationary melting factor
                disIceTMF <- inputDisIceTMF
                
              }
              
              if(nlayers(inputDisSnowTMF) > 1){
                
                # use distributed temperature melting factor for every time step if available
                disSnowTMF <- subset(inputDisSnowTMF, i)
                disSnowTMF <- stack(disSnowTMF)
                
              }else{
                
                # use distributed stationary melting factor
                disSnowTMF <- inputDisSnowTMF
                
              }

              # import radiative melting factors
              if(nlayers(inputDisIceRMF) > 1){
                
                # use distributed radiative melting factor for every time step if available
                disIceRMF <- subset(inputDisIceRMF, i)
                disIceRMF <- stack(disIceRMF)
                
              }else{
                
                # use distributed stationary melting factor
                disIceRMF <- inputDisIceRMF
                
              }
              
              if(nlayers(inputDisSnowRMF) > 1){
                
                # use distributed radiative melting factor for every time step if available
                disSnowRMF <- subset(inputDisSnowRMF, i)
                disSnowRMF <- stack(disSnowRMF)
                
              }else{
                
                # use distributed stationary melting factor
                disSnowRMF <- inputDisSnowRMF
                
              }
              
              # import air temperature tuning factor
              if(nlayers(inputDisTuningFacAirT) > 1){
                
                # use distributed tuning factor for every time step if available
                disTuningFacAirT <- subset(inputDisTuningFacAirT, i)
                disTuningFacAirT <- stack(disTuningFacAirT)
                
              }else{
                
                # use distributed stationary tuning factor
                disTuningFacAirT <- inputDisTuningFacAirT
                
              }
              
              # import relative humidity
              if(nlayers(inputDisRelativeHumidity) > 1){
                
                # use distributed relative humidity for every time step if available
                disRelativeHumidity <- subset(inputDisRelativeHumidity, i)
                disRelativeHumidity <- stack(disRelativeHumidity)
                
              }else{
                
                # use distributed stationary relative humidity
                disRelativeHumidity <- inputDisRelativeHumidity
                
              }
              
              # import wind speed
              if(nlayers(inputDisWindSpeed) > 1){
                
                # use distributed wind speed for every time step if available
                disWindSpeed <- subset(inputDisWindSpeed, i)
                disWindSpeed <- stack(disWindSpeed)
                
              }else{
                
                # use distributed stationary wind speed
                disWindSpeed <- inputDisWindSpeed
                
              }    
              
              # import debris albedo
              if(nlayers(inputDisDebrisAlbedo) > 1){
                
                # use distributed debris albedo for every time step if available
                disDebrisAlbedo <- subset(inputDisDebrisAlbedo, i)
                disDebrisAlbedo <- stack(disDebrisAlbedo)
                
              }else{
                
                # use distributed stationary debris albedo
                disDebrisAlbedo <- inputDisDebrisAlbedo
                
              }

              # import thermal conductivity
              if(nlayers(inputDisThermalConductivity) > 1){
                
                # use distributed thermal conductivity for every time step if available
                disThermalConductivity <- subset(inputDisThermalConductivity, i)
                disThermalConductivity <- stack(disThermalConductivity)
                
              }else{
                
                # use distributed stationary thermal conductivity
                disThermalConductivity <- inputDisThermalConductivity
                
              }
              
              # import thermal emissivity
              if(nlayers(inputDisThermalEmissivity) > 1){
                
                # use distributed thermal emissivity for every time step if available
                disThermalEmissivity <- subset(inputDisThermalEmissivity, i)
                disThermalEmissivity <- stack(disThermalEmissivity)
                
              }else{
                
                # use distributed stationary thermal emissivity
                disThermalEmissivity <- inputDisThermalEmissivity
                
              }
              
              # import surface roughness height
              if(nlayers(inputDisSurfaceRoughnessHeight) > 1){
                
                # use distributed surface roughness height for every time step if available
                disSurfaceRoughnessHeight <- subset(inputDisSurfaceRoughnessHeight, i)
                disSurfaceRoughnessHeight <- stack(disSurfaceRoughnessHeight)
                
              }else{
                
                # use distributed stationary surface roughness height
                disSurfaceRoughnessHeight <- inputDisSurfaceRoughnessHeight
                
              }
              
              # import friction velocity
              if(nlayers(inputDisFrictionVelocity) > 1){
                
                # use distributed friction velocity for every time step if available
                disFrictionVelocity <- subset(inputDisFrictionVelocity, i)
                disFrictionVelocity <- stack(disFrictionVelocity)
                
              }else{
                
                # use distributed stationary friction velocity
                disFrictionVelocity <- inputDisFrictionVelocity
                
              }
              
              
              # import debris volume fraction in ice
              if(nlayers(inputDisVolumeFractionDebrisInIce) > 1){
                
                # use distributed debris volume fraction in ice for every time step if available
                disVolumeFractionDebrisInIce <- subset(inputDisVolumeFractionDebrisInIce, i)
                disVolumeFractionDebrisInIce <- stack(disVolumeFractionDebrisInIce)
                
              }else{
                
                # use distributed stationary debris volume fraction in ice
                disVolumeFractionDebrisInIce <- inputDisVolumeFractionDebrisInIce
                
              }
              
              # import debris air ratio
              if(nlayers(inputDisDebrisAirRatio) > 1){
                
                # use distributed debris air ratio for every time step if available
                disDebrisAirRatio <- subset(inputDisDebrisAirRatio, i)
                disDebrisAirRatio <- stack(disDebrisAirRatio)
                
              }else{
                
                # use distributed stationary debris air ratio
                disDebrisAirRatio <- inputDisDebrisAirRatio
                
              }
              
              # import drag coefficient
              if(nlayers(inputDisDragCoefficient) > 1){
                
                # use distributed drag coefficient for every time step if available
                disDragCoefficient <- subset(inputDisDragCoefficient, i)
                disDragCoefficient <- stack(disDragCoefficient)
                
              }else{
                
                # use distributed stationary drag coefficient
                disDragCoefficient <- inputDisDragCoefficient
                
              }
              
              # import ice density
              if(nlayers(inputDisIceDensity) > 1){
                
                # # use distributed ice density for every time step if available
                disIceDensity <- subset(inputDisIceDensity, i)
                disIceDensity <- stack(disIceDensity)
                
              }else{
                
                # use distributed stationary ice density
                disIceDensity <- inputDisIceDensity
                
              }
              
              #######################
              ### data import end ###
              #######################
              
                            
              ######################################
              ### glacial melt calculation start ###
              ######################################
              
              # calculating melt rate of debris covered ice
              meltRateDebrisCoveredIce <- debrisCoveredIceMelt(airT = airT, airDensity = airDensity, netRad = netRad,
                                                               glacierMask = glacierMask, debrisMask = debrisMask,
                                                               debrisThickness = debrisThickness, inRadSW = inRadSW,
                                                               inRadLW = inRadLW, disTuningFacAirT = disTuningFacAirT,
                                                               disRelativeHumidity = disRelativeHumidity, disWindSpeed = disWindSpeed,
                                                               disDebrisAlbedo = disDebrisAlbedo, disThermalConductivity = disThermalConductivity,
                                                               disThermalEmissivity = disThermalEmissivity, disSurfaceRoughnessHeight = disSurfaceRoughnessHeight,
                                                               disFrictionVelocity = disFrictionVelocity, disVolumeFractionDebrisInIce = disVolumeFractionDebrisInIce,
                                                               disDebrisAirRatio = disDebrisAirRatio, disDragCoefficient = disDragCoefficient,
                                                               disIceDensity = disIceDensity, tmpCreate = FALSE, writeOutput = FALSE, ...)
              
              # calculating melt rate of clean ice
              meltRateIce <- iceMelt(airT = airT, netRad = netRad, glacierMask = glacierMask, iceMask = iceMask, disIceTMF = disIceTMF,
                                     disIceRMF = disIceRMF, disTuningFacAirT = disTuningFacAirT, tmpCreate = FALSE, writeOutput = FALSE, ...)
              
              # calculating melt rate of snow
              meltRateSnow <- snowMelt(airT = airT, netRad = netRad, glacierMask = glacierMask, snowMask = snowMask, disSnowTMF = disSnowTMF,
                                       disSnowRMF = disSnowRMF, disTuningFacAirT = disTuningFacAirT, tmpCreate = FALSE, writeOutput = FALSE, ...)
              
              # calculating total glacial ablation
              totalAblation <- meltRateIce + meltRateSnow + meltRateDebrisCoveredIce
              
              ablationOutput <- stack(meltRateSnow, meltRateIce, meltRateDebrisCoveredIce, totalAblation)
              
              ####################################
              ### glacial melt calculation end ###
              ####################################
              
              
              ###############################
              ### output generation start ###
              ###############################
              
              # output variable names
              outputVariableNames <- c(paste("_01_", sep = ""),
                                       paste("_02_", sep = ""),
                                       paste("_03_", sep = ""),
                                       paste("_04_", sep = ""))
              
              # only export predefined variables
              for (j in which(writeOutput[1:4] == 1)){
                
                writeRaster(subset(ablationOutput, j),
                            filename = paste(outputName, outputVariableNames[j], i, ".tif", sep = ""),
                            NAflag = -99, overwrite = T)
                
              }
              
              if (i == 1){
                
                output1 <- meltRateSnow
                output2 <- meltRateIce
                output3 <- meltRateDebrisCoveredIce
                output4 <- totalAblation
                
              }else{
                
                output1 <- output1 + meltRateSnow
                output2 <- output2 + meltRateIce
                output3 <- output3 + meltRateDebrisCoveredIce
                output4 <- output4 + totalAblation
                
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
              
              output1 = output1 / i
              output2 = output2 / i
              output3 = output3 / i
              output4 = output4 / i
              
            }
            
            output <- stack(output1, output2, output3, output4)
            
            for (j in which(writeOutput[1:4] == 1)){
              
              writeRaster(subset(ablationOutput, j), filename = paste(outputName, outputVariableNames[j], "_1-", i, "_", outType, ".tif", sep = ""), overwrite = T, NAflag = -99)
              
            }
            
            #############################
            ### output generation end ###
            #############################

            # output in m d-1
            return( output )
            
          }
          
          #######################
          ### glacialMelt end ###
          #######################
)