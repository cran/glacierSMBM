# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: a distributed model to simulate glacier surface mass balance processes
# latest update: 2017-09-26

setGeneric(name = "glacierSMBM",
           def = function(inputGlacierSMBM, ...)
           {
             standardGeneric("glacierSMBM")
           }
)

setMethod(f = "glacierSMBM",
          signature = c("inputGlacierSMBM"),
          definition = function(inputGlacierSMBM, ...)
          {
            # define output directory
            if(nchar(inputGlacierSMBM@outDir) > 0){
              
              setwd(inputGlacierSMBM@outDir)
              
            }
            
            # optional: create temporary directory for processing large files
            if( inputGlacierSMBM@tmpCreate == TRUE & inputGlacierSMBM@tmpDir > 0){
              
              rasterOptions(tmpdir = inputGlacierSMBM@tmpDir )
              tmpDir(create = TRUE)
              
            }
            
            # create empty matrix for glacierSMBM output
            outputGlacierSMBM <- as.data.frame(matrix(NA, ncol = 17, nrow = nlayers(inputGlacierSMBM@airT)))
            colnames(outputGlacierSMBM) <- c("Date", "Accumulation [rate]", "Accumulation [sum]", "Ablation [rate]",
                                             "Ablation [sum]", "Ice Melt [rate]", "Ice Melt [sum]",
                                             "Debris Covered Ice Melt [rate]", "Debris Covered Ice Melt [sum]",
                                             "Firn Melt [rate]", "Firn Melt [sum]", "Snow Melt [rate]",
                                             "Snow Melt [sum]", "Snow Height [rate]", "Snow Height [sum]",
                                             "Mass Balance [rate]", "Mass Balance [sum]")
            
            
            #############################
            ### data input test start ###
            #############################
            
            # not implemented yet
            
            ###########################
            ### data input test end ###
            ###########################
            
            
            #########################
            ### glacierSMBM start ###
            #########################
            
            print("Start: glacierSMBM")
            
            for(i in 1:nlayers(inputGlacierSMBM@airT)){
              
              # print time as well as number of started processing steps
              print(Sys.time())
              print(paste("processing step ",i, "/", length(inputGlacierSMBM@date)," (", inputGlacierSMBM@date[i], ")", sep = ""))
              
              
              #########################
              ### data import start ###
              #########################
              
              # import glacier mask
              if(nlayers(inputGlacierSMBM@glacierMask) == 1){
                
                # use stationary mask if only one is provided
                glacierMask <- inputGlacierSMBM@glacierMask
                
              }else{
                
                # use updated masks for every time step if available
                glacierMask <- subset(inputGlacierSMBM@glacierMask, i)
                
              }
              
              # import ice mask
              if(nlayers(inputGlacierSMBM@iceMask) == 1){
                
                # use stationary mask if only one is provided
                iceMask <- inputGlacierSMBM@iceMask
                
              }else{
                
                # use updated masks for every time step if available
                iceMask <- subset(inputGlacierSMBM@iceMask, i)
                
              }
              
              # import firn mask
              if(nlayers(inputGlacierSMBM@firnMask) == 1){
                
                # use stationary mask if only one is provided
                snowMask <- inputGlacierSMBM@firnMask
                
              }else{
                
                # use updated masks for every time step if available
                snowMask <- subset(inputGlacierSMBM@firnMask, i)
                
              }
              
              # import debris mask
              if(nlayers(inputGlacierSMBM@debrisMask) == 1){
                
                # use stationary mask if only one is provided
                debrisMask <- inputGlacierSMBM@debrisMask
                
              }else{
                
                # use updated masks for every time step if available
                debrisMask <- subset(inputGlacierSMBM@debrisMask, i)
                
              }
              
              # import distributed debris thickness
              if(nlayers(inputGlacierSMBM@debrisThickness) == 1){
                
                # use stationary distributed debris thickness
                debrisThickness <- inputGlacierSMBM@debrisThickness
                
              }else{
                
                # use updated debris thickness for every time step if available
                debrisThickness <- subset(inputGlacierSMBM@debrisThickness, i)
                
              }
 
              # import determined number of decimal places
              decimalPlaces <- inputGlacierSMBM@decimalPlaces
              
              # determine unit of air temperature
              tUnit <- inputGlacierSMBM@tUnit
              
              # import air temperature tuning factor
              if(nlayers(inputGlacierSMBM@disTuningFacAirT) == 0){
                
                # use general tuning factor if no distributed tuning factor is provided
                airtTuningFactor <- inputGlacierSMBM@tuningFacAirT
                tuningFacAirT <- inputGlacierSMBM@tuningFacAirT
                disTuningFacAirT <- stack()
                
              }else if(nlayers(inputGlacierSMBM@disTuningFacAirT) == 1){
                
                # use stationary distributed tuning factor
                airtTuningFactor <- inputGlacierSMBM@disTuningFacAirT * glacierMask
                disTuningFacAirT <- inputGlacierSMBM@disTuningFacAirT * glacierMask
                
              }else{
                
                # use distributed tuning factor for every time step if available
                airtTuningFactor <- subset(inputGlacierSMBM@disTuningFacAirT, i) * glacierMask
                disTuningFacAirT <- subset(inputGlacierSMBM@disTuningFacAirT, i) * glacierMask
                
              }
              
              # import precipitation tuning factor
              if(nlayers(inputGlacierSMBM@disTuningFacPrecip) == 0){
                
                # use general precipitation tuning factor if no distributed precipitation tuning factor is provided
                precipTuningFactor <- inputGlacierSMBM@tuningFacPrecip
                tuningFacPrecip <- inputGlacierSMBM@tuningFacPrecip
                
              }else if(nlayers(inputGlacierSMBM@disTuningFacPrecip) == 1){
                
                # use stationary distributed precipitation tuning factor
                precipTuningFactor <- inputGlacierSMBM@disTuningFacPrecip * glacierMask
                disTuningFacPrecip <- inputGlacierSMBM@disTuningFacPrecip * glacierMask
                
              }else{
                
                # use distributed precipitation tuning factor for every time step if available
                precipTuningFactor <- subset(inputGlacierSMBM@disTuningFacPrecip, i) * glacierMask
                disTuningFacPrecip <- subset(inputGlacierSMBM@disTuningFacPrecip, i) * glacierMask
                disTuningFacPrecip <- stack()
                
              }
              
              # import air temperature
              airT <- ( subset(inputGlacierSMBM@airT, i) * airtTuningFactor ) * glacierMask
              
              # if unit is degree Celsius convert into kelvin
              if(tUnit == "C"){
                
                airT <- airT + 273.15
                
              }
              
              airT <- stack(airT)
              
              # import air density
              if(nlayers(inputGlacierSMBM@airDensity) == 1){
                
                # use stationary distributed air density
                airDensity <- inputGlacierSMBM@airDensity * glacierMask
                airDensity <- stack(airDensity)
                
              }else{
                
                # use distributed air density for every time step if available
                airDensity <- subset(inputGlacierSMBM@airDensity, i) * glacierMask
                airDensity <- stack(airDensity)
                
              }
              
              # import net radiation and if available incoming short- and longwave radiation
              if(nlayers(inputGlacierSMBM@inRadSW) > 0 && nlayers(inputGlacierSMBM@inRadLW) > 0){
                
                netRad <- subset(inputGlacierSMBM@netRad, i) * glacierMask
                netRad <- stack(netRad)
                inRadSW <- subset(inputGlacierSMBM@inRadSW, i) * glacierMask
                inRadSW <- stack(inRadSW)
                inRadLW <- subset(inputGlacierSMBM@inRadLW, i) * glacierMask
                inRadLW <- stack(inRadLW)
                
              }else{
                
                netRad <- subset(inputGlacierSMBM@netRad, i) * glacierMask
                netRad <- stack(netRad)
                
              }
              
              # determine snow rain transition temperature
              snowTransTempThreshold <- inputGlacierSMBM@snowTransTempThreshold
              
              # if unit is degree Celsius convert into kelvin
              if(tUnit == "C"){
                
                snowTransTempThreshold <- snowTransTempThreshold + 273.15
                
              }
              
              # import precipitation
              if(nlayers(inputGlacierSMBM@snowfall) > 0){
                
                # import snowfall if provided
                snowFall <- (subset(inputGlacierSMBM@snowfall, i) * precipTuningFactor) * glacierMask
              
              }else{
                
                # import precipitation files as meteorological input if no snowfall files are available
                precip <- subset(inputGlacierSMBM@precip, i) * glacierMask
                precip <- stack(precip)
                
                # convert precipitation to snowfall when air temperatures are below transition threshold
                snowFall <- snowFall(airT = airT, precip = precip, glacierMask = glacierMask,
                                     snowTransTempThreshold = snowTransTempThreshold,
                                     tuningFacPrecip = tuningFacPrecip, disTuningFacPrecip = disTuningFacPrecip,
                                     tuningFacAirT = tuningFacAirT, disTuningFacAirT = disTuningFacAirT,
                                     tmpCreate = FALSE, writeOutput = FALSE, ...)
                
              }
              
              # import initial snow height file
              if (i == 1){
                
                initialSnowHeight <- inputGlacierSMBM@snowHeight
                
                # use first snowfall file as initial "snow height" if initial snow height is not provided
                if(nlayers(initialSnowHeight) == 0){
                  
                  initialSnowHeight <- snowFall
                  
                }
                
                presentSnowHeight <- initialSnowHeight * glacierMask
                
              }else{

                # present snow height is sum of previous snow height and fresh snow
                presentSnowHeight <- round( (previousSnowHeight + snowFall), decimalPlaces)
                
              }
              
              # import optional arguments for the functions iceMelt and snowMelt
              iceTMF <- inputGlacierSMBM@iceTMF
              snowTMF <- inputGlacierSMBM@snowTMF
              iceRMF <- inputGlacierSMBM@iceRMF
              snowRMF <- inputGlacierSMBM@snowRMF
              
              # import temperature melting factors

              if(nlayers(inputGlacierSMBM@disIceTMF) > 1){
                
                # use distributed temperature melting factor for every time step if available
                disIceTMF <- subset(inputGlacierSMBM@disIceTMF, i)
                disIceTMF <- stack(disIceTMF)
                
              }else{
                
                # use distributed stationary melting factor
                disIceTMF <- inputGlacierSMBM@disIceTMF
                
              }
              
              if(nlayers(inputGlacierSMBM@disSnowTMF) > 1){
                
                # use distributed temperature melting factor for every time step if available
                disSnowTMF <- subset(inputGlacierSMBM@disSnowTMF, i)
                disSnowTMF <- stack(disSnowTMF)
                
              }else{
                
                # use distributed stationary melting factor
                disSnowTMF <- inputGlacierSMBM@disSnowTMF
                
              }
              
              # import radiative melting factors
              if(nlayers(inputGlacierSMBM@disIceRMF) > 1){
                
                # use distributed radiative melting factor for every time step if available
                disIceRMF <- subset(inputGlacierSMBM@disIceRMF, i)
                disIceRMF <- stack(disIceRMF)
                
              }else{
                
                # use distributed stationary melting factor
                disIceRMF <- inputGlacierSMBM@disIceRMF
                
              }
              
              if(nlayers(inputGlacierSMBM@disSnowRMF) > 1){
                
                # use distributed radiative melting factor for every time step if available
                disSnowRMF <- subset(inputGlacierSMBM@disSnowRMF, i)
                disSnowRMF <- stack(disSnowRMF)
                
              }else{
                
                # use distributed stationary melting factor
                disSnowRMF <- inputGlacierSMBM@disSnowRMF
                
              }
              
              ###########################
              ###########################
              ###########################
              
              # import optional arguments for the function desbrisCoveredIceMelt
              tmpRes <- inputGlacierSMBM@tmpRes
              measurementHeight <- inputGlacierSMBM@measurementHeight
              relativeHumidity <- inputGlacierSMBM@relativeHumidity
              windSpeed <- inputGlacierSMBM@windSpeed
              debrisAlbedo <- inputGlacierSMBM@debrisAlbedo              
              thermalConductivity <- inputGlacierSMBM@thermalConductivity
              thermalEmissivity <- inputGlacierSMBM@thermalEmissivity
              surfaceRoughnessHeight <- inputGlacierSMBM@surfaceRoughnessHeight
              frictionVelocity <- inputGlacierSMBM@frictionVelocity
              volumeFractionDebrisInIce <- inputGlacierSMBM@volumeFractionDebrisInIce
              debrisAirRatio <- inputGlacierSMBM@debrisAirRatio
              dragCoefficient <- inputGlacierSMBM@dragCoefficient
              iceDensity <- inputGlacierSMBM@iceDensity
              
              # import relative humidity
              if(nlayers(inputGlacierSMBM@disRelativeHumidity) > 1){
                
                # use distributed relative humidity for every time step if available
                disRelativeHumidity <- subset(inputGlacierSMBM@disRelativeHumidity, i)
                disRelativeHumidity <- stack(disRelativeHumidity)
                
              }else{
                
                # use distributed stationary relative humidity
                disRelativeHumidity <- inputGlacierSMBM@disRelativeHumidity
                
              }
              
              # import wind speed
              if(nlayers(inputGlacierSMBM@disWindSpeed) > 1){
                
                # use distributed wind speed for every time step if available
                disWindSpeed <- subset(inputGlacierSMBM@disWindSpeed, i)
                disWindSpeed <- stack(disWindSpeed)
                
              }else{
                
                # use distributed stationary wind speed
                disWindSpeed <- inputGlacierSMBM@disWindSpeed
                
              }    
              
              # import debris albedo
              if(nlayers(inputGlacierSMBM@disDebrisAlbedo) > 1){
                
                # use distributed debris albedo for every time step if available
                disDebrisAlbedo <- subset(inputGlacierSMBM@disDebrisAlbedo, i)
                disDebrisAlbedo <- stack(disDebrisAlbedo)
                
              }else{
                
                # use distributed stationary debris albedo
                disDebrisAlbedo <- inputGlacierSMBM@disDebrisAlbedo
                
              }
              
              # import thermal conductivity
              if(nlayers(inputGlacierSMBM@disThermalConductivity) > 1){
                
                # use distributed thermal conductivity for every time step if available
                disThermalConductivity <- subset(inputGlacierSMBM@disThermalConductivity, i)
                disThermalConductivity <- stack(disThermalConductivity)
                
              }else{
                
                # use distributed stationary thermal conductivity
                disThermalConductivity <- inputGlacierSMBM@disThermalConductivity
                
              }
              
              # import thermal emissivity
              if(nlayers(inputGlacierSMBM@disThermalEmissivity) > 1){
                
                # use distributed thermal emissivity for every time step if available
                disThermalEmissivity <- subset(inputGlacierSMBM@disThermalEmissivity, i)
                disThermalEmissivity <- stack(disThermalEmissivity)
                
              }else{
                
                # use distributed stationary thermal emissivity
                disThermalEmissivity <- inputGlacierSMBM@disThermalEmissivity
                
              }
              
              # import surface roughness height
              if(nlayers(inputGlacierSMBM@disSurfaceRoughnessHeight) > 1){
                
                # use distributed surface roughness height for every time step if available
                disSurfaceRoughnessHeight <- subset(inputGlacierSMBM@disSurfaceRoughnessHeight, i)
                disSurfaceRoughnessHeight <- stack(disSurfaceRoughnessHeight)
                
              }else{
                
                # use distributed stationary surface roughness height
                disSurfaceRoughnessHeight <- inputGlacierSMBM@disSurfaceRoughnessHeight
                
              }
              
              # import friction velocity
              if(nlayers(inputGlacierSMBM@disFrictionVelocity) > 1){
                
                # use distributed friction velocity for every time step if available
                disFrictionVelocity <- subset(inputGlacierSMBM@disFrictionVelocity, i)
                disFrictionVelocity <- stack(disFrictionVelocity)
                
              }else{
                
                # use distributed stationary friction velocity
                disFrictionVelocity <- inputGlacierSMBM@disFrictionVelocity
                
              }
              
              
              # import debris volume fraction in ice
              if(nlayers(inputGlacierSMBM@disVolumeFractionDebrisInIce) > 1){
                
                # use distributed debris volume fraction in ice for every time step if available
                disVolumeFractionDebrisInIce <- subset(inputGlacierSMBM@disVolumeFractionDebrisInIce, i)
                disVolumeFractionDebrisInIce <- stack(disVolumeFractionDebrisInIce)
                
              }else{
                
                # use distributed stationary debris volume fraction in ice
                disVolumeFractionDebrisInIce <- inputGlacierSMBM@disVolumeFractionDebrisInIce
                
              }
              
              # import debris air ratio
              if(nlayers(inputGlacierSMBM@disDebrisAirRatio) > 1){
                
                # use distributed debris air ratio for every time step if available
                disDebrisAirRatio <- subset(inputGlacierSMBM@disDebrisAirRatio, i)
                disDebrisAirRatio <- stack(disDebrisAirRatio)
                
              }else{
                
                # use distributed stationary debris air ratio
                disDebrisAirRatio <- inputGlacierSMBM@disDebrisAirRatio
                
              }
              
              # import drag coefficient
              if(nlayers(inputGlacierSMBM@disDragCoefficient) > 1){
                
                # use distributed drag coefficient for every time step if available
                disDragCoefficient <- subset(inputGlacierSMBM@disDragCoefficient, i)
                disDragCoefficient <- stack(disDragCoefficient)
                
              }else{
                
                # use distributed stationary drag coefficient
                disDragCoefficient <- inputGlacierSMBM@disDragCoefficient
                
              }
              
              # import ice density
              if(nlayers(inputGlacierSMBM@disIceDensity) > 1){
                
                # # use distributed ice density for every time step if available
                disIceDensity <- subset(inputGlacierSMBM@disIceDensity, i)
                disIceDensity <- stack(disIceDensity)
                
              }else{
                
                # use distributed stationary ice density
                disIceDensity <- inputGlacierSMBM@disIceDensity
                
              }
              
              #######################
              ### data import end ###
              #######################
              
              ######################################
              ### mass balance calculation start ###
              ######################################
              
              # new snow height (if no ablation occurs)
              newSnowHeight <- presentSnowHeight
              
              # in the current version of the model, ablation can only occur if air temperature is above the freezing point
              if (cellStats(airT, stat = max) > 273.15){

                # supraglacial snow melt only takes place if snow is present somewhere on the glacier (snow height > 0 m)
                if (cellStats(presentSnowHeight, stat = max) > 0){
                  
                  snowLayer <- stack(reclassify(presentSnowHeight, c(-Inf, 0, 0, 0, Inf, 1)))
                  
                  # new snow height is substraction of present snow height and amount of melted snow
                  snowmelt <- snowMelt(airT = airT, netRad = netRad, glacierMask = glacierMask,
                                       snowMask = snowLayer, tUnit = "K", snowTMF = snowTMF,
                                       disSnowTMF = disSnowTMF, snowRMF = snowRMF, disSnowRMF = disSnowRMF,
                                       tuningFacAirT = tuningFacAirT, disTuningFacAirT = disTuningFacAirT,
                                       decimalPlaces = decimalPlaces, tmpCreate = FALSE, writeOutput = FALSE, ...)
                  
                  newSnowHeight <- presentSnowHeight - snowmelt

                  # use remaining energy for ablation of underlying glacier ice if more energy is available than snow to be melted
                  if (cellStats(newSnowHeight, stat = min) < 0){

                    # determine the amount of energy free to be used for ice ablation
                    excessEnergy <- reclassify(newSnowHeight, c(0, Inf, 0)) * -1
                    
                    # snow melt
                    snowmelt <- snowmelt - excessEnergy
                    
                    # remove "negative" snowheights
                    newSnowHeight <- newSnowHeight + excessEnergy
                    
                    # snow mask update
                    snowLayerOld <- snowLayer
                    snowLayer <- reclassify(newSnowHeight, c(-Inf, 0, 0, 0, Inf, 1))
                    
                    # snow free areas
                    snowFreeArea <- reclassify(presentSnowHeight, c(-Inf, 0, 1, 0, Inf, 0))
                    
                    # new snow free areas
                    newSnowFreeArea <- snowLayerOld - snowLayer
                    
                    # net radiation and air temperature proportion of the energy used to melt the snow
                    meltProportionAirT <- (reclassify(airT, c(-Inf, 0, 0)) * snowTMF) / ( (reclassify(airT, c(-Inf, 0, 0)) * snowTMF) + (reclassify(netRad, c(-Inf, 0, 0)) * snowRMF) )
                    meltProportionNetRad <- 1 - meltProportionAirT
                    
                    # energy (proportion of positive degrees) left for ice ablation of (new) snow-free areas
                    airTnew <- ( airT * snowFreeArea ) + ( ( ( ( excessEnergy / snowTMF ) + 273.15 ) * meltProportionAirT )* newSnowFreeArea )
                    
                    # energy (proportion of positive net radiation) left for ice ablation of (new) snow-free areas
                    netRadnew <- ( netRad * snowFreeArea ) + ( ( ( excessEnergy / snowRMF ) * meltProportionNetRad )* newSnowFreeArea )
                    
                    # calculate glacial ablation of snow-free areas
                    glacialMelt <- glacialMelt(airT = stack(airTnew), netRad = stack(netRadnew), airDensity = airDensity,
                                               glacierMask = glacierMask, iceMask = iceMask, snowMask = snowMask, debrisMask = debrisMask,
                                               debrisThickness = debrisThickness, tUnit = "K", iceTMF = iceTMF, disIceTMF = disIceTMF,
                                               iceRMF = iceRMF, disIceRMF = disIceRMF, snowTMF = snowTMF, disSnowTMF = disSnowTMF,
                                               snowRMF = snowRMF, disSnowRMF = disSnowRMF, tuningFacAirT = tuningFacAirT,
                                               disTuningFacAirT = disTuningFacAirT, relativeHumidity = relativeHumidity, disRelativeHumidity = disRelativeHumidity,
                                               windSpeed = windSpeed, disWindSpeed = disWindSpeed, debrisAlbedo = debrisAlbedo, disDebrisAlbedo = disDebrisAlbedo,
                                               thermalConductivity = thermalConductivity, disThermalConductivity = disThermalConductivity,
                                               thermalEmissivity = thermalEmissivity, disThermalEmissivity = disThermalEmissivity,
                                               surfaceRoughnessHeight = surfaceRoughnessHeight, disSurfaceRoughnessHeight = disSurfaceRoughnessHeight,
                                               frictionVelocity = frictionVelocity, disFrictionVelocity = disFrictionVelocity,
                                               volumeFractionDebrisInIce = volumeFractionDebrisInIce, disVolumeFractionDebrisInIce = disVolumeFractionDebrisInIce,
                                               debrisAirRatio = debrisAirRatio, disDebrisAirRatio = disDebrisAirRatio, dragCoefficient = dragCoefficient,
                                               disDragCoefficient = disDragCoefficient, iceDensity = iceDensity, disIceDensity = disIceDensity,
                                               decimalPlaces = decimalPlaces, tmpCreate = FALSE, writeOutput = FALSE, ...)

                  }else{
                    
                    # calculate glacial melt of snow free areas
                    
                    # calculate snow free areas
                    snowFreeArea <- reclassify(presentSnowHeight, c(-Inf, 0, 1, 0, Inf, 0))
                    
                    airT <- airT * snowFreeArea
                    airT <- stack(airT)
                    
                    netRad <- netRad * snowFreeArea
                    netRad <- stack(netRad)
                    
                    # calculate glacial ablation of snow-free areas
                    glacialMelt <- glacialMelt(airT = airT, netRad = netRad, airDensity = airDensity, glacierMask = glacierMask,
                                               iceMask = iceMask, snowMask = snowMask, debrisMask = debrisMask, debrisThickness = debrisThickness,
                                               iceTMF = iceTMF, disIceTMF = disIceTMF, iceRMF = iceRMF, disIceRMF = disIceRMF, snowTMF = snowTMF,
                                               disSnowTMF = disSnowTMF, snowRMF = snowRMF, disSnowRMF = disSnowRMF, tuningFacAirT = tuningFacAirT,
                                               disTuningFacAirT = disTuningFacAirT, relativeHumidity = relativeHumidity, disRelativeHumidity = disRelativeHumidity,
                                               windSpeed = windSpeed, disWindSpeed = disWindSpeed, debrisAlbedo = debrisAlbedo, disDebrisAlbedo = disDebrisAlbedo,
                                               thermalConductivity = thermalConductivity, disThermalConductivity = disThermalConductivity,
                                               thermalEmissivity = thermalEmissivity, disThermalEmissivity = disThermalEmissivity,
                                               surfaceRoughnessHeight = surfaceRoughnessHeight, disSurfaceRoughnessHeight = disSurfaceRoughnessHeight,
                                               frictionVelocity = frictionVelocity, disFrictionVelocity = disFrictionVelocity,
                                               volumeFractionDebrisInIce = volumeFractionDebrisInIce, disVolumeFractionDebrisInIce = disVolumeFractionDebrisInIce,
                                               debrisAirRatio = debrisAirRatio, disDebrisAirRatio = disDebrisAirRatio, dragCoefficient = dragCoefficient,
                                               disDragCoefficient = disDragCoefficient, iceDensity = iceDensity, disIceDensity = disIceDensity,
                                               decimalPlaces = decimalPlaces, tUnit = "K", tmpCreate = FALSE,
                                               writeOutput = FALSE, ...)
                    
                  }
                
                }else{
                  
                  # no snow melt
                  snowmelt <- reclassify( presentSnowHeight, c(-Inf, Inf, 0) )
                  snowLayer <- snowmelt
                  
                  # calculate glacial melt
                  glacialMelt <- glacialMelt(airT = airT, netRad = netRad, airDensity = airDensity, glacierMask = glacierMask,
                                             iceMask = iceMask, snowMask = snowMask, debrisMask = debrisMask, debrisThickness = debrisThickness,
                                             iceTMF = iceTMF, disIceTMF = disIceTMF, iceRMF = iceRMF, disIceRMF = disIceRMF, snowTMF = snowTMF,
                                             disSnowTMF = disSnowTMF, snowRMF = snowRMF, disSnowRMF = disSnowRMF, tuningFacAirT = tuningFacAirT,
                                             disTuningFacAirT = disTuningFacAirT, relativeHumidity = relativeHumidity, disRelativeHumidity = disRelativeHumidity,
                                             windSpeed = windSpeed, disWindSpeed = disWindSpeed, debrisAlbedo = debrisAlbedo, disDebrisAlbedo = disDebrisAlbedo,
                                             thermalConductivity = thermalConductivity, disThermalConductivity = disThermalConductivity,
                                             thermalEmissivity = thermalEmissivity, disThermalEmissivity = disThermalEmissivity,
                                             surfaceRoughnessHeight = surfaceRoughnessHeight, disSurfaceRoughnessHeight = disSurfaceRoughnessHeight,
                                             frictionVelocity = frictionVelocity, disFrictionVelocity = disFrictionVelocity,
                                             volumeFractionDebrisInIce = volumeFractionDebrisInIce, disVolumeFractionDebrisInIce = disVolumeFractionDebrisInIce,
                                             debrisAirRatio = debrisAirRatio, disDebrisAirRatio = disDebrisAirRatio, dragCoefficient = dragCoefficient,
                                             disDragCoefficient = disDragCoefficient, iceDensity = iceDensity, disIceDensity = disIceDensity,
                                             decimalPlaces = decimalPlaces, tUnit = "K", tmpCreate = FALSE,
                                             writeOutput = FALSE, ...)
                  
                }

                
              }else{

                # neither snow nor glacial melt occurs if temperature is below freezing
                snowmelt <- reclassify( presentSnowHeight, c(-Inf, Inf, 0) )
                
                snowLayer <- reclassify( newSnowHeight, c(0, Inf, 1) )
                
                glacialMelt <- stack(snowmelt, snowmelt, snowmelt, snowmelt)
                
              }
              
              # calculate surface mass balance
              surfaceMassBalance <- snowFall - ( subset(glacialMelt, 4) + snowmelt )
              
              ####################################
              ### mass balance calculation end ###
              ####################################


              ###############################
              ### output generation start ###
              ###############################
              
              # output: date
              outputGlacierSMBM[i, 1] <- as.character(inputGlacierSMBM@date[i])
              
              # output: accumulation [rate (e.g. m d-1)]
              outputGlacierSMBM[i,2] <- round(cellStats(snowFall, stat = mean), inputGlacierSMBM@decimalPlaces)
              
              # output: ablation [rate (e.g. m d-1)], clean ice melt [rate], debris covered ice melt [rate]
              outputGlacierSMBM[i,4] <- round(cellStats(subset(glacialMelt,4), stat = mean), inputGlacierSMBM@decimalPlaces) +
                round(cellStats(snowmelt, stat = mean), inputGlacierSMBM@decimalPlaces)
              outputGlacierSMBM[i,6] <- round(cellStats(subset(glacialMelt,2), stat = mean), inputGlacierSMBM@decimalPlaces)
              outputGlacierSMBM[i,8] <- round(cellStats(subset(glacialMelt,3), stat = mean), inputGlacierSMBM@decimalPlaces)
              
              # output: firn and snow melt [rate (e.g. m d-1)]
              outputGlacierSMBM[i,10] <- round(cellStats(subset(glacialMelt,1), stat = mean), inputGlacierSMBM@decimalPlaces)
              outputGlacierSMBM[i,12] <- round(cellStats(snowmelt, stat = mean), inputGlacierSMBM@decimalPlaces)
              
              # output: snow height [m]
              outputGlacierSMBM[i,15] = round(cellStats(newSnowHeight, stat = mean), inputGlacierSMBM@decimalPlaces)
              
              # output: surface mass balance [rate (e.g. m d-1)]
              outputGlacierSMBM[i,16] = round(cellStats(surfaceMassBalance, stat = mean), inputGlacierSMBM@decimalPlaces)
              
              if(i == 1){
                
                # output: accumulation in [m]
                outputGlacierSMBM[i,3] <- round(cellStats(snowFall, stat = mean), inputGlacierSMBM@decimalPlaces)
                
                # output: ablation [m], clean ice melt [m], debris covered ice melt [m]
                outputGlacierSMBM[i,5] <- outputGlacierSMBM[i,4]
                outputGlacierSMBM[i,7] <- outputGlacierSMBM[i,6]
                outputGlacierSMBM[i,9] <- outputGlacierSMBM[i,8]
                
                # output: firn and snow melt [m] as well as snow height changes [rate (e.g. m d-1)]
                outputGlacierSMBM[i,11] <- outputGlacierSMBM[i,10]
                outputGlacierSMBM[i,13] <- outputGlacierSMBM[i,12]
                outputGlacierSMBM[i,14] <- outputGlacierSMBM[i,15]
                
                # output: surface mass balance [m]
                outputGlacierSMBM[i,17] <- outputGlacierSMBM[i,16]
                
                # define temporally integrated glacier SMBM output
                accumulationSum <- snowFall
                ablationSum <- subset(glacialMelt, 4)
                iceMeltSum <- subset(glacialMelt, 2)
                debrisCoveredIceMeltSum <- subset(glacialMelt, 3)
                firnMeltSum <- subset(glacialMelt, 1)
                snowMeltSum <- snowmelt
                previousSnowHeight <- reclassify(newSnowHeight, c(-Inf, Inf, 0) )
                surfaceMassBalanceSum <- surfaceMassBalance
                
              }else{
                
                # output: accumulation in [m]
                outputGlacierSMBM[i,3] <- round(outputGlacierSMBM[(i-1),3] + outputGlacierSMBM[i,2], inputGlacierSMBM@decimalPlaces)
                
                # output: ablation [m], clean ice melt [m], debris covered ice melt [m]
                outputGlacierSMBM[i,5] <- round(outputGlacierSMBM[(i-1),5] + outputGlacierSMBM[i,4], inputGlacierSMBM@decimalPlaces)
                outputGlacierSMBM[i,7] <- round(outputGlacierSMBM[(i-1),7] + outputGlacierSMBM[i,6], inputGlacierSMBM@decimalPlaces)
                outputGlacierSMBM[i,9] <- round(outputGlacierSMBM[(i-1),9] + outputGlacierSMBM[i,8], inputGlacierSMBM@decimalPlaces)
                
                # output: snow and firn melt [m]
                outputGlacierSMBM[i,11] <- round(outputGlacierSMBM[(i-1),11] + outputGlacierSMBM[i,10], inputGlacierSMBM@decimalPlaces)
                outputGlacierSMBM[i,13] <- round(outputGlacierSMBM[(i-1),13] + outputGlacierSMBM[i,12], inputGlacierSMBM@decimalPlaces)
                
                # output: snow height change [m]
                outputGlacierSMBM[i,14] <- round(outputGlacierSMBM[i,15] - outputGlacierSMBM[(i-1),15], inputGlacierSMBM@decimalPlaces)
                
                # output: surface mass balance [m]
                outputGlacierSMBM[i,17] <- round(outputGlacierSMBM[(i-1),17] + outputGlacierSMBM[i,16], inputGlacierSMBM@decimalPlaces)
                
                # define temporally integrated glacier SMBM output
                accumulationSum <- accumulationSum + snowFall
                ablationSum <- ablationSum + subset(glacialMelt,4) + snowmelt
                iceMeltSum <- iceMeltSum + subset(glacialMelt,2)
                debrisCoveredIceMeltSum <- debrisCoveredIceMeltSum + subset(glacialMelt, 3)
                firnMeltSum <- firnMeltSum + subset(glacialMelt,1)
                snowMeltSum <- snowMeltSum + snowmelt
                surfaceMassBalanceSum <- surfaceMassBalanceSum + surfaceMassBalance
                
              }
              
              # ouput raster stack
              ouputRasterStack <- stack(snowFall, accumulationSum, subset(glacialMelt, 4), ablationSum,
                                        subset(glacialMelt, 2), iceMeltSum, subset(glacialMelt, 3),
                                        debrisCoveredIceMeltSum, subset(glacialMelt, 1), firnMeltSum, snowmelt,
                                        snowMeltSum, snowLayer, newSnowHeight,
                                        surfaceMassBalance, surfaceMassBalanceSum)
              
              # output date
              outputDate <- strftime(inputGlacierSMBM@date[i], format = inputGlacierSMBM@timeStamp)
              periodSMBM <- paste(strftime(inputGlacierSMBM@date[1], format = inputGlacierSMBM@timeStamp), "_",
                                  strftime(inputGlacierSMBM@date[nlayers(inputGlacierSMBM@airT)],
                                           format = inputGlacierSMBM@timeStamp), sep = "")
              
              # output variable names
              outputVariableNames <- c(paste("_01_", outputDate, sep = ""),
                                       paste("_02_", periodSMBM, sep = ""),
                                       paste("_03_", outputDate, sep = ""),
                                       paste("_04_", periodSMBM, sep = ""),
                                       paste("_05_", outputDate, sep = ""),
                                       paste("_06_", periodSMBM, sep = ""),
                                       paste("_07_", outputDate, sep = ""),
                                       paste("_08_", periodSMBM, sep = ""),
                                       paste("_09_", outputDate, sep = ""),
                                       paste("_10_", periodSMBM, sep = ""),
                                       paste("_11_", outputDate, sep = ""),
                                       paste("_12_", periodSMBM, sep = ""),
                                       paste("_13_", outputDate, sep = ""),
                                       paste("_14_", periodSMBM, sep = ""),
                                       paste("_15_", outputDate, sep = ""),
                                       paste("_16_", periodSMBM, sep = ""))
              
              # only export predefined variables
              for (j in which(inputGlacierSMBM@writeOutput[1:16] == 1)){
                
                writeRaster(subset(ouputRasterStack, j),
                            filename = paste(inputGlacierSMBM@outputName, outputVariableNames[j], ".tif", sep = ""),
                            NAflag = -99, overwrite = T)
                
              }
              
              # export glacierSMBM output (overwrite for every iteration)
              if (inputGlacierSMBM@writeOutput[17] == 1){
                
                write.table(outputGlacierSMBM, paste(inputGlacierSMBM@outputName, ".txt", sep = ""), sep = inputGlacierSMBM@outputSep, row.names = F)
                
              }
              
              ###############################
              ### output generation end ###
              ###############################
              
              # plot output
              #if (plotOutput == T){
                
              #  add plot function
                
              #}
              
              # define snow height for next iteration
              previousSnowHeight <- newSnowHeight
              
              
              # if individual temporary folder is used, delete files regularly
              # !!! requires further improvement !!!
              if(inputGlacierSMBM@tmpCreate == TRUE){
                
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
            
            return(subset(ouputRasterStack, 16))
            
            print("End: glacierSMBM")
            
            #########################
            ### glacierSMBM end ###
            #########################

          }
)
