# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: ice melt model
# latest update: 2017-09-26

setGeneric(name = "iceMelt",
           def = function(airT, netRad, glacierMask, iceMask,  tUnit = "K", iceTMF = 67*10^-4, disIceTMF = stack(),
                          iceRMF = 0.79*10^-4, disIceRMF = stack(), tuningFacAirT = 1, disTuningFacAirT = stack(),
                          decimalPlaces = 4, outType = "mean", writeOutput = FALSE, outputName = "iceMelt",
                          tmpCreate = FALSE, tmpDir = "", outDir = "", ... )
           {
             standardGeneric("iceMelt")
           }
)

setMethod(f = "iceMelt",
          signature = rep("RasterStack", 4),
          definition = function(airT, netRad, glacierMask, iceMask,  tUnit = "K", iceTMF = 67*10^-4, disIceTMF = stack(),
                                iceRMF = 0.79*10^-4, disIceRMF = stack(), tuningFacAirT = 1, disTuningFacAirT = stack(),
                                decimalPlaces = 4, outType = "mean", writeOutput = FALSE, outputName = "iceMelt",
                                tmpCreate = FALSE, tmpDir = "", outDir = "", ... )
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
            
            #####################
            ### iceMelt start ###
            #####################
            
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
              
              # import ice mask
              if(nlayers(iceMask) == 1){
                
                # use stationary mask if only one is provided
                inputIceMask <- iceMask
                
              }else{
                
                # use updated masks for every time step if available
                inputIceMask <- subset(iceMask, i)
                
              }
              
              # import temperature melting factors
              if(nlayers(disIceTMF) == 0){
                
                # use general temperature melting factor if no distributed temperature melting factor is provided
                inputIceTMF <- iceTMF
                
              }else if(nlayers(disIceTMF) == 1){
                
                # use stationary distributed temperature melting factor
                inputIceTMF <- disIceTMF * inputGlacierMask
                
              }else{
                
                # use distributed temperature melting factor for every time step if available
                inputIceTMF <- subset(disIceTMF, i) * inputGlacierMask
                
              }
              
              # import radiative melting factors
              if(nlayers(disIceRMF) == 0){
                
                # use general radiative melting factor if no distributed radiative melting factor is provided
                inputIceRMF <- iceRMF
                
              }else if(nlayers(disIceRMF) == 1){
                
                # use stationary distributed radiative melting factor
                inputIceRMF <- disIceRMF * inputGlacierMask
                
              }else{
                
                # use distributed radiative melting factor for every time step if available
                inputIceRMF <- subset(disIceRMF, i) * inputGlacierMask
                
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
              
              # import net radiation
              inputNetRad <- subset(netRad, i) * inputGlacierMask
              
              #######################
              ### data import end ###
              #######################
              
              
              ##################################
              ### ice melt calculation start ###
              ##################################
              
              # convert Kelvin into degree Celsius if unit is K 
              if(tUnit == "K"){
                
                inputAirT <- inputAirT - 273.15
                inputAirT <- reclassify(inputAirT, c(-Inf, -272, 0) )
                
              }
              
              # calculation of positive degrees
              positiveDegree <- stack( reclassify(inputAirT, c(-Inf, -0, 0) ) )
			  
              # calculation of positive net radiation
              positiveNetRad <- reclassify(inputNetRad, c(-Inf, -0, 0) )
              
              # melting is only considered at pixels where air temperatur is > 0 degree Celsius
              positiveNetRad <- positiveNetRad * reclassify(inputAirT, c(-Inf, 0, 0, 0, Inf, 1) )
              
              meltRateIce <- ( (inputIceTMF * positiveDegree) + (inputIceRMF * positiveNetRad) ) * inputIceMask
              
              # round
              meltRateIce <- round(meltRateIce, decimalPlaces)
             
              ################################
              ### ice melt calculation end ###
              ################################
              
              
              ###############################
              ### output generation start ###
              ###############################
              
              # write output (GeoTIFF)
              if (writeOutput == TRUE){
                
                writeRaster(meltRateIce, filename = paste(outputName, "_", i, ".tif", sep = ""), overwrite = T, NAflag = -99)
                
              }
              
              if (i == 1){
                
                output <- stack(meltRateIce)
                
              }else{
                
                output <- output + meltRateIce
                
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
            
            return(output)
            
          }
          
          ###################
          ### iceMelt end ###
          ###################
)
