# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: a simple model to derive snowfall from precipitation and air temperature
# latest update: 2017-09-26

setGeneric(name = "snowFall",
           def = function( airT, precip, glacierMask, snowTransTempThreshold = 274.15, tuningFacPrecip = 1, disTuningFacPrecip = stack(),
                           tuningFacAirT = 1, disTuningFacAirT = stack(), decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
                           outputName = "snowfall", tmpCreate = FALSE, tmpDir = "", outDir = "", ... )
           {
             standardGeneric("snowFall")
           }
)

setMethod(f = "snowFall",
          signature = rep("RasterStack", 3),
          definition = function( airT, precip, glacierMask, snowTransTempThreshold = 274.15, tuningFacPrecip = 1, disTuningFacPrecip = stack(),
                                 tuningFacAirT = 1, disTuningFacAirT = stack(), decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
                                 outputName = "snowfall", tmpCreate = FALSE, tmpDir = "", outDir = "", ... )
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
            
            
            ######################
            ### snowFall start ###
            ######################
            
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
              
              # import precipitation tuning factor
              if(nlayers(disTuningFacPrecip) == 0){
                
                # use general precipitation tuning factor if no distributed precipitation tuning factor is provided
                precipTuningFactor <- tuningFacPrecip
                
              }else if(nlayers(disTuningFacPrecip) == 1){
                
                # use stationary distributed precipitation tuning factor
                precipTuningFactor <- disTuningFacPrecip * inputGlacierMask
                
              }else{
                
                # use distributed precipitation tuning factor for every time step if available
                precipTuningFactor <- subset(disTuningFacPrecip, i) * inputGlacierMask
                
              }
              
              # import air temperature
              inputAirT <- ( subset(airT, i) * airtTuningFactor ) * inputGlacierMask
              
              # import precipitation
              inputPrecip <- ( subset(precip, i) * precipTuningFactor ) * inputGlacierMask
              
              #######################
              ### data import end ###
              #######################
              
              
              ##################################
              ### snowfall calculation start ###
              ##################################
              
              # derive snowfall from precipitation and air temperature
              negativeAirT <- reclassify( inputAirT, c(-Inf, snowTransTempThreshold, 1, snowTransTempThreshold, Inf, 0) )
              solidPrecip <- inputPrecip * negativeAirT
              
              # round
              solidPrecip <- round(solidPrecip, decimalPlaces)
              
              ################################
              ### snowfall calculation end ###
              ################################
              
              
              ###############################
              ### output generation start ###
              ###############################
              
              if (writeOutput == TRUE){
                
                writeRaster(solidPrecip, filename = paste(outputName, "_", i, ".tif", sep = ""), overwrite = T, NAflag = -99)
                
              }
              
              if (i == 1){
                
                output <- solidPrecip
                
              }else{
                
                output <- output + solidPrecip
                
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
            
            return( output )
            
          }
          
          ####################
          ### snowFall end ###
          ####################
          
)