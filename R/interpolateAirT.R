# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: simple model to interpolate reanalysed air temperature data
# latest update: 2017-09-26

setGeneric(name = "interpolateAirT",
           def = function( airT, lapseRate, demDiff, decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
                           outputName = "interpolatedAirT", tmpCreate = TRUE, tmpDir = "", outDir = "" )
           {
             standardGeneric("interpolateAirT")
           }
)

setMethod(f = "interpolateAirT",
          signature = c( "RasterStack", "numeric", "RasterLayer"),
          definition = function( airT, lapseRate, demDiff, decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
                                 outputName = "interpolatedAirT", tmpCreate = TRUE, tmpDir = "", outDir = "" )
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
            
            # check whether vector length ("lapseRate") and number of layers ("airT") match
            nLayers <- nlayers(airT)
            lapseRateLength <- length(lapseRate)
            
            if ( lapseRateLength != nLayers ){
              
              if (lapseRateLength > nLayers){
                
                print("Warning: Number of elements in numeric vector ('lapseRate') exceeds number of layers in raster stack ('airT')")
                
              }else{
                
                print("Number of elements in numeric vector ('lapseRate') and number of layers in raster stack ('airT') differ")
                print(paste("Number of elements in vector 'lapseRate': ", lapseRateLength, sep=""))
                print(paste("Number of nlayers in rasterStack 'airT': ", nLayers, sep=""))
                print("The mean lapse rate derived from 'lapseRate' is used to replace the missing elements")
                
                meanLapseRate <- round(mean(na.omit(lapseRate)),4)
                lapseRate[(lapseRateLength+1):nLayers] <- meanLapseRate
                
              }
              
            }
            
            for( i in 1:nlayers(airT) ){
              
              inputAirT <- subset(airT, i )
              
              inputAirT <- resample(inputAirT, demDiff)
              
              interpolatedAirT <- inputAirT + ( demDiff * lapseRate[i] )
                            
              if (writeOutput == TRUE){
                
                writeRaster(interpolatedAirT, filename = paste(outputName, "_", i, ".tif", sep = ""), overwrite = T, na.value = -99)
                
              }
                
              if (i == 1){
                
                output <- interpolatedAirT
                
              }else{
                
                output <- output + interpolatedAirT
                
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
            
            output <- round(output, decimalPlaces)
            
            # output as mean or sum
            if (outType == "mean"){
              
              output = output / i
              
            }
            
            if (writeOutput == TRUE){
              
              writeRaster(output, filename = paste(outputName, "_1-", i, "_", outType, ".tif", sep = ""), overwrite = T, NAflag = -99)
              
            }
            
            return( output )
            
          }
          
)
