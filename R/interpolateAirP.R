# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: simple model to interpolate reanalysed pressure data
# latest update: 2017-09-26

setGeneric(name = "interpolateAirP",
           def = function( airP, airT, lapseRate, demDiff, decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
                           outputName = "interpolatedAirP", tmpCreate = TRUE, tmpDir = "", outDir = "" )
           {
             standardGeneric("interpolateAirP")
           }
)

setMethod(f = "interpolateAirP",
          signature = c( "RasterStack", "RasterStack", "numeric", "RasterLayer"),
          definition = function( airP, airT, lapseRate, demDiff, decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
                                 outputName = "interpolatedAirP", tmpCreate = TRUE, tmpDir = "", outDir = "" )
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
            
            # check whether vector length ("lapseRate") and number of layers ("airP") match
            nLayers <- nlayers(airP)
            lapseRateLength <- length(lapseRate)
            
            if ( lapseRateLength != nLayers ){
              
              if (lapseRateLength > nLayers){
                
                print("Warning: Number of elements in numeric vector ('lapseRate') exceeds number of layers in raster stack ('airP')")
                
              }else{
                
                print("Number of elements in numeric vector ('lapseRate') and number of layers in raster stack ('airP') differ")
                print(paste("Number of elements in vector 'lapseRate': ", lapseRateLength, sep=""))
                print(paste("Number of nlayers in rasterStack 'airP': ", nLayers, sep=""))
                print("The mean lapse rate derived from 'lapseRate' is used to replace the missing elements")
                
                meanLapseRate <- round(mean(na.omit(lapseRate)),4)
                lapseRate[(lapseRateLength+1):nLayers] <- meanLapseRate
                
              }
              
            }
            
            for( i in 1:nlayers(airT) ){
              
              # input parameters
              gravity <- 9.81 # m s-2
              gasConstant <- 287.05 # J kg-1 K-1
              
              # input variables
              inputAirP <- subset(airP, i )
              inputAirT <- subset(airT, i )

              inputAirP <- resample(inputAirP, demDiff)
              
              # calculate mean temperature along a vertical profile
              meanAirT <- ( ( inputAirT + ( inputAirT + ( demDiff * -1 * lapseRate[i] ) ) ) / 2 )
              
              # barometric height formula to interpolate air pressure
              interpolatedAirP <- round(inputAirP / exp( ( ( -gravity * ( demDiff ) ) / ( gasConstant * meanAirT ) ) ), 0)
              
              if (writeOutput == TRUE){
                
                writeRaster(interpolatedAirP, filename = paste(outputName, "_", i, ".tif", sep = ""), overwrite = T, na.value = -99)
                
              }
                
              if (i == 1){
                
                output <- interpolatedAirP
                
              }else{
                
                output <- output + interpolatedAirP
                
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
