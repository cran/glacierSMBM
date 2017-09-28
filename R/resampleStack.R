# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: resample RasterStack objects
# latest update: 2017-09-26

setGeneric(name = "resampleStack",
           def = function( rasterStack, resampleSettings, decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
                           outputName = "resample", tmpCreate = FALSE, tmpDir = "", outDir = "", ... )
           {
             standardGeneric("resampleStack")
           }
)

setMethod(f = "resampleStack",
          signature = c( "RasterStack", "RasterLayer"),
          definition = function( rasterStack, resampleSettings, decimalPlaces = 4, outType = "mean", writeOutput = FALSE,
                                 outputName = "resample", tmpCreate = FALSE, tmpDir = "", outDir = "", ... )
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
            if (!hasArg(u1) && !hasArg(u2)){
              
              u1 <- NULL
              u2 <- NULL
              
            }
            
            for( i in 1:nlayers(rasterStack) ){
              
              rasterLayer <- subset(rasterStack, i )
              
              resampledRasterLayer <- resample(x = rasterLayer, y = resampleSettings,  method='bilinear')
              resampledRasterLayer <- resampledRasterLayer * reclassify(resampleSettings, c(-Inf,Inf, 1))
              
              if (hasArg(u1) && hasArg(u2)){
                
                resampledRasterLayer <- unitConv(resampledRasterLayer, ...)
                
              }
              
              resampledRasterLayer <- round(resampledRasterLayer, decimalPlaces)
              
              if (writeOutput == TRUE){
                
                writeRaster(resampledRasterLayer, filename = paste(outputName, "_", i, ".tif", sep = ""), overwrite = T, NAflag = -99)
                
              }
              
              if (i == 1){
                
                output <- resampledRasterLayer
                
              }else{
                
                output <- output + resampledRasterLayer
                
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
              
            return( output )
            
          }
          
)