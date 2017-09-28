# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: ebris thickness fit
# latest update: 2017-09-26

setGeneric(name = "debrisThicknessFit",
           def = function(surfaceTemperature, debrisThickness, plotOutput = FALSE)
           {
             standardGeneric("debrisThicknessFit")
           }
)

setMethod(f = "debrisThicknessFit",
          signature = c("numeric", "numeric"),
          definition = function(surfaceTemperature, debrisThickness, plotOutput = FALSE)
          {
            
            # initial parameters for fitting function
            fp1 <- 0.01
            fp2 <- 1
            
            # fitting function (cf. Mihalcea et al. 2006, 2008 and Minora et al. 2015)
            fit <- nls(debrisThickness ~ exp( ( fp1 * surfaceTemperature ) - fp2 ), start = list(fp1 = fp1, fp2 = fp2) )
            
            # fitting paramters
            fitPar1 <- round(as.numeric(coef(fit)[1]), 4)
            fitPar2 <- round(as.numeric(coef(fit)[2]), 4)
            
            if (plotOutput == TRUE){
              
              # calculate debris thickness
              dtCalc <- exp( ( fitPar1 * surfaceTemperature ) - fitPar2 )
              
              # calculate fitting curve
              dtCurve <- exp( ( fitPar1 * seq( min(surfaceTemperature), max(surfaceTemperature), length = length(dtCalc)*10 ) )  - fitPar2 )
              
              # plot surfaceTemperature vs. measured debris thickness
              plot(surfaceTemperature, debrisThickness, xlab = "Glacier surface temperature [K]", ylab = "Debris thickness [m]",
                   ylim = c(0,max(debrisThickness)), xlim = c(min(surfaceTemperature),max(surfaceTemperature)), pch = 19)
              
              # add fitting curve
              lines(surfaceTemperature, dtCalc, t = "p", col= "blue", pch = 19)
              
              # add surfaceTemperature vs. calculated debris thickness
              lines(seq(min(surfaceTemperature), max(surfaceTemperature), length = length(dtCalc)*10), dtCurve)
              
              # add legend
              legend("topleft", legend = c("measured", "best fit"), col = c("black", "blue"), lty = c(NA, 1), pch = c(19, 19), bty = "n")
              
            }
            
            # return fitting parameters
            return( c(fitPar1, fitPar2) )
            
          }
)
