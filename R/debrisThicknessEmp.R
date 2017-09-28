# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# function: empirical debris thickness model
# latest update: 2017-09-26

setGeneric(name = "debrisThicknessEmp",
           def = function(fittingParameters, surfaceTemperature = c(), disSurfaceTemperature = stack(), decimalPlaces = 4)
           {
             standardGeneric("debrisThicknessEmp")
           }
)

setMethod(f = "debrisThicknessEmp",
          signature = c("numeric"),
          definition = function(fittingParameters, surfaceTemperature = c(), disSurfaceTemperature = stack(), decimalPlaces = 4)
          {
            
            # import surface temperature
            if(nlayers(disSurfaceTemperature) == 0){
              
              # use selected surface temperatures if no surface temperature distribution is provided
              surfaceTemperature <- surfaceTemperature
              
            }else{
              
              # use surface temperature distribution
              surfaceTemperature <- subset(disSurfaceTemperature, 1)
              
            }
            
            # fitting parameters calculated with function "debrisThicknessFit"
            fitPar1 <- fittingParameters[1]
            fitPar2 <- fittingParameters[2]
            
            # debris thickness calculation (cf. Mihalcea et al. 2006, 2008 and Minora et al. 2015)
            debrisThickness <- exp( ( fitPar1 * (surfaceTemperature) ) - fitPar2 )
            
            # round debris thickness
            debrisThickness <- round(debrisThickness, decimalPlaces)
            
            return( debrisThickness )
            
          }
)