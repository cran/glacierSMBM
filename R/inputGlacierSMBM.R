# author: alexander raphael groos (alexander.groos@giub.unibe.ch)
# class for glacierSMBM input
# latest update: 2017-09-26

setClass("inputGlacierSMBM",
         slots = c(
           # date
           date = "POSIXct",
           # time stamp
           timeStamp = "character",
           # directories
           outDir = "character",
           tmpDir = "character",
           # decimal places,
           decimalPlaces = "numeric",
           # write output
           writeOutput = "numeric",
           # delimiter for output table
           outputSep = "character",
           # plot output
           plotOutput = "logical",
           # name for output GeoTIFF(s)
           outputName = "character",
           # creating a temporary directory
           tmpCreate = "logical",
           # meteorological input variables
           airT = "RasterStack",
           airDensity = "RasterStack",
           inRadSW = "RasterStack",
           inRadLW = "RasterStack",
           netRad = "RasterStack",
           snowfall = "RasterStack",
           precip = "RasterStack",
           snowHeight = "RasterStack",
           # glacier surface masks
           glacierMask = "RasterStack",
           iceMask = "RasterStack",
           firnMask = "RasterStack",
           debrisMask = "RasterStack",
           debrisThickness = "RasterStack",
           # temperature unit
           tUnit = "character",
           # melting factors
           iceTMF = "numeric",
           disIceTMF = "RasterStack",
           snowTMF = "numeric",
           disSnowTMF = "RasterStack",
           iceRMF = "numeric",
           disIceRMF = "RasterStack",
           snowRMF = "numeric",
           disSnowRMF = "RasterStack",
           # tuning factors
           tuningFacAirT = "numeric",
           disTuningFacAirT = "RasterStack",
           tuningFacPrecip = "numeric",
           disTuningFacPrecip = "RasterStack",
           # rain snow transtion temperature
           snowTransTempThreshold = "numeric",
           # debris characteristics
           tmpRes = "character",
           measurementHeight = "numeric",
           relativeHumidity = "numeric",
           disRelativeHumidity = "RasterStack",
           windSpeed = "numeric",
           disWindSpeed = "RasterStack",
           debrisAlbedo = "numeric",
           disDebrisAlbedo = "RasterStack",
           thermalConductivity = "numeric",
           disThermalConductivity = "RasterStack",
           thermalEmissivity = "numeric",
           disThermalEmissivity = "RasterStack",
           surfaceRoughnessHeight  = "numeric",
           disSurfaceRoughnessHeight = "RasterStack",
           frictionVelocity = "numeric",
           disFrictionVelocity = "RasterStack",
           volumeFractionDebrisInIce = "numeric",
           disVolumeFractionDebrisInIce = "RasterStack",
           debrisAirRatio = "numeric",
           disDebrisAirRatio = "RasterStack",
           dragCoefficient = "numeric",
           disDragCoefficient = "RasterStack",
           iceDensity = "numeric",
           disIceDensity = "RasterStack"
         ),
         prototype = list(
           # date
           date = as.POSIXct(NA),
           # time stamp
           timeStamp = "%Y-%m-%d",
           # directories
           outDir = "",
           tmpDir = "",
           # decimal places,
           decimalPlaces = 4,
           # write output
           writeOutput = c(rep(0, 15), 1, 1),
           # delimiter for output table
           outputSep = "\t",
           # plot output
           plotOutput = FALSE,
           # name for output GeoTIFF(s)
           outputName = "glacierSMBM_Output",
           # creating a temporary directory
           tmpCreate = FALSE,
           # temperature unit
           tUnit = "K",
           # melting factors
           iceTMF = 67*10^-4,
           snowTMF = 45*10^-4,
           iceRMF = 0.79*10^-4,
           snowRMF = 0.53*10^-4,
           # tuning factors
           tuningFacAirT = 1,
           tuningFacPrecip = 1,
           snowTransTempThreshold = 274.15,
           # debris characteristics
           tmpRes = "d",
           measurementHeight = 2,
           relativeHumidity = 0.73,
           windSpeed = 2,
           debrisAlbedo = 0.07,
           thermalConductivity = 0.585,
           thermalEmissivity = 0.95,
           surfaceRoughnessHeight  = 0.01,
           frictionVelocity = 0.16,
           volumeFractionDebrisInIce = 0.01,
           debrisAirRatio = 188,
           dragCoefficient = 5,
           iceDensity = 900
         )
         )