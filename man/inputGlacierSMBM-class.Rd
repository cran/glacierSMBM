\name{inputGlacierSMBM-class}
\Rdversion{1.1}
\docType{class}
\alias{inputGlacierSMBM-class}
\title{Class \code{inputGlacierSMBM}}
\description{
A class consisting of parameters and variables required to run and modify \code{'glacierSMBM'}.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("inputGlacierSMBM", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{date}:}{An object of class \code{'POSIXct'}. Date and time of each \code{RasterLayer} in the \code{RasterStack}. Required.}
    \item{\code{timeStamp}:}{An object of class \code{'POSIXct'}. Date format used in the output table.}
    \item{\code{outDir}:}{An object of class \code{'character'}. Directory for the output files if \code{'writeOutput'} = TRUE.}
    \item{\code{tmpDir}:}{An object of class \code{'character'}. Directory where processing files can be temporarily stored if \code{'tmpCreate'} = TRUE.}
    \item{\code{tmpCreate}:}{An object of class \code{'logical'}.  Determines whether a temporary directory should be used (TRUE) or not (FALSE, default). Recommendend if large datasets are processed.}
    \item{\code{decimalPlaces}:}{An object of class \code{'numeric'}. Number of decimal places (default = 4).}
    \item{\code{writeOutput}:}{An object of class \code{'character'}. Determines which of the seventeen outputs shall be exported as \code{RasterLayer} (1) and which not (0). For more information see section values.}
    \item{\code{outputSep}:}{An object of class \code{'character'}. Delimiter for the ouput table (tab is default).}
    \item{\code{plotOutput}:}{An object of class \code{'logical'}.  Determines whether the mass balance results shall be plotted (\code{TRUE}) or not (\code{FALSE}, default).}
    \item{\code{outputName}:}{An object of class \code{'character'}.  File name for the output \code{RasterLayer(s)} (default = "glacierSMBM_Output").}
    \item{\code{glacierMask}:}{An object of class \code{'RasterStack'}. Glacier area (1 = glacier, 0 = no glacier). Stationary or for every time step. Required.}
    \item{\code{iceMask}:}{An object of class \code{'RasterStack'}. Area of bare glacier ice (1 = bare ice, 0 = no bare ice). Stationary or for every time step. Required.}
    \item{\code{firnMask}:}{An object of class \code{'RasterStack'}. Area of supraglacial snow or firn (1 = snow or firn, 0 = no snow or firn). Stationary or for every time step. Required.}
    \item{\code{debrisMask}:}{An object of class \code{'RasterStack'}. Area of debris covered glacier ice (1 = debris, 0 = no debris). Stationary or for every time step. Required.}
    \item{\code{debrisThickness}:}{An object of class \code{'RasterStack'}. Distributed supraglacial debris thickness (m). Stationary or for every time step. Required.}
    \item{\code{airT}:}{An object of class \code{'RasterStack'}. Distributed air temperature (Kelvin or degree Celsius). For every time step. Required.}
    \item{\code{airDensity}:}{An object of class \code{'RasterStack'}. Distributed air density (kg m-3). Stationary or for every time step. Required.}
    \item{\code{netRad}:}{An object of class \code{'RasterStack'}. Distributed net radiation (W m-2). For every time step. Required.}
    \item{\code{inRadSW}:}{An object of class \code{'RasterStack'}. Distributed incoming shortwave radiation (W m-2). For every time step. Optional in the function \code{'debrisCoveredIceMelt'} instead of \code{'netRad'}.}    
    \item{\code{inRadLW}:}{An object of class \code{'RasterStack'}. Distributed incoming longwave radiation (W m-2). For every time step. Optional in the function \code{'debrisCoveredIceMelt'} instead of \code{'netRad'}.}
    \item{\code{snowHeight}:}{An object of class \code{'RasterStack'}. Initial snow and firn height (m).}
    \item{\code{snowfall}:}{An object of class \code{'RasterStack'}. Distributed snowfall (m). Required.}
    \item{\code{precip}:}{An object of class \code{'RasterStack'}. Distributed precipitation (m). Optional instead of \code{'snowfall'}.}
    \item{\code{tUnit}:}{An object of class \code{'character'}. Unit ("K" = Kelvin, "C" = degree Celsius) of air temperature (default = "K").}
    \item{\code{iceTMF}:}{An object of class \code{'numeric'}. Temperature melting factor (m K-1 timestep-1) of ice (default = 67*10^-4).}
    \item{\code{disIceTMF}:}{An object of class \code{'RasterStack'}. Distributed temperature melting factor (m K-1 timestep-1) of ice. Stationary or for every time step.}
    \item{\code{snowTMF}:}{An object of class \code{'numeric'}. Temperature melting factor (m K-1 timestep-1) of snow (default = 45*10^-4).}
    \item{\code{disSnowTMF}:}{An object of class \code{'RasterStack'}. Distributed temperature melting factor (m K-1 timestep-1) of snow. Stationary or for every time step.}
    \item{\code{iceRMF}:}{An object of class \code{'numeric'}. Radiative melting factor (m K-1 timestep-1) of ice (default = 0.79*10^-4).}
    \item{\code{disIceRMF}:}{An object of class \code{'RasterStack'}. Distributed radiative temperature melting factor (m K-1 timestep-1) of ice. Stationary or for every time step.}
    \item{\code{snowRMF}:}{An object of class \code{'numeric'}. Radiative melting factor (m K-1 timestep-1) of snow (default = 0.53*10^-4).}
    \item{\code{disSnowRMF}:}{An object of class \code{'RasterStack'}. Distributed radiative temperature melting factor (m K-1 timestep-1) of snow. Stationary or for every time step.}
    \item{\code{tuningFacAirT}:}{An object of class \code{'numeric'}. General air temperature tuning factor (<1 = temperature decrease, 1 = default, >1 = temperature increase).}
    \item{\code{disTuningFacAirT}:}{An object of class \code{'RasterStack'}. Distributed air temperature tuning factor (\code{tuningFacAirT}). Stationary or for every time step.}
    \item{\code{tuningFacPrecip}:}{An object of class \code{'numeric'}. General precipitation tuning factor (<1 = precipitation decrease, 1 = default, >1 = precipitation increase).}
    \item{\code{disTuningFacPrecip}:}{An object of class \code{'RasterStack'}. Distributed precipitation tuning factor (\code{tuningFacPrecip}). Stationary or for every time step.}
    \item{\code{snowTransTempThreshold}:}{An object of class \code{'numeric'}. Temperature threshold (same unit as \code{airT}) for the transition from rain- to snowfall (default = 274.15 Kelvin).}
    \item{\code{tmpRes}:}{An object of class \code{'character'}. Time aggregation (temporal resolution) of the input variables (default = "d"). "y" = year, "w" = week, "d" = day, "h" = hour, "s" = second.}
    \item{\code{measurementHeight}:}{An object of class \code{'numeric'}. Height (m) of meteorological measurements (default = 2).}
    \item{\code{relativeHumidity}:}{An object of class \code{'numeric'}. Relative humidity (0-1) at measurement height (default = 0.73).}
    \item{\code{disRelativeHumidity}:}{An object of class \code{'RasterStack'}. Distributed relative humidity (0-1) at measurement height. Stationary or for every time step.}
    \item{\code{windSpeed}:}{An object of class \code{'numeric'}. Wind speed (m s-1) at measurement height (default = 2).}
    \item{\code{disWindSpeed}:}{An object of class \code{'RasterStack'}. Distributed wind speed (m s-1) at measurement height. Stationary or for every time step.}
    \item{\code{debrisAlbedo}:}{An object of class \code{'numeric'}. Albedo (0-1) of the debris (default = 0.07).}
    \item{\code{disDebrisAlbedo}:}{An object of class \code{'RasterStack'}. Distributed albedo (0-1) of the debris. Stationary or for every time step.}
    \item{\code{thermalConductivity}:}{An object of class \code{'numeric'}. Thermal conductivity (W m-1 K-1) of the debris layer (default = 0.585).}
    \item{\code{disThermalConductivity}:}{An object of class \code{'RasterStack'}. Distributed thermal conductivity (W m-1 K-1) of the debris layer. Stationary or for every time step.}
    \item{\code{thermalEmissivity}:}{An object of class \code{'numeric'}. Thermal emissivity (0-1) of the debris layer (default = 0.95).}
    \item{\code{disThermalEmissivity}:}{An object of class \code{'RasterStack'}. Distributed thermal emissivity (0-1) of the debris layer. Stationary or for every time step.}
    \item{\code{surfaceRoughnessHeight}:}{An object of class \code{'numeric'}. Surface roughness height (m) of the debris layer (default = 0.01).}
    \item{\code{disSurfaceRoughnessHeight}:}{An object of class \code{'RasterStack'}. Distributed surface roughness height (m) of the debris layer. Stationary or for every time step.}
    \item{\code{frictionVelocity}:}{An object of class \code{'numeric'}. Friction velocity (m s-1) of the debris layer (default = 0.16).}
    \item{\code{disFrictionVelocity}:}{An object of class \code{'RasterStack'}. Distributed friction velocity (m s-1) of the debris layer. Stationary or for every time step.}
    \item{\code{volumeFractionDebrisInIce}:}{An object of class \code{'numeric'}. Volume fraction (0-1) of debris in the ice body (default = 0.01).}
    \item{\code{disVolumeFractionDebrisInIce}:}{An object of class \code{'RasterStack'}. Distributed volume fraction (0-1) of debris in the ice body. Stationary or for every time step.}
    \item{\code{debrisAirRatio}:}{An object of class \code{'numeric'}. Ratio of the debris surface area to the volume of air (m-1) in the debris layer (default = 188).}
    \item{\code{disDebrisAirRatio}:}{An object of class \code{'RasterStack'}. Distributed ratio of the debris surface area to the volume of air (m-1) in the debris layer. Stationary or for every time step.}
    \item{\code{dragCoefficient}:}{An object of class \code{'numeric'}. Drag coefficient (m-1) of the debris layer (default = 5).}
    \item{\code{disDragCoefficient}:}{An object of class \code{'RasterStack'}. Distributed drag coefficient (m-1) of the debris layer. Stationary or for every time step.}
    \item{\code{iceDensity}:}{An object of class \code{'numeric'}. Density (kg m-3) of ice (default = 900).}
    \item{\code{disIceDensity}:}{An object of class \code{'RasterStack'}. Distributed density (kg m-3) of ice. Stationary or for every time step.} 
  }
}
\section{Methods}{
  \describe{
    \item{glacierSMBM}{\code{signature(inputGlacierSMBM = "inputGlacierSMBM")}: ... }
	 }
}
\author{
Alexander R. Groos (\email{alexander.groos@giub.unibe.ch})
}

\note{
The following input variables are the requested minimum to run the model:
\itemize{
  \item \code{date}
  \item \code{airT} (for every time step)
  \item \code{netRad} (for every time step)
  \item \code{airDensity} (stationary or for every time step)
  \item \code{snowfall} or \code{precip} (for every time step)
  \item \code{glacierMask} (stationary or for every time step)
  \item \code{iceMask} (stationary or for every time step)
  \item \code{firnMask} (stationary or for every time step)
  \item \code{debrisMask} (stationary or for every time step)
  \item \code{debrisThickness} (stationary or for every time step)
}

Default settings for the individual slots can be viewed and checked by calls in the

form of \code{new("inputGlacierSMBM")}.
}
\seealso{
\code{\link{glacierSMBM}}
}


\examples{
showClass("inputGlacierSMBM")
}
\keyword{classes}
