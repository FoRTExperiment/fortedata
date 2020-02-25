#' Leaf-level spectral indices.
#'
#' Canopy leaf spectra collected with a CI-710
#' Miniature Leaf SpectrometerARI1	 #FF0000	(1/W550) - (1/W700)	 Leaf Pigments	 The value of this index ranges from 0 to more than 0.2. The common range for green vegetation is 0.001 to 0.1.
#' subplotID	species	index	value
#'
#' @docType data
#'
#' @usage data(leaf_spectrometry)
#'
#' @format A data frame with four variables: \code{subplotID}, \code{species},
#'   \code{index}, and \code{value}
#'
#' \describe{
#'   \item{subplotID}{subplot identifier}
#'   \item{species}{species of leaf analyzed}
#'   \item{index}{spectral index}
#'   \item{value}{value corresponding to the spectral index}
#' }
#'  @source \url{http://atkinsjeff.github.io}
"leaf_spectrometry"

#' Soil CO2 Efflux measuremnts and Associated Micrometeorological Measurements
#'
#' Soil CO efflux measurements collected with a Li-Cor 8100A
#' Soil temperature measurements were made at 7 cm depth
#' soil moisture as volumetric water content msmts were wmade with a Campbell CS620
#' time domain reflectometer (TDR) at 0 - 20 cm depth.
#' dateTime	subplotID	nestedPlot	run	soilCO2Efflux	soilTemp	VWC	notes

#' @docType data
#'
#' @usage data(soil_efflux)
#'
#' @format A data frame with four variables: \code{dateTime} \code{subplotID}, \code{nestedPlot},
#'   \code{run},\code{soilCO2Efflux}, \code{soilTemp}, \code{VWC} and \code{notes}
#'
#' \describe{
#'   \item{dateTime}{date and time of sample in YYYY-MM-DD HH:MM:SS}
#'   \item{subplotID}{subplot identifier}
#'   \item{nestedPlot}{sampling point of collar inside of subplot}
#'   \item{soilCO2Efflux}{soil carbon dioxide efflux in umol CO2 m^2 s^-1}
#'   \item{soilTemp}{soil temperature at 7 cm depth in celsius}
#'   \item{VWC}{volumetric water content from 0 to 20 cm depth}
#'   \item{notes}{additional information}
#' }
#'  @source \url{http://atkinsjeff.github.io}
"soil_efflux"


