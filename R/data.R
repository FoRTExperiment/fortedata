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

#' Soil CO2 Efflux Measurements and Associated Micrometeorological Measurements
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
#' @format A data frame with four variables: \code{dateTime} \code{subplotID}, \code{run},\code{nestedPlot},
#'   \code{soilCO2Efflux}, \code{soilTemp}, \code{VWC} and \code{notes}
#'
#' \describe{
#'   \item{dateTime}{date and time of sample in YYYY-MM-DD HH:MM:SS}
#'   \item{subplotID}{subplot identifier}
#'   \item{run}{differnt sampling occurence}
#'   \item{nestedPlot}{sampling point of collar inside of subplot}
#'   \item{soilCO2Efflux}{soil carbon dioxide efflux in umol CO2 m^2 s^-1}
#'   \item{soilTemp}{soil temperature at 7 cm depth in celsius}
#'   \item{VWC}{volumetric water content from 0 to 20 cm depth}
#'   \item{notes}{additional information}
#' }
#'  @source \url{http://atkinsjeff.github.io}
"soil_efflux"

#' Canopy Structural Traits from 2D Canopy LiDAR
#'
#' Canopy strucutral Traits derived using the forestr 1.0.1 package from
#' 2D portable canopy lidar
#'
#' @docType data
#'
#' @usage data(canopy_structural_traits)
#'
#' @format A data frame with lots of variables:  \code{subplotID}, \code{year}, \code{mean.height},
#'   \code{height.2}, \code{mean.height.var}, \code{mean.height.rms}, \code{transect.length}, \code{mode.el},
#'   \code{max.el}, \code{mode.2}, \code{max.can.ht}, \code{mean.max.ht}, \code{mean.vai},
#'   \code{mean.peak.vai}, \code{deep.gaps}, \code{deep.gap.fraction}, \code{porosity},
#'   \code{std.std}, \code{mean.std}, \code{rugosity}, \code{top.rugosity}, \code{mean.return.ht},
#'   \code{sd.return.ht}, \code{sky.fraction}, \code{cover.fraction}, \code{max.ht}, \code{scan.density},
#'   \code{rumple}, \code{clumping.index}, and \code{enl}
#'
#' \describe{
#'   \item{subplotID}{subplot identifier}
#'   \item{year}{msmt year}
#'   \item{mean.height}{mean height of vai}
#'   \item{height.2}{standard deviation of mean height}
#'   \item{mean.height.var}{variance of mean height}
#'   \item{mean.height.rms}{variance of mean height}
#'   \item{transect.length}{length of transect}
#'   \item{mode.el}{i forgot what this is}
#'   \item{max.el}{greatest density of VAI x, z position}
#'   \item{mode.2}{variance of maximum VAI}
#'   \item{max.can.ht}{maximum measured canopy height}
#'   \item{mean.max.ht}{mean outer canopy height or MOCH}
#'   \item{mean.vai}{average VAI across transect}
#'   \item{mean.peak.vai}{average height of maximum VAI}
#'   \item{deep.gaps}{number of 1 m wide bins with no lidar returns}
#'   \item{deep.gap.fraction}{deep gaps dividied by transect length}
#'   \item{porosity}{ratio of empty to filled bins in the canopy}
#'   \item{std.std}{precursor to rugosity}
#'   \item{mean.std}{precursor to rugosity}
#'   \item{rugosity}{accumulated canopy complexity metric}
#'   \item{top.rugosity}{standard deviation of final lidar returns}
#'   \item{mean.return.ht}{average lidar return distance}
#'   \item{sd.return.ht}{standard deviation of lidar return distances}
#'   \item{sky.fraction}{ratio of sky hits to lidar returns}
#'   \item{cover.fraction}{1/sky fraction}
#'   \item{max.ht}{same as max can ht, removed in later forestr updates}
#'   \item{scan.density}{no. of lidar returns divided by transect length}
#'   \item{rumple}{outer surface variability divided by transect lenght}
#'   \item{clumping.index}{clumpiness}
#'   \item{enl}{effective number of layers}
#' }
#'  @source \url{http://atkinsjeff.github.io}
"canopy_structural_traits"

