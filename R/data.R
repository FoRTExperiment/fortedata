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
#'   \item{subplotID}{price, in US dollars}
#'   \item{species}{weight of the diamond, in carats}
#'   \item{index}{spectral index}
#'   \item{value}{value corresponding to the spectral index}
#' }
#'  @source \url{http://atkinsjeff.github.io}
"leaf_spectrometry"



