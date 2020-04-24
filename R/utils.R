# Utilities

#' Read an internal package CSV file.
#'
#' @param ... Filename, character
#'
#' @return The read-in dataframe.
#' @keywords internal
#' @importFrom utils read.csv
#' @details \code{...} is passed to \code{\link{system.file}},
#' so can be vectors specifying
#' subdirectory and file(s) within the package.
#' @note This is an internal function and not intended to be called directly.
read_csv_file <- function(...) {
  weak_as_tibble(
    read.csv(
      system.file("extdata", ..., package = "fortedata", mustWork = TRUE),
      stringsAsFactors = FALSE
    )
  )
}


# weak_tibble - use tibble() if available but fall back to
# data.frame() if necessary
# not a user-facing function; document via roxygen?
weak_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  out <- data.frame(..., stringsAsFactors = FALSE)
  if (!(.force_df || no_tibble)) out <- weak_as_tibble(out)
  out
}

# weak_as_tibble - use as_tibble() if available but fall back to
# as.data.frame() if necessary
weak_as_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  if (.force_df || no_tibble) {
    as.data.frame(..., stringsAsFactors = FALSE)
  } else {
    tibble::as_tibble(...)
  }
}


#' Data tables' metadata.
#'
#' @param table Name of table to return, character, optional
#' @return A `data.frame` or `tibble` holding field metadata.
#' @export
fd_metadata <- function(table = NULL) {
  md <- read_csv_file("forte_table_metadata.csv")

  if(!is.null(table) && table %in% md$Table) {
    md <- md[md$Table == table,]
  }
  weak_as_tibble(md)
}


#' Split the SubplotID column into more useful individual columns
#'
#' @param df A data.frame with a four-character \code{subplot_id} column
#'
#' @return The data frame with new columns \code{replicate}, \code{plot},
#' and \code{subplot}.
#' @keywords internal
#' @examples
#' fortedata:::split_subplot_id(data.frame(subplot_id = "A01E"))
split_subplot_id <- function(df) {
  stopifnot("subplot_id" %in% names(df))
  df$replicate <- substr(df$subplot_id, 1, 1)
  df$plot <- as.integer(substr(df$subplot_id, 3, 3))
  df$subplot <- substr(df$subplot_id, 4, 4)
  df
}

#' FoRTE color palette

#' The FoRTE color palettte
#'
#' These are a handful of color palettes pulled from photographs of US National Parks.
#' @return A vector of colours.
#' @export
#' @keywords colors
#' @examples
#'
#' pal <- forte_colors()
forte_colors <- function() {
  pal <- c("#000000", "#009E73", "#0072B2", "#D55E00")
  pal <- structure(pal, class = "palette", name = 'forte')
}



#' @export
#' @importFrom graphics rect par image text
#' @importFrom grDevices rgb
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

#' Calculate biomass from DBH data in fd_inventory() using equations from
#' # Table  . Parameters a and b and diameter at breast height (DBH, cm) range for the aboveground biomass
#' sans leaves equation Mass = a * DBH^b for tree species occurring in the UMBS-Flux footprint.
#'
#' Studies used to augment Cooper’s raw data for a species are denoted by superscripts.
#' BIOMASS BACKGROUND INFO
#' Species	a	b	DBH range (cm)
#' Acer rubrum a	0.0312	2.7780	1 – 40
#' Acer saccharum b	0.1693	2.3436	3 – 66
#' Amelanchier spp.c	0.1630	2.4940	0.3-2.7h
#' Betula papyrifera d	0.0301	2.8387	2 – 49
#' Fagus grandifolia b	0.1892	2.3097	3 – 66
#' Pinus resinosa e	0.0526	2.5258	2 – 32
#' Pinus strobus e	0.0408	2.5735	1 – 32
#' Populus grandidentata f	0.1387	2.3498	1 – 37
#' Populus tremuloides b	0.0589	2.6235	3 – 51
#' Quercus rubra g	0.0398	2.7734	1 – 44
#' a Crow and Erdmann, 1983, Perala and Alban, 1994, Young et al., 1980.
#' b Young et al., 1980 equations used exclusively, DBH range estimated by Ker-Mikaelian and Korzukhin, 1997.
#' c Perala and Alban, 1994 equations used exclusively.
#' d Perala and Alban, 1994, Schmitt and Grigal, 1981, Young et al., 1980.
#' e Ker, 1980, Perala and Alban, 1994, Young et al., 1980.
#' f Koerper, 1977  raw data collected near UMBS-Flux tower.
#' g Hocker and Early, 1983, Perala and Alban, 1994, Wiant et al., 1977.
#' h Diameter at 15 cm height.
#'
#'
#' @return A data frame of biomass at the individual level build from fd_inventory
#' @export
#' @examples
#' fortedata::calc_biomass()

calc_biomass <- function(){
# The allometries
allo.df <- read_csv_file("fd_biomass_allometries.csv") #this has the same equations AmeriFlux uses
df <- fortedata::fd_inventory()


# merge the two
stem <- merge(df, allo.df)

#calculates biomass in units of kg
stem$biomass <- stem$a_biomass * stem$dbh^stem$b_biomass
stem <- weak_as_tibble(stem)
}


#' Function that returns LAI values at the plot scale
#' #'
#' @return A data frame of leaf area fro SLA * mass at the plot scale
#' @export
#' @examples
#' fortedata::calc_lai()
calc_lai <- function() {

  # importlitter mass
  leaf <- fd_litter()

  # bring in the specific leaf area
  sla <- read_csv_file("fd_sla.csv") #this has the same equations AmeriFlux uses

  # calculate mass of leaves only
  leaf$leafmass_g <- leaf$bagmass_g - leaf$bagtare_g

  # Add SLA to the leaf tibble
  leaf <- merge(leaf, sla)

  # calculate leaf area totals
  leaf$leaf_area <- leaf$leafmass_g * leaf$sla

  # make plot lai by species
  lai <- stats::aggregate(leaf_area ~ subplot_id + year, data = leaf, FUN = sum)


  # adds in plot area
  plot_area <- 1000  #plot area in m^2 (is 0.1 ha)

  # calculates LAI
  lai$lai <- lai$leaf_area / plot_area

  lai <- split_subplot_id(lai)
  # reorders columns
  lai <- lai[c("subplot_id", "replicate", "plot", "subplot", "year", "lai")]

  lai <- weak_as_tibble(lai)
}
