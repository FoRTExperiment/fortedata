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
      # Empty strings, and ONLY empty strings, should be interpreted as missing values.
      na.strings = "",
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
#' @return A `data.frame` or `tibble` holding data table metadata.
#' @export
fd_metadata <- function(table = NULL) {
  md <- read_csv_file("forte_table_metadata.csv")

  if (!is.null(table)) {
    md <- md[md$table == table,]
    if (nrow(md) < 1) {
      stop("Table ", table, " is not present in metadata.")
    }
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



# data conditions
# data.frame() if necessary
# not a user-facing function; document via roxygen?
data_conditions <- function(x, published = FALSE, contact_person, citation) {

  if(!published) {
    message("These data are unpublished. Please contact ", contact_person, " to ask about using")
  }

  message("Data citation: ", citation)
  message("Contact person: ", contact_person)

  # add the above information to `x` as attributes...

  invisible(x)
}



#' FoRTE color palette

#' The FoRTE color palette
#'
#' A FoRTE specific color palette used for color-classification of disturbance severity
#' treatments. 0% = "#000000", 45% = "#009E73", 65% = "#0072B2", and 85% = "#D55E00".
#'
#' Per the example below, `forte_colors()` can be used to define a custom palette that
#' can then be used for plotting with other commands such as `scale_fill_manual()`, etc.
#' @return A vector of colors.
#' @export
#' @keywords colors palette
#' @examples
#'
#'  forte.pal <- forte_colors()
#'
forte_colors <- function() {
  # list of disturbance severities
  disturbance_severity <- c(0, 45, 65, 85)

  # list of hex codes
  pal <-  c("#000000", "#009E73", "#0072B2", "#D55E00")

  # name it
  names(pal) <- disturbance_severity

  return(pal)
}



#' Calculate biomass from DBH data in `fd_inventory()`
#'
#' `calc_biomass()` uses biomass equations from `fd_table_biomass_allometries.csv`.
#' Parameters a and b and diameter at breast height (DBH, cm) range for the aboveground biomass
#' sans leaves equation Mass = a * DBH^b for tree species occurring in the UMBS-Flux footprint.
#' See `fd_inventory_vignette` for further information.
#'
#' Studies used to augment Cooper’s raw data for a species are denoted below.
#'
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
#'
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
#' @return A `data.frame` or `tibble` of tree biomass calculated
#' using allometries (listed in ?calc_biomass().
#' Call \code{\link{fd_metadata}} for field metadata.
#'
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

#' Metadata for the experimental field plots
#'
#' These data include `latitude` and `longitude` of each plot as well as eperimental assignments inluding `disturbance_severity` and `treatment`.
#'
#' @return A A `data.frame` or `tibble` with the experimental treatments by UMBS plot.
#' @export
#' @examples
#' fd_plot_metadata()
fd_plot_metadata <- function(){
  dat <- read_csv_file("forte_plot_metadata.csv")

  weak_as_tibble(dat)
}


#' LAI values at the subplot scale from littertrap data
#'
#' LAI is calculated using the equation SLA * mass, where SLA is specific leaf area and
#' mass is leaf litter mass from `fd_litter()`. Species and site specific values are
#' in `fd_sla.csv`. Also, see `fd_litter_vignette` for further explanation,
#' including references, calculation, and possible use cases.
#'
#'
#' @return A data frame of leaf area by year, at the subplot scale.
#' @export
#' @examples
#' calc_lai()
calc_lai <- function() {

  # litter mass
  leafs <- fd_litter()

  # subsetting leaves
  leaf <- subset(leafs, leafs$fraction == "leaf")

  # specific leaf area
  sla <- read_csv_file("fd_sla.csv") # this has the same equations AmeriFlux uses

  # calculate mass of leaves only
  leaf$leafmass_g <- leaf$bagmass_g - leaf$bagtare_g

  # Add SLA to the leaf tibble and calculate leaf area totals
  leaf <- merge(leaf, sla)
  leaf$leaf_area <- leaf$leafmass_g * leaf$sla

  # aggregate by species
  lai <- stats::aggregate(leaf_area ~ subplot_id + year, data = leaf, FUN = sum)
  plot_area <- 1000  # m^2 (= 0.1 ha)
  lai$lai <- lai$leaf_area / plot_area

  lai <- split_subplot_id(lai)

  # reorder columns
  lai <- lai[c("subplot_id", "replicate", "plot", "subplot", "year", "lai")]

  weak_as_tibble(lai)
}
