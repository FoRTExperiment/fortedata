# Remote Sensing data


#' Hemispherical camera data
#'
#' @details The columns are as follows:
#'
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `NestedSubPlot` (integer):  NestedSubplotSampling points but need to check other data
#' - `Date` (date): Date of measurement
#' - `Year` (integer): Year of
#' - `NDVI` (numeric): Normalized Difference Vegetation Index, estimates greenness.
#' - `GapFraction` (numeric): Ratio of gap space in the canopy, or open area.
#' - `Openness` (numeric): something something
#' - `LAI_cam` (numeric): leaf area index
#' - `ClumpingIndex` (numeric): Clumping index
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_hemi_camera()
fd_hemi_camera <- function() {
  cam <- read_csv_file("fd_hemi_camera.csv")

  # formatting columns
  cam$Date <- as.Date(cam$date, format = "%m/%d/%Y")
  names(cam)[names(cam) == "nsp"] <- "NestedPlot"
  names(cam)[names(cam) == "ndvi"] <- "NDVI"
  names(cam)[names(cam) == "gf"] <- "GapFraction"
  names(cam)[names(cam) == "open"] <- "Openness"
  names(cam)[names(cam) == "lai"] <- "LAI_cam"
  names(cam)[names(cam) == "ci"] <- "ClumpingIndex"
  names(cam)[names(cam) == "year"] <- "Year"

  # Split the SubplotID column into more useful individual columns
  cam$Replicate <- substr(cam$SubplotID, 1, 1)
  cam$Plot <- as.integer(substr(cam$SubplotID, 2, 3))
  cam$Subplot <- substr(cam$SubplotID, 4, 4)

  # filters to just FoRTE data
  cam <- subset(cam, cam$project == "forte")

  # replaces NSP data with numbers
  cam$NestedPlot[cam$NestedPlot == "C"] <- 0
  cam$NestedPlot[cam$NestedPlot == "N"] <- 1
  cam$NestedPlot[cam$NestedPlot == "E"] <- 3
  cam$NestedPlot[cam$NestedPlot == "S"] <- 5
  cam$NestedPlot[cam$NestedPlot == "W"] <- 7
  cam$NestedPlot[cam$NestedPlot == "X"] <- NA
  cam$NestedPlot[cam$NestedPlot == ""] <- NA

  cam$NestedPlot <- as.integer(cam$NestedPlot)
  cam <- na.omit(cam) # this removes the images that were retained as placemarkers if there are any that missed being culled

    # Reorder columns
 cam[c("SubplotID", "Replicate", "Plot", "Subplot", "NestedPlot", "Date", "Year", "NDVI", "GapFraction", "Openness", "LAI_cam", "ClumpingIndex")]
}

#' Summary data for hemispherical camera data
#'
#' @details The columns are as follows:
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Date` (date): Date of measurement
#' - `Year` (integer): Year of
#' - `LAI_cam` (numeric): mean of leaf area index
#' - `LAI_cam_sd` (numeric): sd of leaf area index
#' - `LAI_cam_n` (integer): number of observations per sample
#' - `LAI_cam_se` (numeric): se of leaf area index
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @note For now this is pretty basic. More detailed summaries could be made,
#' e.g. by live/dead, species, etc.
#' @export
#' @importFrom stats aggregate sd na.omit
#' @examples
#' fd_hemi_camera_summary()
fd_hemi_camera_summary <- function() {
  # Load the inventory and subplot tables and merge them
  # vsubplots <- fd_hemi_camera()[c("SubplotID", "Replicate", "Plot", "Subplot", "NestedPlot")]
  df <- fd_hemi_camera()

  # Calculate rugosity means and SD
  lai <- aggregate(LAI_cam ~ Replicate + Plot + Subplot + Date , data = df, FUN = mean)
  lai.sd <- aggregate(LAI_cam ~ Replicate + Plot + Subplot + Date, data = df, FUN = sd)
  lai.n <- aggregate(LAI_cam ~ Replicate + Plot + Subplot + Date, data = df, FUN = length)

  # Merge and munge
  names(lai.sd)[names(lai.sd) == "LAI_cam"] <- "LAI_cam_sd"
  names(lai.n)[names(lai.n) == "LAI_cam"] <- "LAI_cam_n"

  lai <- merge(lai, lai.sd)
  lai <- merge(lai, lai.n)
  lai$lai_cam_se <- lai$LAI_cam_sd / sqrt(lai$LAI_cam_n)  # based on the SD /sqrt(n)

  # VAI means and SD
  ndvi <- aggregate(NDVI ~ Replicate , data = df, FUN = mean)
  ndvi.sd <- aggregate(NDVI ~ Replicate , data = df, FUN = sd)

  # Merge and munge
  names(ndvi.sd)[names(ndvi.sd) == "NDVI"] <- "NDVI_sd"
  ndvi <- merge(ndvi, ndvi.sd)
  ndvi$NDVI_se <- ndvi$NDVI_sd / sqrt(8)

  weak_as_tibble(merge(lai, ndvi))
}

#' Canopy Structural Traits from 2D Canopy LiDAR
#'
#' Canopy strucutral Traits derived using the forestr 1.0.1 package from
#' 2D portable canopy lidar
#' Return hemispherical camera data
#'
#' @details The columns are as follows:
#'
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Year` (integer): Year in which measurement was taken
#' - `mean.height` (numeric): mean height of vai
#' - `height.2` (numeric): standard deviation of mean height
#' - `mean.height.var` (numeric): variance of mean height
#' - `mean.height.rms` (numeric): root mean square height
#' - `transect.length` (numeric): length of transect
#' - `mode.el` (numeric): i forgot what this is
#' - `max.el` (numeric): greatest density of VAI x, z position
#' - `mode.2` (numeric): variance of maximum VAI
#' - `max.can.ht` (numeric): maximum measured canopy height
#' - `mean.max.ht` (numeric): mean outer canopy height or MOCH
#' - `mean.vai` (numeric): average VAI across transect
#' - `mean.peak.vai` (numeric): average height of maximum VAI
#' - `deep.gaps` (numeric): number of 1 m wide bins with no lidar returns
#' - `deep.gap.fraction` (numeric): deep gaps dividied by transect length
#' - `porosity` (numeric): ratio of empty to filled bins in the canopy
#' - `std.std` (numeric): precursor to rugosity
#' - `mean.std` (numeric): precursor to rugosity
#' - `rugosity` (numeric): accumulated canopy complexity metric
#' - `top.rugosity` (numeric): standard deviation of final lidar returns
#' - `mean.return.ht` (numeric): average lidar return distance
#' - `sd.return.ht` (numeric): standard deviation of lidar return distances
#' - `sky.fraction` (numeric): ratio of sky hits to lidar returns
#' - `cover.fraction` (numeric): 1/sky fraction
#' - `max.ht` (numeric): same as max can ht, removed in later forestr updates
#' - `scan.density` (numeric): no. of lidar returns divided by transect length
#' - `rumple` (numeric): outer surface variability divided by transect length
#' - `clumping.index` (numeric): clumpiness
#' - `enl` (numeric): effective number of layers
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#'
#' @examples
#' fd_canopy_structure()
fd_canopy_structure <- function() {
  cst <- read_csv_file("canopy_structural_traits.csv")

  # Rename columns that need it
  names(cst)[names(cst) == "subplotID"] <- "SubplotID"
  names(cst)[names(cst) == "year"] <- "Year"

  # Split the SubplotID column into more useful individual columns
  cst$Replicate <- substr(cst$SubplotID, 1, 1)
  cst$Plot <- as.integer(substr(cst$SubplotID, 2, 3))
  cst$Subplot <- substr(cst$SubplotID, 4, 4)

  # Reorder columns
  cst[c(1, 31, 32, 33, 2, 3:30 )]
}

#' Summary data for hemispherical camera data
#'
#' @details The columns are as follows:
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Date` (date): Date of measurement
#' - `Year` (integer): Year of
#' - `rugosity` (numeric): mean of leaf area index
#' - `rugosity_sd` (numeric): sd of leaf area index
#' - `rugosity_n` (integer): number of observations per sample
#' - `rugosity_se` (numeric): se of leaf area index
#' - `mean.vai` (numeric): mean of leaf area index
#' - `mean.vai_sd` (numeric): sd of leaf area index
#' - `mean.vai_n` (integer): number of observations per sample
#' - `mean.vai_se` (numeric): se of leaf area index
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @note For now this is pretty basic.
#' @export
#' @importFrom stats aggregate sd na.omit
#' @examples
#' fd_canopy_structure_summary()
fd_canopy_structure_summary <- function() {
  # Load the inventory and subplot tables and merge them
  subplots <- fd_canopy_structure()[c("SubplotID", "Replicate", "Plot", "Subplot")]
  csc <- merge(fd_canopy_structure(), subplots)

  # Calculate rugosity means and SD
  r_c <- aggregate(rugosity ~ Replicate , data = csc, FUN = mean)
  r_c.sd <- aggregate(rugosity ~ Replicate , data = csc, FUN = sd)
  r_c.n <- aggregate(rugosity ~ Replicate, data = csc, FUN = length)

  # Merge and munge
  names(r_c.sd)[names(r_c.sd) == "rugosity"] <- "rugosity_sd"
  names(r_c.n)[names(r_c.n) == "rugosity"] <- "rugosity_n"

  r_c <- merge(r_c, r_c.sd)
  r_c <- merge(r_c, r_c.n)

  r_c$rugosity_se <- r_c$rugosity_sd / sqrt(r_c$rugosity_n)  # based on the SD /sqrt(n)

  # VAI means and SD
  vai<- aggregate(mean.vai ~ Replicate , data = csc, FUN = mean)
  vai.sd <- aggregate(mean.vai ~ Replicate , data = csc, FUN = sd)
  vai.n <- aggregate(mean.vai ~ Replicate, data = csc, FUN = length)

  # Merge and munge
  names(vai.sd)[names(vai.sd) == "mean.vai"] <- "mean.vai_sd"
  names(vai.n)[names(vai.n) == 'mean.vai'] <- "mean.vai_n"

  vai <- merge(vai, vai.sd)
  vai <- merge(vai, vai.n)

  vai$mean.vai_se <- vai$mean.vai_sd / sqrt(vai$mean.vai_n)

  weak_as_tibble(merge(r_c, vai))
}

#' Ceptometer data
#'
#' @details The columns are as follows:
#'
#' - `SubplotID` (character): Subplot ID number. These subplot codes are a
#' concatenation of the plot (\code{\link{fd_plots}}) and
#' subplot \code{\link{fd_subplots}} codes.
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Year` (integer): Year of mesmt
#' - `DateTime` (asPOSIXlt): Date of measurment
#' - `aPAR` (numeric): above canopy PAR (photosynthetically available radiation)
#' - `bPAR` (numeric): below canopy PAR (photosynthetically available radiation)
#' - `faPAR` (numeric): fraction of PAR absorbed by the canopy
#' - `LAI_cept` (numeric): leaf area index derived from ceptometer
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @export
#' @examples
#' fd_par()
fd_par <- function() {
  par <- read_csv_file("fd_ceptometer.csv")

  # Rename that weird column
  colnames(par)[1] <- "project"

  # Rename columns
  names(par)[names(par) == "Average.Above.PAR"] <- "aPAR"
  names(par)[names(par) == "Average.Below.PAR"] <- "bPAR"
  names(par)[names(par) == "Leaf.Area.Index..LAI."] <- "LAI_cept"

  # Make DateTime column as datetime object
  par$DateTime <- as.POSIXlt(par$DateTime, format = "%m/%d/%Y %H:%M")

  # Filter to just FoRTE data
  par <- subset(par, par$project == "forte")

  # Remove erroneous entires
  par$Annotation <- gsub("2019", "", par$Annotation)
  par$Year  <- as.integer(par$DateTime$year+1900)

  # Create the SubPlotID column now that it's clean
  par$SubplotID <- par$Annotation

  # Split the SubplotID column into more useful individual columns
  par$Replicate <- substr(par$SubplotID, 1, 1)
  par$Plot <- as.integer(substr(par$SubplotID, 2, 3))
  par$Subplot <- substr(par$SubplotID, 4, 4)

  # faPAR
  par$faPAR <- par$bPAR / par$aPAR

  # Reorder columns
  par[c("SubplotID", "Replicate", "Plot", "Subplot", "Year", "DateTime", "aPAR", "bPAR", "faPAR", "LAI_cept")]
}


#' Return summary data for ceptometer
#'
#' @details The columns are as follows:
#' - `Replicate` (character): Replicate code, extracted from `SubplotID`.
#' - `Plot` (integer): Plot ID number, extracted from `SubplotID`.
#' - `Subplot` (character): Subplot code, extracted from `SubplotID`.
#' - `Date` (date): Date of measurement
#' - `Year` (integer): Year of measurement
#' - `faPAR` (numeric): mean of the fraction of absorbed PAR
#' - `faPAR_sd` (numeric): sd of the fraction of absorbed PAR
#' - `faPAR_n` (integer): number of observations per sample
#' - `faPAR_se` (numeric): se of the fraction of absorbed PAR
#' - `LAI_cept` (numeric): mean of leaf area index
#' - `LAI_cept_sd` (numeric): sd of leaf area index
#' - `LAI_cept_n` (integer): number of observations per sample
#' - `LAI_cept_se` (numeric): se of leaf area index
#'
#' @return A `data.frame` or `tibble`. See "Details" for column descriptions.
#' @note For now this is pretty basic.
#' @export
#' @importFrom stats aggregate sd na.omit
#' @examples
#' fd_par_summary()
fd_par_summary <- function() {
  # Load the inventory and subplot tables and merge them
  # subplots <- fd_par()[c("SubplotID", "Replicate", "Plot", "Subplot")]
  df <- fd_par()

  # calculate rugosity means and SD
  f <- aggregate(faPAR ~ Replicate , data = df, FUN = mean)
  f.sd <- aggregate(faPAR ~ Replicate , data = df, FUN = sd)
  f.n <- aggregate(faPAR ~ Replicate, data = df, FUN = length)

  # Merge and munge
  names(f.sd)[names(f.sd) == "faPAR"] <- "faPAR_sd"
  names(f.n)[names(f.n) == "faPAR"] <- "faPAR_n"

  f <- merge(f, f.sd)
  f <- merge(f, f.n)

  f$faPAR_se <- f$faPAR_sd / sqrt(f$faPAR_n)  # based on the SD /sqrt(n)

  # VAI  means and SD
  lai <- aggregate(LAI_cept ~ Replicate , data = df, FUN = mean)
  lai.sd <- aggregate(LAI_cept ~ Replicate , data = df, FUN = sd)
  lai.n <- aggregate(LAI_cept ~ Replicate, data = df, FUN = length)

  # Merge and munge
  names(lai.sd)[names(lai.sd) == "LAI_cept"] <- "LAI_cept_sd"
  names(lai.n)[names(lai.n) == "LAI_cept"] <- "LAI_cept_n"

  lai <- merge(lai, lai.sd)
  lai <- merge(lai, lai.n)

  lai$LAI_cept_se <- lai$LAI_cept_sd / sqrt(lai$LAI_cept_n)

  weak_as_tibble(merge(f, lai))

}
