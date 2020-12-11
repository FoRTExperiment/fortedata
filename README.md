# fortedata: An open-data package for the R programming Language

![](https://user-images.githubusercontent.com/8354517/87047244-3da40200-c1c8-11ea-91fd-61104ad0f4f8.PNG)


### Overview

*fortedata* is an R package that provides functions for accessing and interpreting data associated with FoRTE: Forest Resilience Threshold Experiment--a modeling and manipulative field experiment that tests the effects of disturbance severity and disturbance type on carbon cycling dynamics in a temperate forest. Package data consists of measurements of carbon pools and fluxes and ancillary measurements to help users analyze and interpret carbon cycling over time.  


### Getting Started

We recommend that users begin with either package [vignettes](https://fortexperiment.github.io/fortedata/articles/index.html) that provide detailed examples of package functions and potential use cases, or with package [documentation](https://fortexperiment.github.io/fortedata/reference/index.html).  


### Installation

Installing the latest development version of *fortedata* from GitHub (requires [devtools](https://github.com/hadley/devtools)):

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("FoRTExperiment/fortedata", dependencies = TRUE, build_vignettes = FALSE)
```


### `fortedata` 1.0.2 Updates and Future Data Availability

**Updates**

* `fd_canopy_structure()` now includes 2020 lidar-derived canopy structural trait data.

* `fd_soil_respiration()` now includes 2020 soil CO_2_ efflux, soil temperature, and soil water content data.

**Future Data Availability**

* Leaf spectral data (see `fd_leaf_spectrometry`) for 2019 and 2020 are on deck. Currently, there are issues with matching tree identification field codes to the data set.

* Light interception data (see `fd_ceptometer()`) for 2020 is being calibrated with the adjacent UMBS AmeriFlux tower PAR data (US-UMb tower ID). 

* 3-D lidar structural data has been held up further by Covid-19. Access to computing resrouces to process said data have been greatly encumbered.

### The FoRTE Team

* Principal Investigators:  [Chris M. Gough](http://www.people.vcu.edu/~cmgough/) and [Ben Bond-Lamberty](https://www.pnnl.gov/science/staff/staff_info.asp?staff_num=7203)

* PNNL Science Team: Kalyn Dorheim, Stephanie Pennington

* Postdoctoral Researchers: Jeff W. Atkins, Alexey Shiklomanov  

* Graduate Students:  Maxim S. Grigri, Lisa T. Haber, Kayla Mathes

* Undergraduate Students: Alexandra Barry, Laura J. Hickey, Catherine McGuigan, Autym Shafer

* REU Students: Evan Paris, Carly Rodriguez

* Research Collaborators: Elizabeth Agee, Brandon Alveshare, Kyla M. Dahlin, Robert T. Fahey, Aaron G. Kamoske, Jason Tallant


