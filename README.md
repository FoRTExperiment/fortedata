# fortedata: An open-data package for the R programming Language

![](https://github.com/FoRTExperiment/fortedata/actions/workflows/test-coverage.yaml/badge.svg)
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
*  `fd_double_dendro` and `fd_dendro` functions have been added that provide data from dendrometer bands

* `fd_cwd` function added that includes coarse woody debris data from a 2019 survey of FoRTE plots

* `fd_ceptometer` now includes 2020 PAR data with an additional correction for some data based on calibration with the UMBS AmeriFlux sensor (see function documentation and additionally an in preperation manuscript, a preprint of which to be added upon submission)

* `fd_dendro` has been added to the package and includes dendrometer data from 2019 and 2020.

* `fd_leaf_spectrometry` now includes 2018, 2019, and 2020 data, as well as corrected leaf_id and tree_id data columns. Additionally, the original filename has been retained in the data set as id (2020-12-14)

* `fd_canopy_structure()` now includes 2020 lidar-derived canopy structural trait data.  (2020-12-09)

* `fd_soil_respiration()` now includes 2020 soil CO_2_ efflux, soil temperature, and soil water content data.  (2020-12-09)

**Future Data Availability**

* 3-D lidar structural data has been held up further by COVID-19. Access to computing resources to process said data have been greatly encumbered. Not likely complete until Summer 2021.

* Processing of 2019 and 2020 photosynthesis data (see `fd_photosynthesis()` for 2018) is underway and will hopefully be done by the end of 2020. 

* Additional future data to include are canopy dendroband data for 2019 and 2020, coarse woody debris surveys from 2019 and 2020, and small veg plot survey counts. Please email Jeff with any concerns or questions (jwatkins6vcu.edu) 

### The FoRTE Team

* Principal Investigators:  [Chris M. Gough](http://www.people.vcu.edu/~cmgough/) and [Ben Bond-Lamberty](https://www.pnnl.gov/science/staff/staff_info.asp?staff_num=7203)

* PNNL Science Team: Kalyn Dorheim, Stephanie Pennington

* Postdoctoral Researchers: Jeff W. Atkins, Alexey Shiklomanov  

* Graduate Students:  Maxim S. Grigri, Lisa T. Haber, Kayla Mathes

* Undergraduate Students: Alexandra Barry, Laura J. Hickey, Catherine McGuigan, Autym Shafer

* REU Students: Evan Paris, Carly Rodriguez

* Research Collaborators: Elizabeth Agee, Brandon Alveshare, Kyla M. Dahlin, Robert T. Fahey, Aaron G. Kamoske, Jason Tallant


