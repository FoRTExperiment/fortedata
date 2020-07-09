# fortedata: An open-data package for the R programming Language

![](https://user-images.githubusercontent.com/8354517/87047244-3da40200-c1c8-11ea-91fd-61104ad0f4f8.PNG)


### Overview

*fortedata* is an R package that provides functions for accessing and interpreting data associated with FoRTE: Forest Resilience Threshold Experimen--a modeling and manipulative field experiment that tests the effects of disturbance severity and disturbance type on carbon cycling dynamics in a temperate forest. Package data consists of measurements of carbon pools and fluxes and ancillary measurements to help analyse and users analyse and interpret carbon cycling over time.  


### Getting Started

We recommend that users begin with either package [vignettes](https://fortexperiment.github.io/fortedata/articles/index.html) that provide detailed examples of package funcitons and potential use cases, or with package [documentation](https://fortexperiment.github.io/fortedata/reference/index.html).  


### Installation

Installing the latest development version of *fortedata* from GitHub (requires [devtools](https://github.com/hadley/devtools)):

```r
if (!require("devtools")) {
  install.packages("devtools")
}
devtools::install_github("FoRTExperiment/fortedata", dependencies = TRUE, build_vignettes = FALSE)
```

### The FoRTE Team

* Principal Investigators:  [Chris M. Gough](http://www.people.vcu.edu/~cmgough/) and [Ben Bond-Lamberty](https://www.pnnl.gov/science/staff/staff_info.asp?staff_num=7203)

* PNNL Science Team: Kalyn Dorheim, Stephanie Pennington

* Postdoctoral Researchers: Jeff W. Atkins, Alexey Shiklomanov  

* Graduate Students:  Maxim S. Grigri, Lisa T. Haber, Kayla Mathes

* Undergraduate Students: Alexandra Barry, Laura J. Hickey, Catherine McGuigan, Autym Shafer

* REU Students: Evan Paris, Carly Rodriguez

* Reserach Collaborators: Elizabeth Agee, Kyla M. Dahlin, Aaron G. Kamoske,  Jason Tallant


