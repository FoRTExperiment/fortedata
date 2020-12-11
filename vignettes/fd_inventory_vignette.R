## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  require(fortedata),
  require(ggplot2),
  require(viridis),
  require(tidyverse)
)

## ----fd_inventory-------------------------------------------------------------
fd_inventory()

## ----biomass, fig.height = 3.5, fig.width = 7.5, fig.align = "center"---------
# import the biomass data
df <- fortedata::calc_biomass()

# return density plot of replicate biomass
ggplot2::ggplot(df, ggplot2::aes(x = biomass, fill = as.factor(replicate)))+
  ggplot2::geom_density(alpha = 0.4)+
  ggplot2::theme(legend.position = "none")+
  ggplot2::facet_grid(.~replicate)

