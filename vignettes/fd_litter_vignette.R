## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  require(fortedata),
  require(ggplot2),
  require(viridis)
)

## ----fd_litter, fig.height = 4, fig.width = 6, fig.align = "center"------
# import litter mass dataset
df <- fd_litter()

# we want to calculate the leaf mass by subtracting the tare weight `bagtare_g` from `bagmass_g`
df$leafmass_g <- df$bagmass_g - df$bagtare_g

# let's plot it
ggplot2::ggplot(df, ggplot2::aes(x = as.factor(replicate), y = leafmass_g, fill = as.factor(replicate) ))+
  ggplot2::geom_boxplot()+
  viridis::scale_color_viridis(discrete = TRUE, option = "D")+
  viridis::scale_fill_viridis(discrete = TRUE)+
  ggplot2::ylab("Leaf Mass [g]")+
  ggplot2::xlab("Replicate")+
  theme(legend.position = "NONE")

## ----lai, fig.height = 3.5, fig.width = 6, fig.align = "center"----------
# import lai values at the plot scale
df <- calc_lai()

# let's plot it
ggplot2::ggplot(df, ggplot2::aes(x = as.factor(replicate), y = lai, fill = as.factor(replicate) ))+
  ggplot2::geom_boxplot()+
  viridis::scale_color_viridis(discrete = TRUE, option = "D")+
  viridis::scale_fill_viridis(discrete = TRUE)+
  ggplot2::ylab("Leaf Area Index (LAI)")+
  ggplot2::xlab("Replicate")+
  theme(legend.position = "NONE")

