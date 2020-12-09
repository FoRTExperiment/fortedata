<<<<<<< HEAD
## ----setup, include = FALSE---------------------------------------------------
=======
## ----setup, include = FALSE----------------------------------------------
>>>>>>> cb9893702271eb3ec6e45781def98757a5820179
knitr::opts_chunk$set(
  collapse = TRUE,
  require(fortedata),
  require(ggplot2),
  require(viridis),
  require(tidyverse)
)


<<<<<<< HEAD
## ----disturb1-----------------------------------------------------------------

# bring in metadata via the plot_metadata() function
df <- fortedata::plot_metadata()
=======
## ----disturb1------------------------------------------------------------

# bring in metadata via the plot_metadata() function
df <- fortedata::fd_plot_metadata()
>>>>>>> cb9893702271eb3ec6e45781def98757a5820179

# now we converte the tibble to a data frame
df <- data.frame(df)

<<<<<<< HEAD
## ----disturb-2----------------------------------------------------------------
# let's look at the structure of the data
str(df)

## ----disturb-3----------------------------------------------------------------
=======
## ----disturb-2-----------------------------------------------------------
# let's look at the structure of the data
str(df)

## ----disturb-3-----------------------------------------------------------
>>>>>>> cb9893702271eb3ec6e45781def98757a5820179
# First we want to concatenate our replicate, plot and subplot data to make a subplot_id column 
df$subplot_id <- paste(df$replicate, 0, df$plot, df$subplot, sep = "")
df$subplot_id <- as.factor(df$subplot_id)

# Now that we have our data in the form for this analysis, let's filter our metadata to the subplot level.
df %>%
  select(subplot_id, disturbance_severity, treatment) %>%
  distinct() %>%
  data.frame() -> dis.meta.data

# this filters the metadata down to the subplot_id level
dis.meta.data <- dis.meta.data[c(1:32), ]

print(dis.meta.data)

<<<<<<< HEAD
## ----disturb-4----------------------------------------------------------------
=======
## ----disturb-4-----------------------------------------------------------
>>>>>>> cb9893702271eb3ec6e45781def98757a5820179
# First we import the camera data
cam <- fd_hemi_camera()

# Then we merge with the metadata from above
cam <- merge(cam, dis.meta.data)

# For this analysis we want to code both disturbance severity and treatment as factors
cam$disturbance_severity <- as.factor(cam$disturbance_severity)
cam$treatment <- as.factor(cam$treatment)

<<<<<<< HEAD
## ----disturb-5, fig.align = "center", fig.width = 6, fig.height = 4-----------
=======
## ----disturb-5, fig.align = "center", fig.width = 6, fig.height = 4------
>>>>>>> cb9893702271eb3ec6e45781def98757a5820179
# making year column
cam$year <- format(as.Date(cam$date), "%Y")
# let's look at distribution by year, by disturbance severity and treatment
ggplot(cam, aes(x = lai_cam, fill = disturbance_severity ))+
  geom_density()+
  xlab("Leaf Area Index")+
  ylab("")+
  facet_grid(year ~ treatment)


<<<<<<< HEAD
## ----colors, fig.align = "center", fig.width = 6, fig.height = 4--------------
=======
## ----colors, fig.align = "center", fig.width = 6, fig.height = 4---------
>>>>>>> cb9893702271eb3ec6e45781def98757a5820179
forte_pal <- forte_colors()

# let's make a function to print the palette
print.palette <- function(x, ...) {
  n <- length(x)
  old <- par(mar = c(0.5, 0.5, 0.5, 0.5))
  on.exit(par(old))

  image(1:n, 1, as.matrix(1:n), col = x,
        ylab = "", xaxt = "n", yaxt = "n", bty = "n")

  rect(0, 0.9, n + 1, 1.1, col = rgb(1, 1, 1, 0.8), border = NA)
  text((n + 1) / 2, 1, labels = attr(x, "name"), cex = 1, family = "serif")
}

print.palette(forte_pal)

<<<<<<< HEAD
## ----disturb-6, fig.align = "center", fig.width = 6, fig.height = 4-----------
=======
## ----disturb-6, fig.align = "center", fig.width = 6, fig.height = 4------
>>>>>>> cb9893702271eb3ec6e45781def98757a5820179
# first let's make some new, more informative labels for our facets
facet.labs <- c("B" = "Bottom-Up", "T" = "Top-Down")

# let's look at distribution by year, by disturbance severity and treatment
ggplot(cam, aes(x = lai_cam, fill = disturbance_severity ))+
  geom_density(alpha = 0.4)+
  theme_minimal()+
  scale_fill_manual(values = forte_pal,
                    name = "Disturbance Severity",
                    labels = c("0%", "45%", "65%", "85%"))+
  theme(legend.position = "bottom")+
  xlab("Leaf Area Index")+
  ylab("Density")+
  facet_grid(year ~ treatment, labeller = labeller(treatment = facet.labs))

