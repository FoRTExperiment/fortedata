## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  require(fortedata),
  require(ggplot2),
  require(tidyverse)
)

## ----fd_photosynthesis--------------------------------------------------------
head(data.frame(fd_photosynthesis()))

## ----photo, fig.width = 6, fig.asp = 0.65, fig.align = "center", echo = FALSE----
x <- data.frame(fd_photosynthesis())


# bring in metadata via the plot_metadata() function
df <- fortedata::plot_metadata()

# now we convert the tibble to a data frame
df <- data.frame(df)

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

# Then we merge with the metadata from above
x <- merge(x, dis.meta.data)

# For this analysis we want to code both disturbance severity and treatment as factors
x$disturbance_severity <- as.factor(x$disturbance_severity)
x$treatment <- as.factor(x$treatment)

# forte color palette
forte_pal <- forte_colors()

# first let's make some new, more informative labels for our facets
facet.labs <- c("B" = "Bottom-Up", "T" = "Top-Down")

ggplot2::ggplot(x, aes(y = photo, x = disturbance_severity, fill = disturbance_severity))+
  geom_boxplot(color = "black")+
  geom_jitter(position = position_jitter(0.2), shape = 21, alpha = 0.3)+
  xlab("Disturbance Severity")+
  ylab("Canopy Rugosity [m]")+
  theme_minimal()+
  scale_color_manual(values = forte_pal, guide = FALSE)+
  scale_fill_manual(values = forte_pal,
                    name = "Disturbance Severity",
                    labels = c("0%", "45%", "65%", "85%"))+
  theme(legend.position = "bottom")+
  ggplot2::ggtitle(paste("Figure 1:  Canopy Photosynthesis"))+
  facet_grid(. ~ treatment, labeller = labeller(treatment = facet.labs)) 


## ----fd_leaf_spectrometry-----------------------------------------------------
fd_leaf_spectrometry()

