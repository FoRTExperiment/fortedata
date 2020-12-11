## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  require(fortedata),
  require(ggplot2),
  require(tidyverse)
)

## ----observations, fig.height=4, fig.width=6, echo = FALSE, message=FALSE, warning=FALSE----
no_of_records.df <- fd_observations()

no_of_records <- subset(no_of_records.df, table == 'fd_leaf_spectrometry' | table == 'fd_photosynthesis')


ggplot2::ggplot(no_of_records, ggplot2::aes(x = as.factor(month), y = as.integer(year), fill= no_of_obs)) +
  ggplot2::geom_tile(ggplot2::aes(fill = no_of_obs), color = "black") +
  ggplot2::geom_text(ggplot2::aes(label = no_of_obs), color = "white") +
  ggplot2::coord_equal()+
  ggplot2::scale_fill_gradient(low = "#450d54", high = "#450d54", na.value = 'white')+
  ggplot2::scale_y_reverse()+
  ggplot2::theme_minimal()+
  ggplot2::theme(legend.position = "none")+
  ggplot2::ylab("Year")+
  ggplot2::xlab("Month")+
  ggplot2::ggtitle(paste("Figure 1: No. of observations currently available \nin each leaf physiology function as of:", Sys.Date()))+
  ggplot2::facet_grid(table ~ .,  space = "free")+
  ggplot2::theme(strip.text.y = element_text(size = 9), strip.background = element_rect(
    color="black", fill="white", size= 0.5, linetype="solid"))


## ----fd_photosynthesis--------------------------------------------------------
head(data.frame(fd_photosynthesis()))

## ----photo, fig.width = 6, fig.asp = 0.65, fig.align = "center", echo = FALSE----
x <- data.frame(fd_photosynthesis())


# bring in metadata via the plot_metadata() function
df <- fortedata::fd_plot_metadata()

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
  ylab(expression(atop('Subcanopy leaf photosynthetic rate', paste('('~mu~'mol' ~CO[2]~ m^-2~s^-1*')'))))+
  theme_minimal()+
  scale_color_manual(values = forte_pal, guide = FALSE)+
  scale_fill_manual(values = forte_pal,
                    name = "Disturbance Severity",
                    labels = c("0%", "45%", "65%", "85%"))+
  theme(legend.position = "bottom")+
  ggplot2::ggtitle(paste("Figure 2:  2018 Subcanopy Photosynthesis"))+
  facet_grid(. ~ treatment, labeller = labeller(treatment = facet.labs)) 


## ----fd_leaf_spectrometry-----------------------------------------------------
fd_leaf_spectrometry()

## ----ndvi-plot, fig.width = 6, fig.asp = 0.65, fig.align = "center", echo = FALSE----
x <- data.frame(fd_leaf_spectrometry())


# bring in metadata via the plot_metadata() function
df <- fortedata::fd_plot_metadata()

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
dis.meta.data <- dis.meta.data[c(25:32), ]

# Then we merge with the metadata from above
x <- merge(x, dis.meta.data)

# For this analysis we want to code both disturbance severity and treatment as factors
x$disturbance_severity <- as.factor(x$disturbance_severity)
x$treatment <- as.factor(x$treatment)

# forte color palette
forte_pal <- forte_colors()

# first let's make some new, more informative labels for our facets
facet.labs <- c("B" = "Bottom-Up", "T" = "Top-Down")

# filter by index, select only NDVI observations
 x %>%
  filter(index == "NDVI" & index_value > 0.2) -> y

ggplot2::ggplot(y, aes(y = index_value, x = disturbance_severity, fill = disturbance_severity))+
  geom_boxplot(color = "black")+
  geom_jitter(position = position_jitter(0.2), shape = 21, alpha = 0.3)+
  xlab("Disturbance Severity")+
  ylab("Canopy Leaf NDVI") +
  theme_minimal()+
  scale_color_manual(values = forte_pal, guide = FALSE)+
  scale_fill_manual(values = forte_pal,
                    name = "Disturbance Severity",
                    labels = c("0%", "45%", "65%", "85%"))+
  theme(legend.position = "bottom")+
  ggplot2::ggtitle(paste("Figure 3:  2018 Canopy NDVI")) 


