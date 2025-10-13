## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  require(fortedata),
  require(ggplot2),
  require(viridis),
   require(magrittr),
  require(dplyr)
)

## ----observations, fig.height=4, fig.width=6, echo = FALSE, message=FALSE, warning=FALSE, fig.align="center"----
no_of_records.df <- fd_observations()

no_of_records <- subset(no_of_records.df, table == 'fd_litter')


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
  ggplot2::ggtitle(paste("Figure 1: No. litter obs. by species as of:", Sys.Date()))+
  ggplot2::facet_grid(table ~ .,  space = "free")+
  ggplot2::theme(strip.text.y = element_text(size = 9), strip.background = element_rect(
    color="black", fill="white", size= 0.5, linetype="solid"))


## ----lit_by_treatment, fig.width = 6, fig.asp = 1, fig.align="center", echo = FALSE, warning=FALSE, message=FALSE----
x <- fd_litter()

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

# we want to calculate the leaf mass by subtracting the tare weight `bagtare_g` from `bagmass_g`
x$leafmass_g <- x$bagmass_g - x$bagtare_g


# For this analysis we want to code both disturbance severity and treatment as factors
x$disturbance_severity <- as.factor(x$disturbance_severity)
x$treatment <- as.factor(x$treatment)

# forte color palette
forte_pal <- forte_colors()

# first let's make some new, more informative labels for our facets
facet.labs <- c("B" = "Bottom-Up", "T" = "Top-Down")

ggplot2::ggplot(x, aes(y = leafmass_g, x = disturbance_severity, fill = disturbance_severity))+
  geom_boxplot(color = "black")+
  geom_jitter(position = position_jitter(0.2), shape = 21, alpha = 0.3)+
  xlab("Disturbance Severity")+
  ylab("Litter Mass [g]")+
  theme_minimal()+
  scale_color_manual(values = forte_pal, guide = FALSE)+
  scale_fill_manual(values = forte_pal,
                    name = "Disturbance Severity",
                    labels = c("0%", "45%", "65%", "85%"))+
  theme(legend.position = "bottom")+
  ggplot2::ggtitle(paste("Figure 2:  Litter Mass"))+
  facet_grid(. ~ treatment, labeller = labeller(treatment = facet.labs)) 

## ----lai, fig.height = 3.5, fig.width = 6, fig.align = "center", echo = FALSE, message = FALSE----
# import lai values at the plot scale
x <- calc_lai()

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

ggplot2::ggplot(x, aes(y = lai, x = disturbance_severity, fill = disturbance_severity))+
  geom_boxplot(color = "black")+
  geom_jitter(position = position_jitter(0.2), shape = 21, alpha = 0.3)+
  xlab("Disturbance Severity")+
  ylab("LAI")+
  theme_minimal()+
  scale_color_manual(values = forte_pal, guide = FALSE)+
  scale_fill_manual(values = forte_pal,
                    name = "Disturbance Severity",
                    labels = c("0%", "45%", "65%", "85%"))+
  theme(legend.position = "bottom")+
  ggplot2::ggtitle(paste("Figure 3:  Leaf Area Index (LAI) From Litter Trap Data"))+
  facet_grid(. ~ treatment, labeller = labeller(treatment = facet.labs)) 


