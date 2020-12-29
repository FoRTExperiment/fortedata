## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  require(fortedata),
  require(ggplot2),
  require(tidyverse),
  require(viridis)
)

## ----observations, fig.asp = 0.6, fig.width=6, echo = FALSE, message=FALSE, warning=FALSE----
no_of_records.df <- fd_observations()
no_of_records <- subset(no_of_records.df, table == "fd_soil_respiration")

sr <- fd_soil_respiration()

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
  ggplot2::ggtitle(paste("Figure 1: No. of observations currently available \nin soil respiration dataset as of:", Sys.Date()))+
  ggplot2::facet_grid(table ~ .,  space = "free")+
  ggplot2::theme(strip.text.y = element_text(size = 9), strip.background = element_rect(
    color="black", fill="white", size= 0.5, linetype="solid"))


## ----fd_soil_respiration------------------------------------------------------
fortedata::fd_soil_respiration()

## ----soil_r, fig.width = 6, fig.asp = 0.7, fig.align = "center", echo = FALSE, warning = FALSE, messages = FALSE----
x <- data.frame(fd_soil_respiration())

# add the year column
x$year <- as.numeric(format(x$date, "%Y"))

# bring in metadata via the plot_metadata() function
df <- data.frame(fortedata::fd_plot_metadata())

# # First we want to concatenate our replicate, plot and subplot data to make a subplot_id column 
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

# remove these weird ones with no year
x <- x[!is.na(x$year), ]
# forte color palette
forte_pal <- forte_colors()

# filter to growing season
x %>%
  filter(date >= as.Date("2019-06-01") & date <= as.Date("2019-09-15")) -> y

# first let's make some new, more informative labels for our facets
facet.labs <- c("B" = "Bottom-Up", "T" = "Top-Down")

ggplot2::ggplot(y, aes(y = soil_co2_efflux, x = date, color = disturbance_severity,
                       fill = disturbance_severity))+
  geom_point(shape = 21, color = "black", alpha = 0.3)+
  xlab("Disturbance Severity")+
  ylab(bquote('Soil '*~CO[2]~ "efflux ["*mu~'mol' ~CO[2]~ m^-2~s^-1*']')) +
  ylim(0, 15)+
  theme_minimal()+
  scale_color_manual(values = forte_pal, guide = FALSE)+
  scale_fill_manual(values = forte_pal,
                    name = "Disturbance Severity",
                    labels = c("0%", "45%", "65%", "85%"))+
  theme(legend.position = "bottom")+
  ggtitle(expression('Figure 2: Soil '*~CO[2]~'efflux ['*mu~'mol' ~CO[2]~ m^-2~s^-1*'] by replicate, by year'))+
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, show.legend = FALSE, size = 1.5)+
  facet_grid(. ~ treatment, labeller = labeller(treatment = facet.labs)) 


## ---- met, fig.width = 6, fig.asp = 0.5, fig.align = "center", echo = FALSE, warning = FALSE, messages = FALSE----


ggplot2::ggplot(y, aes(y = soil_temp, x = date, color = disturbance_severity,
                       fill = disturbance_severity))+
  #geom_line(alpha = 0.8)+
  geom_point(shape = 21, color = "black", alpha = 0.3)+
  xlab("Disturbance Severity")+
  ylab(expression('Temperature ['*~degree*C*']')) +
  theme_minimal()+
  scale_color_manual(values = forte_pal, guide = FALSE)+
  scale_fill_manual(values = forte_pal,
                    name = "Disturbance Severity",
                    labels = c("0%", "45%", "65%", "85%"))+
  theme(legend.position = "bottom")+
  ggplot2::ggtitle(paste('Figure 3: Growing Season Soil Temperature [C]\n by replicate, by year'))+
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, show.legend = FALSE, size = 1.5)+
  facet_grid(. ~ treatment, labeller = labeller(treatment = facet.labs)) 


ggplot2::ggplot(y, aes(y = vwc, x = date, color = disturbance_severity,
                       fill = disturbance_severity))+
  #geom_line(alpha = 0.8)+
  geom_point(shape = 21, color = "black", alpha = 0.3)+
  xlab("Disturbance Severity")+
  ylab("volumetric Water Content [%}") +
  theme_minimal()+
  scale_color_manual(values = forte_pal, guide = FALSE)+
  scale_fill_manual(values = forte_pal,
                    name = "Disturbance Severity",
                    labels = c("0%", "45%", "65%", "85%"))+
  theme(legend.position = "bottom")+
  ggtitle(paste(expression('Figure 4:  Growing Season Soil Moisture [%]\n by replicate, by year')))+
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, show.legend = FALSE, size = 1.5)+
  facet_grid(. ~ treatment, labeller = labeller(treatment = facet.labs)) 


