## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  require(fortedata),
  require(ggplot2),
  require(viridis),
  require(tidyverse)
)

## ----observations, fig.height=4, fig.width=6, echo = FALSE, message=FALSE, warning=FALSE----
no_of_records.df <- fd_observations()

no_of_records <- subset(no_of_records.df, table == 'fd_inventory')


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
  ggplot2::ggtitle(paste("Figure 1: No.forest inventory observations as of:", Sys.Date()))+
  ggplot2::facet_grid(table ~ .,  space = "free")+
  ggplot2::theme(strip.text.y = element_text(size = 9), strip.background = element_rect(
    color="black", fill="white", size= 0.5, linetype="solid"))


## ----fd_inventory-------------------------------------------------------------
fd_inventory()

## ----table_inventory, fig.width=7, echo = FALSE, message=FALSE, warning=FALSE----

inv <- data.frame(fd_inventory())


inv <- subset(inv, species != "????")
inv$species <- as.factor(inv$species)

# function to make limits
stat_box_data <- function(y, upper_limit = max(df$dbh_cm, na.rm = TRUE) * 1.15) {
  return(
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('n =', length(y), sep = " ")
    )
  )
}

# good looking box plot with jitter plot on top
ggplot(inv, aes(x = species, y = dbh_cm, fill = species)) +
  geom_boxplot(outlier.size = 0) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  #geom_jitter(color = "black", size = 0.5, alpha=0.2) +
  stat_summary(
    fun.data = stat_box_data,
    geom = "text",
    hjust = 0.5,
    vjust = 0.9,
    size = 3
  )+
  theme_bw()+
  theme(
    legend.position = "none",
    plot.title = element_text(size=11)
  )+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  xlab("")+
  ylab("Diameter at Breast Height [cm]")+
  facet_grid(replicate ~ species, scales = "free", space = "free")+
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(0, "lines"))+
  ggplot2::ggtitle(paste("Figure 2:  DBH by species, by replicate"))

  

## ----biomass, fig.asp = 0.75, fig.width = 7.5, fig.align = "center", echo = FALSE, warning=FALSE, message=FALSE----
# import the biomass data
x <- fortedata::calc_biomass()

# # return density plot of replicate biomass
# ggplot2::ggplot(x, ggplot2::aes(x = biomass, fill = as.factor(replicate)))+
#   ggplot2::geom_density(alpha = 0.4)+
#   ggplot2::theme(legend.position = "none")+
#   ggplot2::facet_grid(.~replicate)+
#     ggplot2::ggtitle(paste("Figure 3:  Aboveground Woody Biomass by Replicate"))
# 



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



x %>%
  group_by(subplot_id, disturbance_severity, treatment) %>%
  summarize(biomass_plot = sum(biomass)) -> y

y$biomass_mg_ha <- y$biomass_plot * 0.001 * 10

ggplot2::ggplot(y, aes(y = biomass_mg_ha, x = disturbance_severity, fill = disturbance_severity))+
  geom_boxplot(color = "black")+
  geom_jitter(position = position_jitter(0.2), shape = 21, alpha = 0.4, size = 4)+
  xlab("Disturbance Severity")+
  ylab(expression("Above-ground Woody Biomass [Mg per "~Ha^-1*"]"))+
  theme_minimal()+
  scale_color_manual(values = forte_pal, guide = FALSE)+
  scale_fill_manual(values = forte_pal,
                    name = "Disturbance Severity",
                    labels = c("0%", "45%", "65%", "85%"))+
  theme(legend.position = "bottom")+
  ggplot2::ggtitle(paste("Figure 3:  Aboveground Woody Biomass \n by Treatment, Disturbance Severity "))+
  facet_grid(. ~ treatment, labeller = labeller(treatment = facet.labs)) 

## ----scaling, eval= FALSE-----------------------------------------------------
#  # group the data by your factor of interest, here we show how the data are summed in Figure 3.
#  x %>%
#    group_by(subplot_id, disturbance_severity, treatment) %>%
#    summarize(biomass_plot = sum(biomass)) -> y
#  
#  # Above, y$biomass is in units of kg per 0.1 ha. We want to convert to Mg per ha
#  y$biomass_mg_ha <- y$biomass_plot * 0.001 * 10
#  

