###### This script builds the plots found in the ESSD manuscript
###### It does not run internally and could be made into a markdown file
###### at some point, but currently the ggsave functions
###### return the high resolution versions of the plots found in the ESSD
###### manuscript

require(fortedata)
require(ggplot2)
no_of_records <- fd_observations()



no_of_records$data_record <- factor(no_of_records$table,
                                    levels = c("fd_soil_respiration",
                                               "fd_leaf_spectrometry",
                                               "fd_photosynthesis",
                                               "fd_hemi_camera",
                                               "fd_ceptometer",
                                               "fd_canopy_structure",
                                               "fd_inventory",
                                               "fd_litter"),
                                    labels = c("Soil Respiration",
                                               "Leaf Spec",
                                               "Hemi Camera",
                                               "Photosynthesis",
                                               "Light",
                                               "Canopy Structure",
                                               "Forest Inv",
                                               "Litter"))

# fig 2 Heatmap modified
ggplot(no_of_records, aes(x = as.factor(month), y = as.integer(year), fill= no_of_obs)) +
  geom_tile(aes(fill = no_of_obs), color = "black") +
  geom_text(aes(label = no_of_obs), color = "white") +
  coord_equal()+
  scale_fill_gradient(low = "#450d54", high = "#450d54", na.value = 'white')+
  #scale_fill_viridis(na.value = 'white')+
  scale_y_reverse()+
  theme_minimal()+
  theme(legend.position = "none")+
  ylab("Year")+
  xlab("Month")+
  ggtitle(paste("No. of observations currently availble \nin each function as of:", Sys.Date()))+
  facet_grid(data_record ~ .,  space = "free")+
  theme(strip.text.y = element_text(size = 8), strip.background = element_rect(
    color="black", fill="white", size= 0.5, linetype="solid"))

#ggsave("./essd/fig_2.tiff", units = "in", width = 5, height = 8, dpi=300)



##### fig 3
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
  ylab("Diameter at Breast Height (cm)")+
  facet_grid(replicate ~ species, scales = "free", space = "free")+
  theme(
    strip.background.x = element_blank(),
    strip.text.x = element_blank(),
    panel.spacing.x = unit(0, "lines")
  )

#ggsave("essd/images/fig_3.tiff", units = "in", width = 6, height = 4, dpi=300)


##### fig 4
pal <- viridis(4, begin = 0, end = 0.9)
rs <- fd_soil_respiration()
# Violin plot of rs data by rep
rs %>%
  ggplot(aes(x = replicate, y = soil_co2_efflux)) +
  geom_jitter(aes(color = replicate), alpha = 0.2) +
  geom_violin(aes(color = replicate), fill = NA, size = 1) +
  scale_color_manual(values = pal) + theme_bw() +
  labs(y = expression(Soil~Respiration~(μmol~CO[2]~m^-2~s^-1)), x = "Replicate")+
  theme(legend.position = "none")

#ggsave("./essd/images/fig_4.tiff", units = "in", width = 6, height = 4, dpi=300)


#####  fig 5
df <- data.frame(fd_litter())

df$leaf_mass <- df$bagmass_g - df$bagtare_g


df <- subset(df, species != "CWD")
df <- subset(df, species != "SWD")


# good looking box plot
pal <- viridis(4, begin = 0, end = 0.9)
df %>%
  ggplot(aes(x = replicate, y = leaf_mass)) +
  geom_jitter(aes(color = replicate), alpha = 0.2) +
  geom_violin(aes(color = replicate), fill = NA, size = 1) +
  scale_color_manual(values = pal) + theme_bw() +
  ylab("Leaf Mass (g)")+
  xlab("Replicate")+
  theme(legend.position = "none")

#ggsave("./essd/images/fig_5.tiff", units = "in", width = 5, height = 4, dpi=300)
