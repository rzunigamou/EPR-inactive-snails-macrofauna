# script for revised NMDS for Macrofauna paper
# 2025-01-14 Stace Beaulieu
# uses manually created Merged_Per&Sul tab from Periphery_EcolMon2003_Block&Sieve.xlsx downloaded from google drive

library(readxl)
library(data.table) # for transpose function
library(vegan)
library(dplyr)
library(stringr)
library(ggplot2)

# will need top row "Feature/Site" for ggplot
input_top <- read_excel("C:/Users/sbeaulieu/Downloads/Periphery_EcolMon2003_Block&Sieve.xlsx", sheet = "Merged_Per&Sul", skip = 1)
input_sites <- colnames(input_top[,-1])

# but starting with removing top rows to quickly plot
# samples get auto-numbered 2 to 25

input_new <- read_excel("C:/Users/sbeaulieu/Downloads/Periphery_EcolMon2003_Block&Sieve.xlsx", sheet = "Merged_Per&Sul", skip = 6)

# transpose for vegan
t_data <- transpose(input_new)
rownames(t_data) <- colnames(input_new)
colnames(t_data) <- t_data[1,]
t_data <- t_data[-1, ]

# data for vegan NMDS -----------------

data_for_vegan <- as.data.frame(sapply(t_data, as.numeric))
rownames(data_for_vegan) <- rownames(t_data)

#Set NMDS data, assuring binary (presence/absence) jaccard distance
set.seed(50) #arbitrary value for random number generator
jacc_dist <- vegdist(data_for_vegan, method = "jaccard", binary = T)
data_nmds <- metaMDS(jacc_dist, autotransform = FALSE)
plot(data_nmds, display = "sites", type="t")

# --------------------------Plotting in ggplot ----------
# make a dataframe with the nmds points and site data
EFAplot <- data.frame(NMDS1 = data_nmds$points[,1], NMDS2 = data_nmds$points[,2], Site_OG = input_sites)
# add abbreviated site factors
EFAplot <- EFAplot |> 
  mutate(Feature = case_when(
    str_detect(Site_OG, "Sentry") ~ "Sentry Spire",
    str_detect(Site_OG, "Lucky") ~ "Lucky's Mound",
    str_detect(Site_OG, "EW") ~ "East Wall",
    str_detect(Site_OG, "BV") ~ "Biovent",
    str_detect(Site_OG, "WH") ~ "Worm Hole"
  ))
EFAplot$Feature <- factor(EFAplot$Feature)

# view order that will go into legend
levels(EFAplot$Feature)
# reorder for legend labels
EFAplot$Feature <- factor(EFAplot$Feature, levels = c("Lucky's Mound", "Sentry Spire", "Biovent", "East Wall", "Worm Hole"))

# trying a suggestion from stack overflow to set base_size
figures_base_size = 7

# need Lucky's Mound shape 16 filled circle
# need Sentry Spire shape 17 filled triangle point-up

ggplot(data = EFAplot, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(shape = Feature), size = 4) + xlim(-2.4,2.4) + ylim(-2.4,2.4) +
  coord_fixed()+
  scale_shape_manual(values = c(16, 17, 5, 8, 6)) +
  theme_bw(base_size = figures_base_size) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_rect(fill=NA, colour="black", size = 1)) +
  theme(axis.text = element_text(colour="black", size = 12),
        axis.title = element_text(colour="black", size = 14))+
  guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))

# GGSAVE

# ggsave(
#   filename = 'NMDS_Merged_Per&Sul_bs_90_20250114_1604.tiff',
# #  filename = 'NMDS_Merged_Per&Sul_bs_90_20250114_1604.eps',
#   plot = last_plot(),
#   device = NULL,
#   path = "C:\\Users\\sbeaulieu\\Desktop\\", # output files in separate folder
#   scale = 1,
# #  width = NA,
#   width = 90, # Single column 90 mm
#   height = NA,
# #  units = c("in", "cm", "mm", "px"),
#   units = "mm",
#   dpi = 500,
#   limitsize = TRUE,
#   bg = NULL,
# )


