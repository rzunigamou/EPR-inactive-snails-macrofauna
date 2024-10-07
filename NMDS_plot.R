# script to explore Jaccard nmds for macrofauna presence absence
# 2024-09-26 Stace Beaulieu


# google sheet Gastropod_Counts_Assembled export tab At_sea to csv
# manually edit At_sea csv to add the word polychaete into 2nd column missing entry and remove first 6 rows and then rows > = row 27 (retain polychaete row as last row)
# manually edit to add prefix S_ Sentry Spire, SN_ Sentry Spire North, R_ Redwood, L_ Lucky's Mound
# read in At_sea_edited csv  
# initial format is species in rows and samples in columns
# it looks like R vegan requires 'the community data matrix with samples as rows and species as column'

# EDIT TO WORK WITH "supp_Table_Snail_ms" RZM 10/03/2024
# google sheet supp_Table_Snail_ms into csv

library(dplyr)
library(data.table)
library(vegan)
library(stringr)
library(ggplot2)

#input <- read.csv("Gastropod_Counts_Assembled_At_sea_20240926_edited.csv") - OLD SPREADSHEET
input <- read.csv("supp_Table_Snail_ms.csv")
data <- select(input, -(starts_with("X")))
# remove rows with taxa not fully assessed for presence absence
data <- filter(data, Feature != "Copepod")
data <- filter(data, Feature != "Cauliflower-like animal")

# remove extraneous metadata rows
data <- data[-c(1,2,3,4,6), ]

# remove asterisks
data[1,] <- gsub("\\*","",data[1,])

# append rock type to column names and remove rocktype row
new_col_names <- paste (colnames(data), data[1,], sep = "_")
data_conc <- data[-1,]
colnames(data_conc) <- new_col_names


t_data <- transpose(data_conc)
rownames(t_data) <- colnames(data_conc)
colnames(t_data) <- t_data[1,]
t_data <- t_data[-1, ]

# data for vegan NMDS -----------------

data_for_vegan <- as.data.frame(sapply(t_data, as.numeric))
rownames(data_for_vegan) <- rownames(t_data)

#remove empty colums
data_for_vegan <- data_for_vegan[, colSums(is.na(data_for_vegan)) < nrow(data_for_vegan)]

#note we need to edit row names for better plot below
#because R adds .1, .2, etc upon subsequent same rowname

#Set NMDS data, assuring binary (presence/absence) jaccard distance
set.seed(50) #arbitrary value for random number generato
jacc_dist <- vegdist(data_for_vegan, method = "jaccard", binary = T)
data_nmds <- metaMDS(jacc_dist, autotransform = FALSE)
plot(data_nmds, display = "sites", type="t")


# --------------------------Plotting in ggplot ----------
# pull out sites and color
 Rock_color <- as.character(data[1,-1])
 sites <- colnames(data[,-1])
 
# make a dataframe with the nmds points and site data

EFAplot <- data.frame(NMDS1 = data_nmds$points[,1], NMDS2 = data_nmds$points[,2], Site_OG = sites, RockType = Rock_color)
# add abbreviated site factors
EFAplot <- EFAplot |> 
  mutate(Site = case_when(
    str_detect(Site_OG, "Sentry") ~ "Sentry Spire",
    str_detect(Site_OG, "Lucky") ~ "Lucky's Mound"
  ))
EFAplot$Site <- factor(EFAplot$Site)
EFAplot$RockType <- factor(EFAplot$RockType, levels = c("Yellow","Rusty", "Rusty/Green" ))
EFAplot.mean=aggregate(EFAplot[,1:2],list(group = EFAplot$Site), mean)

#  ELLIPSES
# veganCovEllipse<-function(cov, center = c(0, 0), scale = 1, npoints = 100) 
# {
#   theta <- (0:npoints) * 2 * pi/npoints
#   Circle <- cbind(cos(theta), sin(theta))
#   t(center + scale * t(Circle %*% chol(cov)))
# }
# 
# df_ell <- data.frame()
# for(g in levels(EFAplot$Site)){
#   df_ell <- rbind(df_ell, cbind(as.data.frame(with(EFAplot[EFAplot$Site==g,],
#                                                    veganCovEllipse(cov.wt(cbind(NMDS1,NMDS2),wt=rep(1/length(MDS1),length(MDS1)))$cov,center=c(mean(MDS1),mean(MDS2)))))
#                                 ,Site=g))
# }

ggplot(data = EFAplot, aes(x = NMDS1, y = NMDS2)) + 
  geom_point(aes(shape = Site, color = RockType), size = 4) + xlim(-1.25,1.25) + ylim(-1.25,1.25) +
  coord_fixed()+
#  geom_path(data=df_ell, aes(x=MDS1, y=MDS2, linetype = Site), size=0.6)+ #ELLIPSES
#  scale_linetype_manual(values= c("dashed", "dotted"))+ #ELLIPSES
  scale_color_manual(values = c( "orange2", "brown", "aquamarine2")) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(panel.border = element_rect(fill=NA, colour="black", size = 1)) +
  theme(axis.text = element_text(colour="black", size = 12),
        axis.title = element_text(colour="black", size = 14))+
  guides(color = guide_legend(override.aes = list(shape = 15, size = 4)))
#  guides(linetype = guide_legend(order=1), shape = guide_legend(order =2)) #ELLIPSES

# GGSAVE

#  "nmds_fauna_Fig5b_2024-07-17.tif",
#   "nmds_fauna_Fig4b_2024-06-28.tif",
  ggsave(
  filename = 'inactiveNMDS.tiff',
  plot = last_plot(),
  device = NULL,
  path = "C:\\Users\\Rodrigo Zuniga\\Documents\\github\\EPR-inactive-snails-macrofauna\\", # output files in separate folder
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 500,
  limitsize = TRUE,
  bg = NULL,
)








