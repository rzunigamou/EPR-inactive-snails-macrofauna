# script for revised NMDS for Macrofauna paper
# 2025-01-13 Stace Beaulieu
# uses manually created Merged_Per&Sul tab from Periphery_EcolMon2003_Block&Sieve.xlsx downloaded from google drive

library(readxl)
library(data.table) # for transpose function
library(vegan)

# will need to retain metadata from top rows
# but starting with removing top rows to quickly plot
# samples get auto-numbered 2 to 25
# will need to add row for rock color

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

