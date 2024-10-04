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
colnames(data) <- new_col_names
data <- data[-1,]


t_data <- transpose(data)
rownames(t_data) <- colnames(data)
colnames(t_data) <- t_data[1,]
t_data <- t_data[-1, ]



data_for_vegan <- as.data.frame(sapply(t_data, as.numeric))
rownames(data_for_vegan) <- rownames(t_data)

#remove empty colums
data_for_vegan <- data_for_vegan[, colSums(is.na(data_for_vegan)) < nrow(data_for_vegan)]


#note we need to edit row names for better plot below
#because R adds .1, .2, etc upon subsequent same rowname

set.seed(50)
data_nmds <- metaMDS(data_for_vegan, distance = "jaccard", autotransform = FALSE, k=2, trymax=100)
#plot(data_nmds, type="t")
plot(data_nmds, display = "sites", type="t")
