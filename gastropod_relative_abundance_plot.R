# script to generate gastropod relative abundance plot for AT50-20 sorted inactive sulfide samples
# Mikayla Harris, Stace Beaulieu
# 2024-09-25

library(ggplot2)
library(tidyverse)

# input file is manually compiled from 3 datasheets: Ayinde’s, Mikayla’s, and the At-Sea counts
# input file shared in Google folder not on GitHub
gastropod <- read.csv("gastropod_data_20240919.csv")
# note that Taxon Pachydermia was added to the _20240919.csv
# because all Counts for Pachydermia are zero in this set of samples
# remove Pachydermia from the data frame to retain Mikayla's scale_fill_manual
gastropod <- dplyr::filter(gastropod, Taxon != "Pachydermia")

#factor lets you reorder things
gastropod$Rock_Color <- factor(gastropod$Rock_Color, levels=c("rusty2","rusty2/3","rusty3/4","brown4","green5"))

#fill= tells it what to split the bars up by
#position = "" tells it whether the bar graph should be grouped("dodge"),
#stacked("stack"), or percent stacked("fill"). 
#scale_fill_manual(values = c()) changes the color palette. 
#labs lets you rename the y and x axis
#focus on theme/change aspects around

ggplot(gastropod, aes(fill= Taxon, x = Rock_Color, y = Count))+
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = c("#d73027", "#f46d43", "#fdae61", "#fee090","#ffffbf","#e0f3f8","#abd9e9","#74add1","#4575b4"))+
  labs(title= "Relative Abundance of Species Across Rock Colors at Inactive Sulfide Vents", x= "Rock Type", y = "Relative Abundance")+
  theme_gray()

#lollipop plot (not sure if useful?)
ggplot(gastropod, aes(color = Taxon, x = Rock.Color, y= Count))+
  geom_point()+
  geom_segment(x= gastropod$Rock.Color, xend= gastropod$Rock.Color, y=0, yend= gastropod$Count)
  #facet_wrap(~Taxon)

#violin plot (also not sure if this is useful; struggled to figure out how to format it)
ggplot(gastropod, aes(x=Rock.Color, y=Count, fill=Rock.Color)) + 
  geom_violin()

