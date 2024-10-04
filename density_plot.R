density <- read.csv("Density_Data_Harris - Sheet1.csv")

density$Rock_Color <- factor(density$Rock_Color, levels=c("rusty2","rusty2/3","rusty3/4","brown4","green5"))

ggplot(density, aes(x = Rock_Color, y = Population_Density, fill= Rock_Color))+
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#ffffcc", "#a1dab4","#41b6c4","#2c7fb8","#253494"))+
  labs(title= "Gastropod Population Density across Rock Colors at Inactive Sulfides", x= "Rock Type", y = "Population Density (#gastropods/cm²)")+
  theme_dark()
