library(dplyr)
library(ggplot2)
library(ggrepel)

setwd("Z:/Dropbox/Econometrics by Simulation/2018_09_September/")

Hurricanes <- read.csv("Hurricanes.csv", stringsAsFactors = FALSE)

Hurricanes %>% group_by(Year) %>% summarise(US_Fatalities=sum(US_Fatalities)) %>% 
  ggplot(aes(x=Year, y=US_Fatalities)) + geom_point() 

png(file="USFatalities.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Year, y=US_Fatalities, label=Name)) + 
  geom_point(color = 'red') + 
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("US Fatalities")
dev.off()

png(file="USFatalitiesLog10.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Year, y=US_Fatalities, label=Name)) + 
  geom_point(color = 'red') + scale_y_log10(breaks = c(0,1,10,100,1000,2000,3000)) +
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("US Fatalities log10")
dev.off()

png(file="Fatalities.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Year, y=Fatalities, label=Name)) + 
  geom_point(color = 'red') + 
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("Fatalities")
dev.off()

png(file="FatalitiesLog10.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Year, y=Fatalities, label=Name)) + 
  geom_point(color = 'red') + scale_y_log10(breaks = c(0,1,10,100,1000,5000,10000)) +
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("Fatalities log10")
dev.off()

png(file="Damages.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Year, y=Damage_billions, label=Name)) + 
  geom_point(color = 'red') + 
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("Damages in Billions of USD")
dev.off()

png(file="DamagesLog10.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Year, y=Damage_billions, label=Name)) + 
  geom_point(color = 'red') + scale_y_log10(breaks = c(0,1,5,10,25,50,75,100,125)) +
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("Damages in Billions of USD (log10)")
dev.off()

png(file="HighestWinds.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Year, y=Highest_winds_mph, label=Name)) + 
  geom_point(color = 'red') + 
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("Highest Winds MPH")
dev.off()

png(file="LowestPressure.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Year, y=Lowest_Pressure_mbar, label=Name)) + 
  geom_point(color = 'red') +
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("Damages in Billions of USD (log10)")
dev.off()

png(file="LowestPressureUSFatalities.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Lowest_Pressure_mbar, y=US_Fatalities, label=Name)) + 
  geom_point(color = 'red') + 
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("Lowest Pressue (mbar) against US Fatalities")
dev.off()

png(file="HighestWindsUSFatalities.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Highest_winds_mph, y=US_Fatalities, label=Name)) + 
  geom_point(color = 'red') + 
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("Highest Winds MPH against US Fatalities")
dev.off()

png(file="HighestWindsFatalities.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Highest_winds_mph, y=Fatalities, label=Name)) + 
  geom_point(color = 'red') + 
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("Highest Winds MPH against Fatalities")
dev.off()

png(file="LowestPressureUSFatalities.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Lowest_Pressure_mbar, y=Fatalities, label=Name)) + 
  geom_point(color = 'red') + 
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("Lowest Pressure (mbar) against Fatalities")
dev.off()
# US Fatalities Plot

Hurricanes %>% filter(!(Name %in% c("Maria", "Katrina"))) %>% summarize(sum(US_Fatalities))

Hurricanes$Hurricane = Hurricanes$Name
Hurricanes[rank(-Hurricanes$US_Fatalities)>5,]$Hurricane = "All other hurricanes (40)"

png(file="USFatalitiesBarGraph.png",width=800,height=600,res=72)
Hurricanes %>% 
  ggplot(aes(Hurricane, US_Fatalities)) + 
  theme_bw() + theme(text = element_text(size=15)) +
  geom_col()  + ggtitle("US Hurricane Deaths")
dev.off()

Hurricanes$Hurricane = Hurricanes$Name
Hurricanes[rank(-Hurricanes$Damage_billions)>15,]$Hurricane = "All others (30)"

png(file="USDamagesBarGraph.png",width=800,height=600,res=72)
Hurricanes %>% 
  ggplot(aes(Hurricane, Damage_billions)) + theme_bw() + 
  theme(text = element_text(size=15), axis.text.x = element_text(angle=45, hjust=1)) +
  geom_col()  + ggtitle("US Hurricane Damages (Billions)")
dev.off()


Hurricanes %>% summarise(sum(US_Fatalities>0))

Hurricanes %>% summarise(sum(Year>=2000))

Hurricanes %>% summarise(sum(Year>2003))

Hurricanes %>% summarise(sum(Year<=2003))


Hurricanes %>% summarise(sum(Damage_billions))

totUSfat = Hurricanes %>% summarise(sum(US_Fatalities)) # US Deaths
totfat = Hurricanes %>% summarise(sum(Fatalities)) # Atlantic Deaths
totfat - totUSfat # Deaths outside of the US

trim <- function(x) x %>% gsu

Hurricanes$Class <- Hurricanes$Classification %>% 
  gsub("Category|hurricane", "", .) %>% 
  sub("Tropical storm", "0", .) %>% 
  gsub("^\\s+|\\s+$","",.) %>% as.numeric()

png(file="USFatalitiesClass.png",width=800,height=600,res=72)
Hurricanes %>% ggplot(aes(x=Year, y=US_Fatalities, label=Name)) + 
  geom_point(aes(color = Class)) + 
  geom_text_repel(size=5) + theme_bw() + theme(text = element_text(size=20)) +
  ggtitle("US Fatalities")
dev.off()
