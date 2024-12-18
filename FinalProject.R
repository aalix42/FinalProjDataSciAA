#install.packages(c("dplyr", "lubridate"))
#install.packages("ggplot2")
install.packages("terra")
install.packages("ggmap")
install.packages("osmdata")
install.packages("sf")
install.packages("tidyverse")
library(dplyr)
library(lubridate)
library(ggplot2)
library(terra)
library(ggmap)
library(osmdata)
library(sf)
library(tidyverse)

neon2015 <- read.csv("C:/Users/aalix/OneDrive/Desktop/NEONPlants/NEON2015.csv")
neon2017 <- read.csv("C:/Users/aalix/OneDrive/Desktop/NEONPlants/NEON2017.csv")
neon2022 <- read.csv("C:/Users/aalix/OneDrive/Desktop/NEONPlants/NEON2022.csv")
neon2019 <- read.csv("C:/Users/aalix/OneDrive/Desktop/NEONPlants/NEON2019.csv")
neon2016 <- read.csv("C:/Users/aalix/OneDrive/Desktop/NEONPlants/NEON2016.csv")

#what am I doing here?
neon2015$year <- rep(2015, nrow(neon2015))
neon2022$year <- rep(2022, nrow(neon2022))
neon2017$year <- rep(2017, nrow(neon2017))
neon2019$year <- rep(2019, nrow(neon2019))
neon2016$year <- rep(2016, nrow(neon2016))


neon2015Df <- data.frame(neon2015)
select2015Df <- neon2015Df %>%
  select(decimalLatitude, decimalLongitude, subplotID, percentCover, nlcdClass, year)

neon2016Df <- data.frame(neon2016)
select2017Df <- neon2016Df %>%
  select(decimalLatitude, decimalLongitude, subplotID, percentCover, nlcdClass, year)

neon2022Df <- data.frame(neon2022)
select2022Df <- neon2022Df %>%
  select(decimalLatitude, decimalLongitude, subplotID, percentCover, nlcdClass, year)

neon2019Df <- data.frame(neon2019)
select2019Df <- neon2019Df %>%
  select(decimalLatitude, decimalLongitude, subplotID, percentCover, nlcdClass, year)

NeonBINDED <- rbind(select2019Df, select2015Df, select2017Df,select2022Df)

##make two data frames that match! select function and rename to create a data frame that matches. 

CoverAv15 <- neon2015Df %>%
  group_by(subplotID) %>%
  summarise(meanCover = mean(percentCover, na.rm = TRUE))

Joined15 <- left_join(CoverAv15, select2015Df)

CoverAv22 <- neon2022Df %>%
  group_by(subplotID) %>%
  summarise(meanCover = mean(percentCover, na.rm = TRUE)) 

Joined22 <- left_join(CoverAv22, select2022Df)

CoverAv16 <- neon2016Df %>%
  group_by(subplotID) %>%
  summarise(meanCover = mean(percentCover, na.rm = TRUE))


CoverAv19 <- neon2019Df %>%
  group_by(subplotID) %>%
  summarise(meanCover = mean(percentCover, na.rm = TRUE)) 

Joined19 <- left_join(CoverAv19, select2019Df)

select2016Df <- select2016Df %>%
  mutate(subplotID = as.integer(subplotID))

Joined16 <- left_join(CoverAv16, select2016Df, by = "subplotID")


NeonBINDEDAverages <- rbind(Joined19, Joined16, Joined15, Joined22)

#plantNEON2022plantCover2022 <- neon2022 %>% 
#  group_by(subplotID) %>% 
 # summarise(mean.cover= mean(percentCover))

#plantCover2015 <- neon2015 %>% 
#  group_by(subplotID) %>% 
#  summarise(mean.cover = mean(na.omit(percentCover)))

PlotCoverAv <- NeonBINDED %>%
  group_by(subplotID) %>%
  summarise(mean(na.omit(NeonBINDED$percentCover)))

Summary2022_2015 <- NeonBINDED %>% 
  group_by(year) %>% 
  summarise(mean.cover = mean(na.omit(percentCover)))

#chart for check in #2 a graph  
ggplot(data = select2015Df,
       aes(x = percentCover, y=nlcdClass))+ 
        geom_boxplot()+
        labs(x="percent Coverage", y="NLCDclass")+ #axis labels
theme_classic()

ggplot(data = select2022Df,
       aes(x = percentCover, y=nlcdClass))+ 
  geom_boxplot()+
  labs(x="Percent Coverage", y="NLCD Class")+ #axis labels
  theme_classic() + 
  labs(caption = "Plant coverage by NLCD class in 2022") +
  theme(plot.caption = element_text(size=9, color="black", face="italic"))

ggplot(data = NeonBINDEDAverages, # data for plot
       aes(x = meanCover, y=year, color=nlcdClass) )+ # aes, x and y
  geom_point()+ # make points at data point # use lines to connect data points
  labs(x="Average Cover per Plot", y="year")+ # make axis labels
  theme_classic()


ggplot(NeonBINDEDAverages, 
       aes(x = year, y = meanCover, group = (as.factor(subplotID)), color= (as.factor(subplotID)))) +
  geom_line() +  
  labs(title = "Percent Coverage Over Time", x = "Year", y = "Plant Coverage (%)") +
  theme_minimal()

