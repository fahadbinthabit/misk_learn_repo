# world power
#Fahad BinThabit  
#08.10.2020


#packages

library(tidyverse)
library(rio)
library(lubridate)
library(rgios)
library(cowplot)
library(googleway)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(libwgeom)
library(sf)
library(tidyverse)
library(rio)
library(lubridate)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(ggrepel)
library(HSAUR)
library(dplyr)
library(scales)
library(rgeos)
library(classInt)
library(RColorBrewer)
library(rworldmap)
theme_set(theme_bw())



# Data set

mimo <- read_csv("GlobalFirePower.csv")




#top ten military Army in terms of  Military personnel , Tanks , etc


scipen(9000000)
head(mimo[order(mimo$`Total Military Personnel`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Total Military Personnel`) %>% ggplot(aes(Country,`Total Military Personnel`)) + geom_col() + coord_flip()

head(mimo[order(mimo$`Fighter Aircraft`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Fighter Aircraft`) %>% ggplot(aes(Country,`Fighter Aircraft`)) + geom_col() + coord_flip()

head(mimo[order(mimo$`Attack Helicopters`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Attack Helicopters`) %>% ggplot(aes(Country,`Total Military Personnel`)) + geom_col() + coord_flip()


head(mimo[order(mimo$`Combat Tanks`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Combat Tanks`) %>% ggplot(aes(Country,`Combat Tanks`)) + geom_col() + coord_flip()


head(mimo[order(mimo$`Submarines`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Submarines`) %>% ggplot(aes(Country,`Submarines`)) + geom_col() + coord_flip()


head(mimo[order(mimo$`Rocket Projectors`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Rocket Projectors`) %>% ggplot(aes(Country,`Rocket Projectors`)) + geom_col() + coord_flip()

head(mimo[order(mimo$`Destroyers`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Destroyers`) %>% ggplot(aes(Country,`Destroyers`)) + geom_col() + coord_flip()



head(mimo[order(mimo$`Destroyers`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Destroyers`) %>% ggplot(aes(Country,`Destroyers`)) + geom_col() + coord_flip()





# weapons per country # !!! < change country in filter > !!!!


mimo <- mimos %>%
  select(Country,`Fighter Aircraft`,`Attack Aircraft`,`Transport Aircraft`,`Attack Helicopters`,`Combat Tanks`,`Armored Fighting Vehicles`,`Self-Propelled Artillery`,`Towed Artillery`,`Rocket Projectors`, `Total Naval Assets`, `Aircraft Carriers`, Frigates, Destroyers, Corvettes, Submarines, `Patrol Craft`, `Mine Warfare Vessels`) 

mimos_ %>%
  pivot_longer(-Country, names_to = "Weapons" , values_to = "value") %>%
  filter(Country == "Russia") %>% 
  ggplot(aes(Weapons, value)) + geom_col() + coord_flip()






































world <- ne_countries(scale = "medium", returnclass = "sf")
# gene world map
ggplot(data = world) +
  geom_sf() +
  labs( x = "Longitude", y = "Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$admin)), " countries)"))

world


mp_map <- c( mimo$Country, mimo$ISO3, mimo$`Total Military Personnel`)
                               
inthemap <- joinCountryData2Map(mimo, 
                                  joinCode = "ISO3",
                                  nameJoinColumn = "Country")

mapParams <- mapCountryData(inthemap, 
                            nameColumnToPlot= "Total Military Personnel",
                            oceanCol = "azure2",
                            catMethod = "categorical",
                            missingCountryCol = gray(.8),
                           # colourPalette = c("coral",
                                             # "coral2",
                                           #   "coral3", "orangered", 
                                           #   "orangered3", "orangered4"),
                            addLegend = F,
                            mapTitle = "",
                            border = NA)
# add legend and display map
do.call(addMapLegendBoxes, c(mapParams,
                             x = 'bottom',
                             title = "No. of visits",
                             horiz = TRUE,
                             bg = "transparent",
                             bty = "n"))












