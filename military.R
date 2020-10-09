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



head(mimo[order(mimo$`Total Military Personnel`, decreasing=TRUE), ], 20) %>%
  select(Country,
         `Total Military Personnel`) %>% ggplot(aes(Country,`Total Military Personnel`)) + geom_col() + coord_flip()

head(mimo[order(mimo$`Fighter Aircraft`, decreasing=TRUE), ], 20) %>%
  select(Country,
         `Fighter Aircraft`) %>% ggplot(aes(Country,`Fighter Aircraft`)) + geom_col() + coord_flip()

head(mimo[order(mimo$`Attack Helicopters`, decreasing=TRUE), ], 20) %>%
  select(Country,
         `Attack Helicopters`) %>% ggplot(aes(Country,`Attack Helicopters`)) + geom_col() + coord_flip()


head(mimo[order(mimo$`Combat Tanks`, decreasing=TRUE), ], 20) %>%
  select(Country,
         `Combat Tanks`) %>% ggplot(aes(Country,`Combat Tanks`)) + geom_col() + coord_flip()


head(mimo[order(mimo$`Submarines`, decreasing=TRUE), ], 20) %>%
  select(Country,
         `Submarines`) %>% ggplot(aes(Country,`Submarines`)) + geom_col() + coord_flip()


head(mimo[order(mimo$`Rocket Projectors`, decreasing=TRUE), ], 20) %>%
  select(Country,
         `Rocket Projectors`) %>% ggplot(aes(Country,`Rocket Projectors`)) + geom_col() + coord_flip()

head(mimo[order(mimo$`Destroyers`, decreasing=TRUE), ], 20) %>%
  select(Country,
         `Destroyers`) %>% ggplot(aes(Country,`Destroyers`)) + geom_col() + coord_flip()



head(mimo[order(mimo$`Destroyers`, decreasing=TRUE), ], 20) %>%
  select(Country,
         `Destroyers`) %>% ggplot(aes(Country,`Destroyers`)) + geom_col() + coord_flip()





# weapons per country # !!! < change country in filter > !!!!


mimo %>%
  select(Country,`Fighter Aircraft`,`Attack Aircraft`,`Transport Aircraft`,`Attack Helicopters`,`Combat Tanks`,`Armored Fighting Vehicles`,`Self-Propelled Artillery`,`Towed Artillery`,`Rocket Projectors`, `Total Naval Assets`, `Aircraft Carriers`, Frigates, Destroyers, Corvettes, Submarines, `Patrol Craft`, `Mine Warfare Vessels`) %>%
  pivot_longer(-Country, names_to = "Weapons" , values_to = "value") %>%
  filter(Country == "Saudi Arabia") %>% 
    ggplot(aes(Weapons, value)) + geom_col() + coord_flip()



# strength per country


mimo %>% 
  select(Country, `Total Aircraft Strength`,`Total Helicopter Strength`,`Merchant Marine Strength`) %>%
  pivot_longer(-Country, names_to = "type", values_to ="strenght" ) %>%
  filter(Country == "Saudi Arabia") %>%
  ggplot(aes(type, strenght)) + geom_col() + coord_flip() 






















