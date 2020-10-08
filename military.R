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
theme_set(theme_bw())


mimo <- read_csv("GlobalFirePower.csv")

glimpse(mimo)



head(mimo[order(mimo$`Total Military Personnel`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Total Military Personnel`) %>% ggplot(aes(Country,`Total Military Personnel`)) + geom_col()

head(mimo[order(mimo$`Fighter Aircraft`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Fighter Aircraft`) %>% ggplot(aes(Country,`Fighter Aircraft`)) + geom_col()

head(mimo[order(mimo$`Attack Helicopters`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Attack Helicopters`) %>% ggplot(aes(Country,`Total Military Personnel`)) + geom_col()


head(mimo[order(mimo$`Combat Tanks`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Combat Tanks`) %>% ggplot(aes(Country,`Combat Tanks`)) + geom_col()


head(mimo[order(mimo$`Submarines`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Submarines`) %>% ggplot(aes(Country,`Submarines`)) + geom_col()


head(mimo[order(mimo$`Rocket Projectors`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Rocket Projectors`) %>% ggplot(aes(Country,`Rocket Projectors`)) + geom_col()

head(mimo[order(mimo$`Destroyers`, decreasing=TRUE), ], 10) %>%
  select(Country,
         `Destroyers`) %>% ggplot(aes(Country,`Destroyers`)) + geom_col() + coord_flip()









