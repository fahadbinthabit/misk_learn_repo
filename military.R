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
  select(Country,ISO3,
         `Total Military Personnel`)

head(mimo[order(mimo$`Fighter Aircraft`, decreasing=TRUE), ], 10) %>%
  select(Country,ISO3,
         `Fighter Aircraft`)

head(mimo[order(mimo$`Attack Helicopters`, decreasing=TRUE), ], 10) %>%
  select(Country,ISO3,
         `Attack Helicopters`)


head(mimo[order(mimo$`Combat Tanks`, decreasing=TRUE), ], 10) %>%
  select(Country,ISO3,
         `Combat Tanks`)


head(mimo[order(mimo$`Submarines`, decreasing=TRUE), ], 10) %>%
  select(Country,ISO3,
         `Submarines`)


head(mimo[order(mimo$`Rocket Projectors`, decreasing=TRUE), ], 10) %>%
  select(Country,ISO3,
         `Rocket Projectors`)

head(mimo[order(mimo$`Destroyers`, decreasing=TRUE), ], 10) %>%
  select(Country,ISO3,
         `Destroyers`)