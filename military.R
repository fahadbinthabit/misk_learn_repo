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
theme_set(theme_bw())


mimo <- read_csv("GlobalFirePower.csv")
mimo

topfit <- mimo %>%
  


summary(mimo)

ggplot(mimo, aes(ISO3,`Fit-for-Service`)) + geom_point()


