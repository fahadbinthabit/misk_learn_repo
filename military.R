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
mimo1 <- read_csv("GlobalFirePower.csv")
mimo <- na.omit(mimo1)



na.omit(mimo)
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



##### var and SD (NA removed from results)

globalaircraftstrength_mean <- mimo %>%
  summarise(avg = sum(`Total Aircraft Strength`)/length(`Total Aircraft Strength`))

globalaircraftstrength_mean$avg

gfp_as1 <- mimo$`Total Aircraft Strength`[mimo$`Total Aircraft Strength`]
gfp_as <- na.omit(gfp_as1) 

globalaircraftstrength_mean <- sum(gfp_as)/length(gfp_as)


gfp_as_var <- sum((gfp_as - globalaircraftstrength_mean)^2)/(length(gfp_as) - 1)
var(gfp_as)


gfp_as_sd <- sqrt(gfp_as_var)
sd(gfp_as)



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

























mimospider <- mimo %>% 
  select(Country, `Total Aircraft Strength`,`Total Helicopter Strength`,`Merchant Marine Strength`) %>%
  pivot_longer(-Country, names_to = "type", values_to ="strenght" ) %>%
  filter(Country == "Saudi Arabia") 

library(fmsb)

data <- as.data.frame(matrix( sample( mimospider$strenght , 10 , replace=T) , ncol=100000))
 
# Create data: note in High school for Jonathan:

colnames(data) <- mimospider$type

data <- rbind(rep(20,10) , rep(0,100) , data)


# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- rbind(rep(100000,10) , rep(0,100) , data)

radarchart( data  , axistype=1 , 
            
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,1000,100), cglwd=0.8,
            
            #custom labels
            vlcex=0.8 
)

# Check your data, it has to look like this!
# head(data)

# The default radar chart 
radarchart(data)



















