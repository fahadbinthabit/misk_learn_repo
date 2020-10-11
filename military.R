# world power
#Fahad BinThabit  
#08.10.2020


#packages

library(tidyverse)
library(rio)



# Data set:

mimo1 <- read_csv("GlobalFirePower.csv")
mimo <- na.omit(mimo1)

# :


defense_budget
airpower 
military_units 
var(gfp_as) # Global strength variance
sd(gfp_as)  #Global SD strenght


# plots : 



defense_budget %>%
  mutate(name = fct_reorder(Country, `Defense Budget`)) %>%
  ggplot( aes(x= name, y = `Defense Budget` )) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
 
as_tibble(mitlitary_units_tidy)  %>%
  mutate(name = fct_reorder(Country, total_count)) %>%
  ggplot( aes(x= name, y = total_count )) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()

as_tibble(airpower_) %>%
  mutate(name = fct_reorder(Country, total_str)) %>%
  ggplot( aes(x= name, y = total_str )) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw()
  


# Run Code :


defense_budget <- head(mimo[order(mimo$`Defense Budget`,
                          decreasing=TRUE), ], 20) %>%
  
  select(Country,
         `Defense Budget`)





military_assets <- mimo %>%
  select(Country,`Fighter Aircraft`,`Attack Aircraft`,`Transport Aircraft`,`Attack Helicopters`,`Combat Tanks`,`Armored Fighting Vehicles`,`Self-Propelled Artillery`,`Towed Artillery`,`Total Naval Assets`, `Aircraft Carriers`, Frigates, Destroyers, Corvettes, `Rocket Projectors`, Submarines, `Patrol Craft`, `Mine Warfare Vessels` ) %>%
  group_by(Country)

masset <- military_assets %>%
  pivot_longer(-Country, names_to = "mili_assets" , values_to = "Count") %>%
  group_by(Country)


military_units <-   masset %>%
  select(Country, Count) %>%
  group_by(Country) %>%
  mutate(total_count = sum (Count)) %>%
  select(Country, total_count) %>%
  filter(row_number(Country) == TRUE ) %>%
  arrange(desc(total_count))
  
mitlitary_units_tidy <- head(military_units[order(military_units$total_count,
                                  decreasing=TRUE), ], 20)   



strenght <- mimo %>%
  select(Country, `Total Aircraft Strength`,
         `Total Helicopter Strength` ) %>%
  group_by(Country)

tidystrength <- strenght %>%
  pivot_longer(-Country, names_to = "strength_" , values_to = "total_s") %>%
  group_by(Country)


airpower <-   tidystrength %>%
  select(Country, total_s) %>%
  group_by(Country) %>%
  mutate(total_str = sum (total_s)) %>%
  select(Country, total_str) %>%
  filter(row_number(Country) == TRUE ) %>%
  arrange(desc(total_str)) 
  
airpower_ <- head(airpower[order(airpower$total_str,
                                                  decreasing=TRUE), ], 20)  



Rocket_Projectors <- mimo %>%
  select(Country, `Rocket Projectors`) %>%
  arrange(desc(`Rocket Projectors`))


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



#top ten military Army in terms of  Military personnel , Tanks , etc :



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



##### var and SDfor total strengt (NA removed from results) :






# weapons per country # !!! < change country in filter > !!!! :



mimo %>%
  select(Country,`Fighter Aircraft`,`Attack Aircraft`,`Transport Aircraft`,`Attack Helicopters`,`Combat Tanks`,`Armored Fighting Vehicles`,`Self-Propelled Artillery`,`Towed Artillery`,`Rocket Projectors`, `Total Naval Assets`, `Aircraft Carriers`, Frigates, Destroyers, Corvettes, Submarines, `Patrol Craft`, `Mine Warfare Vessels`, `Total Military Personnel` ,`Active Personnel`, `Reserve Personnel`, ) %>%
  pivot_longer(-Country, names_to = "Weapons" , values_to = "value") %>%
  filter(Country == "Saudi Arabia") %>% 
    ggplot(aes(Weapons, value)) + geom_col() + coord_flip()



# strength per country


mimo %>% 
  select(Country, `Total Aircraft Strength`,`Total Helicopter Strength`) %>%
  pivot_longer(-Country, names_to = "type", values_to ="strenght" ) %>%
  filter(Country == "Saudi Arabia") %>%
  ggplot(aes(type, strenght)) + geom_col() + coord_flip() 




