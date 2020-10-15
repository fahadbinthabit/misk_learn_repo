#find the relation between error column and the rest #aggregator report 
#technical performance 

library(tidyverse)
library(rio)
library(tinytex)
library(ggplot2)
library(data.table)
library(scales)


main_ = read_csv("tameeni report/arranged data.csv")
main_ = as.data.table(main_)

fit =  lm(Error_Response ~ sample_count, main_ )
predict(fit)
summary(fit)

plotit <- ggplot(main_ , aes(x= sample_count , y= Error_Response , col= late_success )) + geom_point() + geom_smooth(method = "lm")  


fit =  lm(sample_count ~ Error_Response , main_ )
predict(fit)
summary(fit)

ggplot(main_ , aes(x= Error_Response , y=sample_count )) + geom_point() + geom_smooth(method = "lm")  

error_ = read_csv("tameeni report/error_details .csv")
error_ = as.data.table(error_)

error_ratio <- error_ %>% 
  mutate( ratio_ = `Error Count` / 3194 ) %>%
  select(`Error Message`, ratio_)
error_ratio = as.data.table(error_ratio)

error_ratio <- head(error_ratio[order(error_ratio$ratio,
                                 decreasing=TRUE), ], 5) 

percentage <- error_ratio %>% group_by(`Error Message`) 

percentage


blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold"))


pie + scale_fill_brewer("Blues") + blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = ratio + c(0, cumsum("Error Count")[-length("Error Count")]), 
                label = percent( "Error Count"/100)), size=10)



43.2+30.4+17.4

view(error_)

