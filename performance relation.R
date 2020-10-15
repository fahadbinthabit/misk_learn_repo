#find the relation between error column and the rest #aggregator report 
#technical performance 

library(tidyverse)
library(rio)
library(tinytex)
library(ggplot2)
library(data.table)


main_ = read_csv("tameeni report/arranged data.csv")
main_ = as.data.table(main_)

fit =  lm(late_success ~ sample_count, main_ )
predict(fit)
summary(fit)

ggplot(main_ , aes(x= sample_count , y= late_success)) + geom_point() + geom_smooth(method = "lm")  


fit =  lm(sample_count ~ Error_Response , main_ )
predict(fit)
summary(fit)

ggplot(main_ , aes(x= Error_Response , y=sample_count )) + geom_point() + geom_smooth(method = "lm")  

error_ = read_csv("tameeni report/error_details .csv")
error_ = as.data.table(main_)

error_ %>% group_by(`Error Message`) %>%
  mutate(ratio = `Error Count`/nrow(count()))



