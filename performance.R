library(tidyverse)
library(rio)


performance <- read_csv("tameeni report/perf.csv")
performance


percent_performance <- performance %>% 
  mutate(success_ = success/`sample count` ) %>%
  mutate(late = `late success`/`sample count`) %>%
  mutate(error = `Error Response`/`sample count`) %>%
  select(date, success_, late, error)

mean_v <- percent_performance %>%
  mutate(mean_s = mean(success_)) %>%
  mutate(mean_l = mean(late)) %>%
  mutate(mean_e = mean(error) ) %>%
  select(date, mean_s, mean_l, mean_e)

var(percent_performance)
sd(percent_performance$success_)
sd(percent_performance$late)
sd(percent_performance$error)


mean_of_all_means <- mean(xbar$total_str)
n <- nrow(xbar)


x~N(277.16, 912.8507 , n = 133)

histogram <- rnorm(mean = 0.231
                   , sd = 0.094,  n = 130 )
hist(histogram, main = expression(paste()))

t.test(histogram, mu = 0.231 )


