gdp_agg <- read.csv(file="data/eu_gdp_eu28.csv", header=TRUE, sep=",")
gdp_agg  <- ts(gdp_agg$values, frequency = 4, start = 1995) 
plot(gdp_agg)

train <- window(gdp_agg, end=c(2016,4))
test <- window(gdp_agg, start=c(2017,1))

library(dplyr)
library(fasster)



time_count  <- seq(1, length(train), by=1)


train_in_diff = train %>% 
  as_tsibble %>% mutate(d_value = value - lag(value))  %>% mutate(time_count)  #%>% filter(index >= "2010-01-01")
fit = train %>% as_tsibble() %>%
  model(fasster = FASSTER(value ~ poly(2) + trig(4)))
fit_in_diff = train_in_diff %>%
  model(fasster = FASSTER(d_value ~ poly(1) + trig(4)))

fit %>% summary 

fit[[1]][[1]][["fit"]]  %>% summary 

fit[[1]][[1]][["fit"]][["dlm"]]  %>% summary 


fit_time = train_in_diff %>%
  model(fasster = FASSTER(value ~ poly(2) + trig(4) + time_count)) #+ time_count^2))
fit_time %>% summary 