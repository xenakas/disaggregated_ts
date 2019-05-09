library(forecast)
library(ggplot2)
library(dplyr)

df_eu  <-  read.csv(file="hts_eu_a10.csv", header=TRUE, sep=",", row.names =1 )

gdp_agg <- df_eu_sums <-  rowSums(df_eu)

gdp_agg %>% head()

gdp_agg  <- ts(gdp_agg, frequency = 4, start = 2000) 

train <- window(gdp_agg, end=c(2016,4))
test <- window(gdp_agg, start=c(2017,1))

ets(train)

auto.arima(train)

auto.arima(train, ic='bic')

Arima(train, order=c(2,1,2), seasonal=c(2,1,2))

gdp_rwf <- rwf(train,h=7)
gdp_rwfwd <- rwf(train,h=7, drift=TRUE)
gdp_snaive  <- snaive(train,h=7)
gdp_theta  <- thetaf(train, h=7)
gdp_arima  <- forecast(Arima(train, order=c(2,1,2), seasonal=c(2,1,2)), h=7)
gdp_auto_arima  <- forecast(auto.arima(train), h=7)
gdp_ets <- forecast(ets(train), h=7)

cat("RW")
accuracy(gdp_rwf, test)[, c(2,5,7,8)]
cat("RW with drift ")
accuracy(gdp_rwfwd, test)[, c(2,5,7,8)]
cat("SNaive")
accuracy(gdp_snaive, test)[, c(2,5,7,8)]
cat("Theta")
accuracy(gdp_theta, test)[, c(2,5,7,8)]
cat("ARIMA")
accuracy(gdp_arima, test)[, c(2,5,7,8)]
cat("Auto ARIMA")
accuracy(gdp_auto_arima, test)[, c(2,5,7,8)]
cat("ETS")
accuracy(gdp_ets, test)[, c(2,5,7,8)]

autoplot(window(gdp_agg, start=2012)) +
    autolayer(gdp_rwf, series="RW", PI=FALSE) +
    autolayer(gdp_rwfwd, series="RW with drift", PI=FALSE) +
    autolayer(gdp_snaive, series="SNaive", PI=FALSE) +
    autolayer(gdp_theta, series="Theta Method", PI=FALSE) +
    autolayer(gdp_arima, series="ARIMA", PI=FALSE) +
    autolayer(gdp_auto_arima, series="Auto ARIMA", PI=FALSE) +
    autolayer(gdp_ets, series="ETS", PI=FALSE) +
    xlab("Year") + ylab("Mln. Euro") +
    ggtitle("Модель 0, 00") +
    guides(colour=guide_legend(title="Forecast")) +
    theme(aspect.ratio = 0.75)

library(strucchange)

## F statistics indicate one breakpoint
fs.gdp_agg <- Fstats(gdp_agg ~ 1)
plot(fs.gdp_agg)
breakpoints(fs.gdp_agg)
lines(breakpoints(fs.gdp_agg))

a <- breakpoints(fs.gdp_agg)

    ## or
bp.gdp_agg <- breakpoints(gdp_agg ~ 1)
summary(bp.gdp_agg)

## the BIC also chooses one breakpoint
plot(bp.gdp_agg)
breakpoints(bp.gdp_agg)

## fit null hypothesis model and model with 1 breakpoint
fm0 <- lm(gdp_agg ~ 1)
fm1 <- lm(gdp_agg ~ breakfactor(bp.gdp_agg, breaks = 1))

plot(gdp_agg)
lines(ts(fitted(fm0), start = 1871), col = 3)
lines(ts(fitted(fm1), start = 1871), col = 4)
lines(bp.gdp_agg)

## confidence interval
ci.gdp_agg <- confint(bp.gdp_agg)
ci.gdp_agg
lines(ci.gdp_agg)

#### Simulated data example ###
segs <- 6 # Number of segements
M <- c(1500, 2200, 800, 2500, 1000, 2000) # Segment width
#true.locations <- c(1501, 3701, 4501, 7001, 8001) # True break-point locations
seg <- NULL
p <- c(0.45, 0.25, 0.4, 0.2, 0.3, 0.6) # Specification of p's for each segment
for(j in 1:segs){
seg <- c(seg, rnbinom(M[j], size =10, prob = p[j]))
}
simdata <- as.data.frame(seg)
rm(p, M, seg, segs, j)
#plot(data[, 1])
## Not run:
## CE with the four parameter beta distribution with BIC as the selection criterion ##
obj1 <- CE.NB(simdata, distyp = 1, penalty = BIC, parallel = TRUE) # Parallel computation
obj1
profilePlot(obj1, simdata) # To obtain the mean profile plot
## CE with truncated normal distribution with BIC as the selection criterion ##
obj2 <- CE.NB(simdata, distyp = 2, penalty = BIC, parallel = TRUE) # Parallel computation
obj2
profilePlot(obj1, simdata) # To obtain the mean profile plot
## End(Not run)
