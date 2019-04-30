
library(forecast)
library(ggplot2)
library(dplyr)


library(doParallel)
library(doMC)
options(cores = 8)
registerDoMC()

library(data.table)
library(hts)  

df_eu  <-  read.csv(file="hts_eu_a10.csv", header=TRUE, sep=",", row.names =1 )

gdp_agg <- rowSums(df_eu)
gdp_agg  <- ts(gdp_agg, frequency = 4, start = 2000) 

gdp_agg  <-  gdp_agg/1000
df_eu  <-  df_eu/1000


library(dtw)
library(TSclust)

#### DTWARP 

df_eu_diff  <-  apply(df_eu , 2 , diff )

df_eu_diff_4  <-  apply(df_eu_diff , 2 , function(x) diff(x, 4) )

df_eu  <- as.data.table(df_eu)
df_eu_diff_4  <- as.data.table(df_eu_diff_4)

dim(df_eu)
dim(df_eu_diff_4)

train  <- df_eu_diff_4[1:(68-4), ]

corr_mat <- diss(train, "DTWARP")

diss_mat_dtw  <- matrix(0, nrow = dim(train)[2], ncol = dim(train)[2])
i <- j  <- a  <- 1
while (i < dim(train)[2]){
  for (j in (i+1):dim(train)[2]){ 
    # cat("(", j, i, ")")
    diss_mat_dtw[j,i] <-  corr_mat[a]
    a <- a+1
  }
  i <-i+1
}
diss_mat_dtw  <- diss_mat_dtw + t(diss_mat_dtw)
diag(diss_mat_dtw) <- NA

closests_dtw <- c()
for (i in 1:dim(df_eu)[2]){
  closests_dtw  <-  c(closests_dtw, which.min(diss_mat_dtw[i,]) )
}

names_dtw <- c()
for (i in closests_dtw){
  names_dtw  <-  c(names_dtw, colnames(train)[i])
}

iii  <- 130


autoplot(ts(train[[names_dtw[iii]]], frequency = 4, start = 2000)) +
  autolayer(ts(train[[names_dtw[iii]]], frequency = 4, start = 2000), series=names_dtw[iii]) +
  autolayer(ts(train[[iii]], frequency = 4, start = 2000), series=colnames(train)[iii]) +
  xlab("Year") + ylab("Mln. Euro") +
  theme(aspect.ratio = 0.75)

y  <-  hts(df_eu, characters = c(2,1))
gts_train  <- window(y, start=1, end=68)
gts_test  <- window(y, start=69, end=75)

names_dtw_1  <-  c(rep(NA, 29), names_dtw)
allts <- aggts(gts_train)
allf <- matrix(, nrow=7, ncol=ncol(allts))



length(diffed_reg)

length(allts[,i])

start_time <- Sys.time()
for(i in 1:29){
  model  <- Arima(msts(allts[,i], seasonal.periods = 4), order=c(3,0,0), seasonal=c(2,0,0), include.drift = TRUE, include.mean = TRUE, method = "ML")
  allf[,i] <- forecast(model, h=7)$mean
}
ms <-  foreach(i=30:ncol(allts)) %dopar% {
  reg1  <-  lag(diff(diff(allts[,names_dtw_1[i]][1:length(allts[,names_dtw_1[i]])]),4))
  reg1  <- c(rep(NA, 5), diffed_reg)
  tryCatch(        
    Arima(msts(allts[,i], seasonal.periods = 4), order=c(3,0,0), seasonal=c(2,0,0), include.drift = TRUE, include.mean = TRUE, xreg = reg1, method = "ML"), 
    error = function(e) Arima(msts(allts[,i], seasonal.periods = 4), order=c(3,0,0), seasonal=c(2,0,0), include.drift = TRUE, include.mean = TRUE, xreg = reg1, method = "CSS"))
  
  #     forecast(ms[[i]], h=7)$mean
}
end_time <- Sys.time()
end_time - start_time