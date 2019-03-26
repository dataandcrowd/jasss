######################################
##1.Import multiple files to R env--##
######################################
library(tidyverse)

setwd("~/Dropbox (Cambridge University)/2018_Cambridge/[Conferences and Journals]/J_JASSS/JASSS_Data")
files  <- list.files(pattern= "seoul_|gg_.*.csv")
tables <- lapply(files, read.csv, header = TRUE,fileEncoding="CP949", encoding="UTF-8")
aq <- do.call(rbind, tables)
aq[aq == -999] <- NA
setwd("~/Dropbox (Cambridge University)/2018_Cambridge/[Conferences and Journals]/J_JASSS/JASSS_Data")

for(i in c(3,5:10)){
  aq[,i] <- as.numeric(aq[,i])
}

aq$time1 <- lubridate::parse_date_time(as.character(aq$Date), "ymdH", tz = "Asia/Seoul")
aq$DATE <- lubridate::as_date(as.character(aq$Date), format = "%Y%m%d",tz = "Asia/Seoul")
aq$hour <- lubridate::hour(aq$time)
aq <- aq %>% arrange(Station.ID)


# Gangnam Background station
aq.g <- aq %>% filter (Station.ID == 111261)
gn <- aq.g %>% select(time1, DATE, hour,pm10)

# Gwanak Background station
aq.gw <- aq %>% filter (Station.ID == 111251)
gw <- aq.gw %>% select(time1, DATE, hour,pm10)


####################
#-- Missing Data --#
####################
library(imputeTS)
library(xts)
library(forecast)

## In the package, there are three steps that one can follow
## 1) Plot NA, 2) Use various NA functions, 3) Analyse after imputation

#-- 1. Transform a dataframe to a multi-sequence timeseries data
time_index <- seq(from = as.POSIXct("2010-01-01 01:00"), 
                  to = as.POSIXct("2018-01-01 00:00"), by = "hour", tz = "Asia/Seoul")
gn.NA <- gn[[4]]
df_ts <- msts(gn.NA, seasonal.periods = c(24*7) )

gw.NA <- gw[[4]]
gw_ts <- msts(gw.NA, seasonal.periods = c(24*7) )

#-- 2. NA distribution
# 1) Gangnam
plotNA.distribution(df_ts)
plotNA.distribution(df_ts[17520:26303], main = "Distribution of NAs in 2012") # year 2012
plotNA.distributionBar(df_ts, breaks=20,main = "Distribution of NAs\n 2010-2018") # For long time-series
plotNA.distributionBar(df_ts[17520:26303], main = "Distribution of NAs\n 2012", breaks=20)

# 2) Gwanak
plotNA.distribution(gw_ts)
plotNA.distribution(gw_ts[17520:26303], main = "Distribution of NAs in 2012") # year 2012
plotNA.distributionBar(gw_ts, breaks=20,main = "Distribution of NAs\n 2010-2018") # For long time-series
plotNA.distributionBar(gw_ts[17520:26303], main = "Distribution of NAs\n 2012", breaks=20)

statsNA(df_ts) # NA Statistics summary
statsNA(df_ts[17520:26303])

statsNA(gw_ts) # NA Statistics summary
statsNA(gw_ts[17520:26303])


#-- 3. Imputation Options
#-- 1)Basic imputation
na.mean(df_ts, option = "mean")
na.mean(df_ts, option = "median") # Median Imputation
na.mean(df_ts, option = "mode")   # Mode Imputation

#-- 2) Replace Missing values by a defined value
na.replace(df_ts, fill = 0)

#-- 3) NA Interpolation
na.l <- na.interpolation(df_ts, option = "linear")
na.p <- na.interpolation(df_ts, option = "spline")
na.t <- na.interpolation(df_ts, option = "stine")

plot(na.l)

#-- 4) NA Kalman: Missing Value Imputation by Kalman Smoothing and State Space Models
# na.k <- na.kalman(df_ts, model = "StructTS", smooth = T) # Don't know why it takes so much time!!

#-- 5) Weighted Moving Average
na.ma(tsAirgap, weighting = "simple")
na.ma(tsAirgap, k = 6, weighting = "simple")
na.ma(df_ts, weighting = "exponential")

#-- 6) Time series imputation
a <- na.random(df_ts)                   # Random Imputation
b <- na.locf(df_ts, option = "locf")   # Last Obs. Carried Forward
c <- na.locf(df_ts, option = "nocb")   # Next Obs. Carried Backward
d <- na.interpolation(df_ts)           # Linear Interpolation

#-- Missing Value Imputation by seasons
seadec   <- na.seadec(df_ts, algorithm = "interpolation")   # Seasonal Adjustment then Linear Interpolation
seasplit <- na.seasplit(df_ts, algorithm = "interpolation") # Seasonally Splitted Missing Value Imputation
season <- as.data.frame(cbind(seadec, seasplit))
#a <- gn[580:600,]


# Test: Year 2012 26303
df_12 <- df_ts[17520:18263] %>%  msts(seasonal.periods = (24) )

gn.locf <- na.locf(df_12, option = "locf")   # Last Obs. Carried Forward
plot(gn.locf, type = "l", main = "LOCF imputation", xlab = "Days", ylab = "PM10")
lines(df_12, col = "blue", lwd = 2)

gn.nocb <- na.locf(df_12, option = "nocb")   # Next Obs. Carried Backward
plot(gn.nocb, type = "l", main = "NOCB imputation", xlab = "Days", ylab = "PM10")
lines(df_12, col = "violet", lwd = 2)

gn.ma <- na.ma(df_12, weighting = "simple")
plot(gn.ma, type = "l", main = "Moving average imputation", xlab = "Days", ylab = "PM10")
lines(df_12, col = "red", lwd = 2)

gn.int <- na.interpolation(df_12)  
plot(gn.int, type = "l", main = "Linear interpolated imputation", xlab = "Days", ylab = "PM10")
lines(df_12, col = "orange", lwd = 2)

#gn.kal <- na.kalman(df_12, algorithm = "interpolation")   # Seasonal Adjustment then Linear Interpolation
#plot(gn.kal, type = "l", main = "Seasonal decomposited imputation", xlab = "Days", ylab = "PM10")
#lines(df_12, col = "khaki2", lwd = 2)

gn.sdec <- na.seadec(df_12, algorithm = "ma")   # Seasonal Adjustment then Linear Interpolation
plot(gn.sdec, type = "l", main = "Seasonal decomposited imputation\nMoving Average", xlab = "Days", ylab = "PM10")
lines(df_12, col = "khaki2", lwd = 2)

gn.sdec <- na.seadec(df_12, algorithm = "interpolation")   # Seasonal Adjustment then Linear Interpolation
plot(gn.sdec, type = "l", main = "Seasonal decomposited imputation\nInterplation", xlab = "Days", ylab = "PM10")
lines(df_12, col = "khaki2", lwd = 2)

gn.sdec <- na.seadec(df_12, algorithm = "kalman")   # Seasonal Adjustment then Linear Interpolation
plot(gn.sdec, type = "l", main = "Seasonal decomposited imputation\nKalman Smoothing", xlab = "Days", ylab = "PM10")
lines(df_12, col = "khaki2", lwd = 2)

gn.sdec <- na.seadec(df_12, algorithm = "mean")   # Seasonal Adjustment then Linear Interpolation
plot(gn.sdec, type = "l", main = "Seasonal decomposited imputation\nRandom", xlab = "Days", ylab = "PM10")
lines(df_12, col = "khaki2", lwd = 2)



#### Seasonal Split #####
#1) Gangnam 
par(mfrow=c(2,2))

gn.12.sma <- na.seasplit(df_12, algorithm = "ma")
plot(gn.12.sma, type = "l", main = "Seasonal splitted imputation\nMoving Average", xlab = "Days", ylab = "PM10")
lines(df_12, col = "khaki1", lwd = 2)

gn.12.sint <- na.seasplit(df_12, algorithm = "interpolation")
plot(gn.12.sint, type = "l", main = "Seasonal splitted imputation\nInterpolation", xlab = "Days", ylab = "PM10")
lines(df_12, col = "khaki2", lwd = 2)

gn.12.skal <- na.seasplit(df_12, algorithm = "kalman")
plot(gn.12.skal, type = "l", main = "Seasonal splitted imputation\nKalman", xlab = "Days", ylab = "PM10")
lines(df_12, col = "khaki3", lwd = 2)

gn.12.smean <- na.seasplit(df_12, algorithm = "mean")
plot(gn.12.smean, type = "l", main = "Seasonal splitted imputation\nMean", xlab = "Days", ylab = "PM10")
lines(df_12, col = "khaki4", lwd = 2)

par(mfrow=c(1,1))


gn.sma <- na.seasplit(df_ts, algorithm = "ma")
plot(gn.sma, type = "l", main = "Seasonal splitted imputation\nMoving Average", xlab = "Days", ylab = "PM10")

gn.sint <- na.seasplit(df_ts, algorithm = "interpolation")
plot(gn.sint, type = "l", main = "Seasonal splitted imputation\nInterpolation", xlab = "Days", ylab = "PM10")

gn.skal <- na.seasplit(df_ts, algorithm = "kalman")
plot(gn.skal, type = "l", main = "Seasonal splitted imputation\nKalman", xlab = "Days", ylab = "PM10")

gn.smean <- na.seasplit(df_ts, algorithm = "mean")
plot(gn.smean, type = "l", main = "Seasonal splitted imputation\nMean", xlab = "Days", ylab = "PM10")

gn.new <- cbind(gn.smean, gn.sma, gn.sint, gn.skal) %>% 
          as.data.frame() %>%
          rename(ts_mean = gn.smean, ts_ma = gn.sma, ts_int = gn.sint, ts_kal = gn.skal)

gn.new <- cbind(gn, gn.new) %>% select(-c(pm10,time1))


#2) Gwanak
gw.sma <- na.seasplit(gw_ts, algorithm = "ma")
plot(gw.sma, type = "l", main = "Seasonal splitted imputation\nMoving Average", xlab = "Days", ylab = "PM10")

gw.sint <- na.seasplit(gw_ts, algorithm = "interpolation")
plot(gw.sint, type = "l", main = "Seasonal splitted imputation\nInterpolation", xlab = "Days", ylab = "PM10")

gw.skal <- na.seasplit(gw_ts, algorithm = "kalman")
plot(gw.skal, type = "l", main = "Seasonal splitted imputation\nKalman", xlab = "Days", ylab = "PM10")

gw.smean <- na.seasplit(gw_ts, algorithm = "mean")
plot(gw.smean, type = "l", main = "Seasonal splitted imputation\nMean", xlab = "Days", ylab = "PM10")

gw.new <- cbind(gw.smean, gw.sma, gw.sint, gw.skal) %>% 
  as.data.frame() %>%
  rename(ts_mean = gw.smean, ts_ma = gw.sma, ts_int = gw.sint, ts_kal = gw.skal)

gw.new <- cbind(gn, gw.new) %>% select(-c(pm10,time1))

#####################################
#-- Clean dataframe with reshape2 --#
#####################################

# 1. Gangnam
gnclean <- reshape2::melt(gn.new, id = c("DATE", "hour"),
                          variable.name = "Type",value.name = "Value") %>% 
           mutate(work = case_when(.$hour >= 9 & .$hour <= 19 ~ "work",
                                   .$hour < 9  | .$hour >  19 ~ "home"),
                  hour = replace(hour, hour == 0, 24)
                 ) 


gncast <- gnclean %>% reshape2::dcast(DATE + Type + work ~ hour + ., value.var = "Value")
df1 = as.data.frame(t(apply(gncast,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(df1) = c("dates", "type","work","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","h12",
                  "h13","h14","h15","h16","h17","h18","h19","h20","h21","h22","h23","h24")
gndf <- df1 %>% select(1:16) #
for(i in c(4:16)){
  gndf[,i] <- as.integer(as.character(gndf[,i]))
}
gndf <- gndf %>% replace_na(list(h12 =-999, h13 = -999)) %>% arrange(type,dates)


# 2. Gwanak
gwclean <- reshape2::melt(gw.new, id = c("DATE", "hour"),
                          variable.name = "Type",value.name = "Value") %>% 
  mutate(work = case_when(.$hour >= 9 & .$hour <= 19 ~ "work",
                          .$hour < 9  | .$hour >  19 ~ "home"),
         hour = replace(hour, hour == 0, 24)
  ) 


gwcast <- gwclean %>% reshape2::dcast(DATE + Type + work ~ hour + ., value.var = "Value")
df2 = as.data.frame(t(apply(gwcast,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
colnames(df2) = c("dates", "type","work","h1","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","h12",
                  "h13","h14","h15","h16","h17","h18","h19","h20","h21","h22","h23","h24")
gwdf <- df2 %>% select(1:16) #
for(i in c(4:16)){
  gwdf[,i] <- as.integer(as.character(gwdf[,i]))
}
gwdf <- gwdf %>% replace_na(list(h12 =-999, h13 = -999)) %>% arrange(type,dates)
#write.csv(gwdf,"gn.csv")



#################################
gn.tib <- as_tibble(gnclean) %>% 
  group_by(Type)

gn.quart <- gn.tib %>%
  tq_transmute(
    select     = Value,
    mutate_fun = apply.quarterly, 
    FUN        = mean
  )

gn.quart$inc10 <- gn.quart$Value * 1.10 
gn.quart$inc20 <- gn.quart$Value * 1.20
gn.quart$inc10sub <- round(gn.quart$inc10 - gn.quart$Value, 2)
gn.quart$inc20sub <- round(gn.quart$inc20 - gn.quart$Value, 2)



gw.tib <- as_tibble(gwclean) %>% 
  group_by(Type)

gw.quart <- gw.tib %>%
  tq_transmute(
    select     = Value,
    mutate_fun = apply.quarterly, 
    FUN        = mean
  )

gw.quart$inc10 <- gw.quart$Value * 1.10 
gw.quart$inc20 <- gw.quart$Value * 1.20
gw.quart$inc10sub <- round(gw.quart$inc10 - gw.quart$Value, 2)
gw.quart$inc20sub <- round(gw.quart$inc20 - gw.quart$Value, 2)

#write.csv(gn.quart,"gnquart.csv")
#write.csv(gw.quart,"gwquart.csv")

