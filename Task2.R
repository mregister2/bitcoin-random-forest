###############################################################################################
# Complete analysis for C722 Capstone project: "Random Forest Regression and Bitcoin Prices."
# 
# Steps include importing asset data, creating technical indicators and querying Google Trends
# data for select keyword searches.
#
# Created by: Mike Register, April 2021.
###############################################################################################

# Set options and load packages
options(scipen = 999, digits = 4)

library(tidyverse)
library(tseries)
library(TTR)
library(corrplot)
library(fpp2)
library(tidymodels)
library(ranger)
library(doParallel)
library(vip)
library(forecast)
library(gridExtra)

# Asset Pricing Data ----


# Load data
BTC_price <- read_csv("coin_Bitcoin.csv")
ETH_price <- read_csv("coin_Ethereum.csv")
TETH_price <- read_csv("coin_Tether.csv")
SP_price <- read_csv("GSPC.csv")
DJI_price <- read_csv("DJI.csv")
NAS_price <-read_csv("IXIC.csv")
Gold_price <-read_csv("GC.csv")
TRE_price <- read_csv("TNX.csv")



#Test for stationarity of variables.

inds <- seq(as.Date("2013-01-01"), as.Date("2021-02-28"), by = "day")

#BTC

#subset data and create time series
BTC_price <- BTC_price %>% select(Date:Marketcap)
Y <- ts(data = BTC_price, start = c(2013, as.numeric(format(inds[1], "%j"))), frequency = 1)
plot(Y)

#autocorrelation graph
acf(Y[,5], main = "Autocorrelation of daily Bitcoin close price", xlab = "")   #Close
acf(Y[,6])   #volume
acf(Y[,7])  #market cap

adf.test(Y[,5], k = 1)

#Difference the Close, VOlume and Market Cap data. 
BTC_price <- BTC_price %>% 
  mutate(BTC_diff = Close - lag(Close, n = 1), Mcap_diff = Marketcap - lag(Marketcap, n=1), 
         Vol_diff = Volume - lag(Volume, n = 1)) %>%
  filter(Date > '2013-04-29')

#Re-run acf on differenced data
Y <- ts(data = BTC_price, start = c(2013, as.numeric(format(inds[1], "%j"))), frequency = 1)

acf(Y[,8], main = "First difference of daily Bitcoin close price", xlab = "")   #Close
acf(Y[,9])   #volume
acf(Y[,10])  #market cap

BTC_price <- select(BTC_price, Date:Close, BTC_diff:Vol_diff)



#Repeat steps for each asset class


#ETH

#subset data and create time series
ETH_price <- ETH_price %>% select(Date:Marketcap)
Y <- ts(data = ETH_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)
plot(Y)

#autocorrelation graph
acf(Y[,5])   #Close
acf(Y[,6])   #volume
acf(Y[,7])  #market cap

#Conclusion: Need to difference the Close and Market Cap data. Volume can be left as is.
ETH_price <- ETH_price %>% 
  mutate(ETH_diff = Close - lag(Close, n = 1), Eth_Mcap_diff = Marketcap - lag(Marketcap, n=1),
         Eth_vol_diff = Volume - lag(Volume, n = 1)) %>%
  filter(Date > '2015-08-08') %>%
  rename(ETH = Close)

#Re-run ADF on differenced data
Y <- ts(data = ETH_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)

acf(Y[,8])   #Close
acf(Y[,9])   #volume
acf(Y[,10])  #market cap

ETH_price <- select(ETH_price, Date, ETH, ETH_diff:Eth_vol_diff)




#TETH - a "stable coin", we are only interested in volume and market cap

#subset data and create time series
TETH_price <- TETH_price %>% select(Date, Volume, Marketcap)
Y <- ts(data = TETH_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)
plot(Y)

#autocorrelation graph
acf(Y[,2])   #volume
acf(Y[,3])  #market cap

TETH_price <- TETH_price %>% 
  mutate(TETH_vol_diff = Volume - lag(Volume, n = 1), TETH_Mcap_diff = Marketcap - lag(Marketcap, n=1)) %>%
  filter(Date > '2015-02-26')

#Re-run ADF on differenced data
Y <- ts(data = TETH_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)

acf(Y[,4])   #volume
acf(Y[,5])  #market cap

TETH_price <- select(TETH_price, Date, TETH_vol_diff, TETH_Mcap_diff)




#SP

#subset data and create time series
SP_price <- SP_price %>% select(Date:Close, Volume)
Y <- ts(data = SP_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)
plot(Y)

#autocorrelation graph
acf(Y[,5])   #Close
acf(Y[,6])   #volume

SP_price <- SP_price %>% 
  mutate(SP_diff = Close - lag(Close, n = 1), SP_vol_diff = Volume - lag(Volume, n = 1)) %>%
  filter(Date > '2015-08-11') %>%
  rename(SP = Close)

#Re-run ADF on differenced data
Y <- ts(data = SP_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)

acf(Y[,7])   #Close
acf(Y[,8])   #volume

SP_price <- select(SP_price, Date, SP, SP_diff, SP_vol_diff)




#DJI

#subset data and create time series
DJI_price <- DJI_price %>% select(Date:Close, Volume)
Y <- ts(data = DJI_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)
plot(Y)

#autocorrelation graph
acf(Y[,5])   #Close
acf(Y[,6])   #volume

DJI_price <- DJI_price %>% 
  mutate(DJI_diff = Close - lag(Close, n = 1), DJI_vol_diff = Volume - lag(Volume, n = 1)) %>%
  filter(Date > '2015-08-10') %>%
  rename(DJI = Close)

#Re-run ADF on differenced data
Y <- ts(data = DJI_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)

acf(Y[,7])   #Close
acf(Y[,8])   #volume

DJI_price <- select(DJI_price, Date, DJI, DJI_diff, DJI_vol_diff)




#NAS
#subset data and create time series
NAS_price <- NAS_price %>% select(Date:Close, Volume)
Y <- ts(data = NAS_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)
plot(Y)

#autocorrelation graph
acf(Y[,5])   #Close
acf(Y[,6])   #volume

NAS_price <- NAS_price %>% 
  mutate(NAS_diff = Close - lag(Close, n = 1), NAS_vol_diff = Volume - lag(Volume, n = 1)) %>%
  filter(Date > '2015-08-10') %>%
  rename(NAS = Close)

#Re-run ADF on differenced data
Y <- ts(data = NAS_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)

acf(Y[,7])   #Close
acf(Y[,8])   #volume

NAS_price <- select(NAS_price, Date, NAS, NAS_diff, NAS_vol_diff)




#Gold
#subset data and create time series
Gold_price <- Gold_price %>% select(Date:Close, Volume)
Y <- ts(data = Gold_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)
plot(Y)

#autocorrelation graph
acf(Y[,5])   #Close
acf(Y[,6])   #volume

Gold_price <- Gold_price %>% 
  mutate(Gold_diff = Close - lag(Close, n = 1), Gold_vol_diff = Volume - lag(Volume, n = 1)) %>%
  filter(Date > '2015-08-10') %>%
  rename(Gold = Close)

#Re-run ADF on differenced data
Y <- ts(data = Gold_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)

acf(Y[,7])   #Close
acf(Y[,8])   #volume

Gold_price <- select(Gold_price, Date, Gold, Gold_diff, Gold_vol_diff)




#TRE
#subset data and create time series
TRE_price <- TRE_price %>% select(Date:Close, Volume)
Y <- ts(data = TRE_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)
plot(Y)

#autocorrelation graph
acf(Y[,5])   #Close


TRE_price <- TRE_price %>% 
  mutate(TRE_diff = Close - lag(Close, n = 1)) %>%
  filter(Date > '2015-08-10') %>%
  rename(TRE = Close)

#Re-run ADF on differenced data
Y <- ts(data = TRE_price, start = c(2015, as.numeric(format(inds[1], "%j"))), frequency = 365.)

acf(Y[,7])   #Close


TRE_price <- select(TRE_price, Date, TRE, TRE_diff)


#Add time-phased variables to Bitcoin data

#xBTCx is next day's difference in Close price. This is our main dependent variable.
BTC_price <- BTC_price %>%
  mutate(xBTCx = lead(BTC_diff, n = 1),
         diff1 = lag(BTC_diff, n = 1))                     #diff1: prev day's closing price difference
         

# Combine asset data 
prices <- list(BTC_price, ETH_price, TETH_price, SP_price, DJI_price, 
               NAS_price, Gold_price, TRE_price) %>% 
  reduce(left_join, by = "Date")

#NA's in SP, DJI, NAS, Gold and treasury represent days market is closed. 
#Replace with last trading day values and 0 for deltas.
prices <- prices %>% 
  fill(DJI, SP, NAS, Gold, TRE, .direction = "down") %>%
  replace(is.na(.), 0)


#Initial filter of days in dataset
prices <- prices %>% filter(Date >= "2016-03-01" & Date < "2021-02-27")


#save prices
write_csv(prices, file = "prices.csv")




#For Google Trends data see Trends.R file




#Create Technical Indicators ----

#Bitcoin

#Simple Moving Average
SMA3 <- SMA(x = prices$Close, n=3)
SMA7 <- SMA(x = prices$Close, n=7)
SMA15 <- SMA(x = prices$Close, n=15)
SMA30 <- SMA(x = prices$Close, n=30)

# SMA3_diff <- diff(SMA3, lag = 1, differences = 1)
# SMA7_diff <- diff(SMA7, lag = 1, differences = 1)
# SMA15_diff <- diff(SMA15, lag = 1, differences = 1)
# SMA30_diff <- diff(SMA30, lag = 1, differences = 1)

#Exponential Moving Average
EMA3 <- EMA(x = prices$Close, n=3)
EMA7 <- EMA(x = prices$Close, n=7)
EMA15 <- EMA(x = prices$Close, n=15)
EMA30 <- EMA(x = prices$Close, n=30)


#Rate of Change
ROC3 <- (ROC(x = prices$Close, n =3, type = "continuous"))*100
ROC7 <- (ROC(x = prices$Close, n =7, type = "continuous"))*100
ROC15 <- (ROC(x = prices$Close, n =15, type = "continuous"))*100
ROC30 <- (ROC(x = prices$Close, n =30, type = "continuous"))*100

#Relative Strength Indicator
RSI_EMA <- RSI(prices$Close, n = 14, EMA)
RSI_SMA <- RSI(prices$Close, n = 14, SMA)

#MACD
macd <- as_tibble(MACD(prices$Close, nFast = 12, nSlow = 26, nSig = 9, percent = F))

macd$macd_Signal <- if_else(lag(macd$macd, k=1) <= macd$signal, if_else(macd$macd >= macd$signal, "BUY", "HOLD"), 
                            if_else(macd$macd < macd$signal, "SELL", "HOLD"))

macd$macd_Signal <- as_factor(macd$macd_Signal)
macd$macd = NULL
macd$signal = NULL

#combine technical indicator data
BTC_TI <- cbind(SMA3, SMA7, SMA15, SMA30, EMA3, EMA7, EMA15, EMA30,
                ROC3, ROC7, ROC15, ROC30, RSI_EMA, RSI_SMA, macd)




#ETHEREUM

SMA3_ETH <- SMA(x = prices$ETH, n=3)
SMA7_ETH <- SMA(x = prices$ETH, n=7)
SMA15_ETH <- SMA(x = prices$ETH, n=15)
SMA30_ETH <- SMA(x = prices$ETH, n=30)

EMA3_ETH <- EMA(x = prices$ETH, n=3)
EMA7_ETH <- EMA(x = prices$ETH, n=7)
EMA15_ETH <- EMA(x = prices$ETH, n=15)
EMA30_ETH <- EMA(x = prices$ETH, n=30)

ROC3_ETH <- (ROC(x = prices$ETH, n =3, type = "continuous"))*100
ROC7_ETH <- (ROC(x = prices$ETH, n =7, type = "continuous"))*100
ROC15_ETH <- (ROC(x = prices$ETH, n =15, type = "continuous"))*100
ROC30_ETH <- (ROC(x = prices$ETH, n =30, type = "continuous"))*100

RSI_EMA_ETH <- RSI(prices$ETH, n = 14, EMA)
RSI_SMA_ETH <- RSI(prices$ETH, n = 14, SMA)

macd_ETH <- as_tibble(MACD(prices$ETH, nFast = 12, nSlow = 26, nSig = 9, percent = F))

macd_ETH$macd_Signal_ETH <- if_else(lag(macd_ETH$macd, k=1) <= macd_ETH$signal, if_else(macd_ETH$macd >= macd_ETH$signal, 
                                                                                        "BUY", "HOLD"), 
                                    if_else(macd_ETH$macd < macd_ETH$signal, "SELL", "HOLD"))

macd_ETH$macd_Signal_ETH <- as_factor(macd_ETH$macd_Signal_ETH)
macd_ETH$macd = NULL
macd_ETH$signal = NULL

#combine technical indicator data
ETH_TI <- cbind(SMA3_ETH, SMA7_ETH, SMA15_ETH, SMA30_ETH, EMA3_ETH, EMA7_ETH,
                EMA15_ETH, EMA30_ETH, ROC3_ETH, ROC7_ETH, ROC15_ETH, ROC30_ETH, RSI_EMA_ETH, 
                RSI_SMA_ETH, macd_ETH)



#DJI

SMA3_DJI <- SMA(x = prices$DJI, n=3)
SMA7_DJI <- SMA(x = prices$DJI, n=7)
SMA15_DJI <- SMA(x = prices$DJI, n=15)
SMA30_DJI <- SMA(x = prices$DJI, n=30)

EMA3_DJI <- EMA(x = prices$DJI, n=3)
EMA7_DJI <- EMA(x = prices$DJI, n=7)
EMA15_DJI <- EMA(x = prices$DJI, n=15)
EMA30_DJI <- EMA(x = prices$DJI, n=30)

ROC3_DJI <- (ROC(x = prices$DJI, n =3, type = "continuous"))*100
ROC7_DJI <- (ROC(x = prices$DJI, n =7, type = "continuous"))*100
ROC15_DJI <- (ROC(x = prices$DJI, n =15, type = "continuous"))*100
ROC30_DJI <- (ROC(x = prices$DJI, n =30, type = "continuous"))*100

RSI_EMA_DJI <- RSI(prices$DJI, n = 14, EMA)
RSI_SMA_DJI <- RSI(prices$DJI, n = 14, SMA)

macd_DJI <- as_tibble(MACD(prices$DJI, nFast = 12, nSlow = 26, nSig = 9, percent = F))

macd_DJI$macd_Signal_DJI <- if_else(lag(macd_DJI$macd, k=1) <= macd_DJI$signal, if_else(macd_DJI$macd >= macd_DJI$signal, 
                                                                                        "BUY", "HOLD"), 
                                    if_else(macd_DJI$macd < macd_DJI$signal, "SELL", "HOLD"))

macd_DJI$macd_Signal_DJI <- as_factor(macd_DJI$macd_Signal_DJI)
macd_DJI$macd = NULL
macd_DJI$signal = NULL


#combine technical indicator data
DJI_TI <- cbind(SMA3_DJI, SMA7_DJI, SMA15_DJI, SMA30_DJI, EMA3_DJI, EMA7_DJI, 
                EMA15_DJI, EMA30_DJI, ROC3_DJI, ROC7_DJI, ROC15_DJI, ROC30_DJI, RSI_EMA_DJI, 
                RSI_SMA_DJI, macd_DJI)



#SP
SMA3_SP <- SMA(x = prices$SP, n=3)
SMA7_SP <- SMA(x = prices$SP, n=7)
SMA15_SP <- SMA(x = prices$SP, n=15)
SMA30_SP <- SMA(x = prices$SP, n=30)

EMA3_SP <- EMA(x = prices$SP, n=3)
EMA7_SP <- EMA(x = prices$SP, n=7)
EMA15_SP <- EMA(x = prices$SP, n=15)
EMA30_SP <- EMA(x = prices$SP, n=30)

ROC3_SP <- (ROC(x = prices$SP, n =3, type = "continuous"))*100
ROC7_SP <- (ROC(x = prices$SP, n =7, type = "continuous"))*100
ROC15_SP <- (ROC(x = prices$SP, n =15, type = "continuous"))*100
ROC30_SP <- (ROC(x = prices$SP, n =30, type = "continuous"))*100

RSI_EMA_SP <- RSI(prices$SP, n = 14, EMA)
RSI_SMA_SP <- RSI(prices$SP, n = 14, SMA)

macd_SP <- as_tibble(MACD(prices$SP, nFast = 12, nSlow = 26, nSig = 9, percent = F))

macd_SP$macd_Signal_SP <- if_else(lag(macd_SP$macd, k=1) <= macd_SP$signal, if_else(macd_SP$macd >= macd_SP$signal, 
                                                                                    "BUY", "HOLD"), 
                                  if_else(macd_SP$macd < macd_SP$signal, "SELL", "HOLD"))

macd_SP$macd_Signal_SP <- as_factor(macd_SP$macd_Signal_SP)
macd_SP$macd = NULL
macd_SP$signal = NULL


#combine technical indicator data
SP_TI <- cbind(SMA3_SP, SMA7_SP, SMA15_SP, SMA30_SP, EMA3_SP, EMA7_SP, 
               EMA15_SP, EMA30_SP, ROC3_SP, ROC7_SP, ROC15_SP, ROC30_SP, RSI_EMA_SP, RSI_SMA_SP, macd_SP)


#NAS
SMA3_NAS <- SMA(x = prices$NAS, n=3)
SMA7_NAS <- SMA(x = prices$NAS, n=7)
SMA15_NAS <- SMA(x = prices$NAS, n=15)
SMA30_NAS <- SMA(x = prices$NAS, n=30)

EMA3_NAS <- EMA(x = prices$NAS, n=3)
EMA7_NAS <- EMA(x = prices$NAS, n=7)
EMA15_NAS <- EMA(x = prices$NAS, n=15)
EMA30_NAS <- EMA(x = prices$NAS, n=30)

ROC3_NAS <- (ROC(x = prices$NAS, n =3, type = "continuous"))*100
ROC7_NAS <- (ROC(x = prices$NAS, n =7, type = "continuous"))*100
ROC15_NAS <- (ROC(x = prices$NAS, n =15, type = "continuous"))*100
ROC30_NAS <- (ROC(x = prices$NAS, n =30, type = "continuous"))*100

RSI_EMA_NAS <- RSI(prices$NAS, n = 14, EMA)
RSI_SMA_NAS <- RSI(prices$NAS, n = 14, SMA)

macd_NAS <- as_tibble(MACD(prices$NAS, nFast = 12, nSlow = 26, nSig = 9, percent = F))

macd_NAS$macd_Signal_NAS <- if_else(lag(macd_NAS$macd, k=1) <= macd_NAS$signal, if_else(macd_NAS$macd >= macd_NAS$signal, 
                                                                                        "BUY", "HOLD"), 
                                    if_else(macd_NAS$macd < macd_NAS$signal, "SELL", "HOLD"))

macd_NAS$macd_Signal_NAS <- as_factor(macd_NAS$macd_Signal_NAS)
macd_NAS$macd = NULL
macd_NAS$signal = NULL

#combine technical indicator data
NAS_TI <- cbind(SMA3_NAS, SMA7_NAS, SMA15_NAS, SMA30_NAS, EMA3_NAS, EMA7_NAS, 
                EMA15_NAS, EMA30_NAS, ROC3_NAS, ROC7_NAS, ROC15_NAS, ROC30_NAS, RSI_EMA_NAS, RSI_SMA_NAS, macd_NAS)



#Gold
SMA3_Gold <- SMA(x = prices$Gold, n=3)
SMA7_Gold <- SMA(x = prices$Gold, n=7)
SMA15_Gold <- SMA(x = prices$Gold, n=15)
SMA30_Gold <- SMA(x = prices$Gold, n=30)

EMA3_Gold <- EMA(x = prices$Gold, n=3)
EMA7_Gold <- EMA(x = prices$Gold, n=7)
EMA15_Gold <- EMA(x = prices$Gold, n=15)
EMA30_Gold <- EMA(x = prices$Gold, n=30)

ROC3_Gold <- (ROC(x = prices$Gold, n =3, type = "continuous"))*100
ROC7_Gold <- (ROC(x = prices$Gold, n =7, type = "continuous"))*100
ROC15_Gold <- (ROC(x = prices$Gold, n =15, type = "continuous"))*100
ROC30_Gold <- (ROC(x = prices$Gold, n =30, type = "continuous"))*100

RSI_EMA_Gold <- RSI(prices$Gold, n = 14, EMA)
RSI_SMA_Gold <- RSI(prices$Gold, n = 14, SMA)

macd_Gold <- as_tibble(MACD(prices$Gold, nFast = 12, nSlow = 26, nSig = 9, percent = F))

macd_Gold$macd_Signal_Gold <- if_else(lag(macd_Gold$macd, k=1) <= macd_Gold$signal, if_else(macd_Gold$macd >= macd_Gold$signal, 
                                                                                            "BUY", "HOLD"), 
                                      if_else(macd_Gold$macd < macd_Gold$signal, "SELL", "HOLD"))

macd_Gold$macd_Signal_Gold <- as_factor(macd_Gold$macd_Signal_Gold)
macd_Gold$macd = NULL
macd_Gold$signal = NULL


#combine technical indicator data
Gold_TI <- cbind(SMA3_Gold, SMA7_Gold, SMA15_Gold, SMA30_Gold, EMA3_Gold, EMA7_Gold, 
                 EMA15_Gold, EMA30_Gold, ROC3_Gold, ROC7_Gold, ROC15_Gold, ROC30_Gold, RSI_EMA_Gold, 
                 RSI_SMA_Gold, macd_Gold)


#TRE
SMA3_TRE <- SMA(x = prices$TRE, n=3)
SMA7_TRE <- SMA(x = prices$TRE, n=7)
SMA15_TRE <- SMA(x = prices$TRE, n=15)
SMA30_TRE <- SMA(x = prices$TRE, n=30)

EMA3_TRE <- EMA(x = prices$TRE, n=3)
EMA7_TRE <- EMA(x = prices$TRE, n=7)
EMA15_TRE <- EMA(x = prices$TRE, n=15)
EMA30_TRE <- EMA(x = prices$TRE, n=30)

ROC3_TRE <- (ROC(x = prices$TRE, n =3, type = "continuous"))*100
ROC7_TRE <- (ROC(x = prices$TRE, n =7, type = "continuous"))*100
ROC15_TRE <- (ROC(x = prices$TRE, n =15, type = "continuous"))*100
ROC30_TRE <- (ROC(x = prices$TRE, n =30, type = "continuous"))*100

RSI_EMA_TRE <- RSI(prices$TRE, n = 14, EMA)
RSI_SMA_TRE <- RSI(prices$TRE, n = 14, SMA)

macd_TRE <- as_tibble(MACD(prices$TRE, nFast = 12, nSlow = 26, nSig = 9, percent = F))

macd_TRE$macd_Signal_TRE <- if_else(lag(macd_TRE$macd, k=1) <= macd_TRE$signal, if_else(macd_TRE$macd >= macd_TRE$signal, 
                                                                                        "BUY", "HOLD"), 
                                    if_else(macd_TRE$macd < macd_TRE$signal, "SELL", "HOLD"))

macd_TRE$macd_Signal_TRE <- as_factor(macd_TRE$macd_Signal_TRE)
macd_TRE$macd = NULL
macd_TRE$signal = NULL

#combine technical indicator data
TRE_TI <- cbind(SMA3_TRE, SMA7_TRE, SMA15_TRE, SMA30_TRE, EMA3_TRE, EMA7_TRE, 
                EMA15_TRE, EMA30_TRE, ROC3_TRE, ROC7_TRE, ROC15_TRE, ROC30_TRE, RSI_EMA_TRE, RSI_SMA_TRE, macd_TRE)


#Combine all technical indicators for all assets
Date <- prices[,1]
tech <- cbind(Date, BTC_TI, ETH_TI, DJI_TI, SP_TI, NAS_TI, Gold_TI, TRE_TI)


#Difference SMA and EMA data
tech <- tech %>%
  mutate(SMA3x = SMA3 - lag(SMA3, n = 1),
         SMA7x = SMA7 - lag(SMA7, n = 1),
         SMA15x = SMA15 - lag(SMA15, n = 1),
         SMA30x = SMA30 - lag(SMA30, n = 1),
         EMA3x = EMA3 - lag(EMA3, n = 1),
         EMA7x = EMA7 - lag(EMA7, n = 1),
         EMA15x = EMA15 - lag(EMA15, n = 1),
         EMA30x = EMA30 - lag(EMA30, n = 1),
         
         SMA3x_ETH = SMA3_ETH - lag(SMA3_ETH, n = 1),
         SMA7x_ETH = SMA7_ETH - lag(SMA7_ETH, n = 1),
         SMA15x_ETH = SMA15_ETH - lag(SMA15_ETH, n = 1),
         SMA30x_ETH = SMA30_ETH - lag(SMA30_ETH, n = 1),
         EMA3x_ETH = EMA3_ETH - lag(EMA3_ETH, n = 1),
         EMA7x_ETH = EMA7_ETH - lag(EMA7_ETH, n = 1),
         EMA15x_ETH = EMA15_ETH - lag(EMA15_ETH, n = 1),
         EMA30x_ETH = EMA30_ETH - lag(EMA30_ETH, n = 1),
         
         SMA3x_DJI = SMA3_DJI - lag(SMA3_DJI, n = 1),
         SMA7x_DJI = SMA7_DJI - lag(SMA7_DJI, n = 1),
         SMA15x_DJI = SMA15_DJI - lag(SMA15_DJI, n = 1),
         SMA30x_DJI = SMA30_DJI - lag(SMA30_DJI, n = 1),
         EMA3x_DJI = EMA3_DJI - lag(EMA3_DJI, n = 1),
         EMA7x_DJI = EMA7_DJI - lag(EMA7_DJI, n = 1),
         EMA15x_DJI = EMA15_DJI - lag(EMA15_DJI, n = 1),
         EMA30x_DJI = EMA30_DJI - lag(EMA30_DJI, n = 1),
         
         SMA3x_SP = SMA3_SP - lag(SMA3_SP, n = 1),
         SMA7x_SP = SMA7_SP - lag(SMA7_SP, n = 1),
         SMA15x_SP = SMA15_SP - lag(SMA15_SP, n = 1),
         SMA30x_SP = SMA30_SP - lag(SMA30_SP, n = 1),
         EMA3x_SP = EMA3_SP - lag(EMA3_SP, n = 1),
         EMA7x_SP = EMA7_SP - lag(EMA7_SP, n = 1),
         EMA15x_SP = EMA15_SP - lag(EMA15_SP, n = 1),
         EMA30x_SP = EMA30_SP - lag(EMA30_SP, n = 1),
         
         SMA3x_NAS = SMA3_NAS - lag(SMA3_NAS, n = 1),
         SMA7x_NAS = SMA7_NAS - lag(SMA7_NAS, n = 1),
         SMA15x_NAS = SMA15_NAS - lag(SMA15_NAS, n = 1),
         SMA30x_NAS = SMA30_NAS - lag(SMA30_NAS, n = 1),
         EMA3x_NAS = EMA3_NAS - lag(EMA3_NAS, n = 1),
         EMA7x_NAS = EMA7_NAS - lag(EMA7_NAS, n = 1),
         EMA15x_NAS = EMA15_NAS - lag(EMA15_NAS, n = 1),
         EMA30x_NAS = EMA30_NAS - lag(EMA30_NAS, n = 1),
         
         SMA3x_Gold = SMA3_Gold - lag(SMA3_Gold, n = 1),
         SMA7x_Gold = SMA7_Gold - lag(SMA7_Gold, n = 1),
         SMA15x_Gold = SMA15_Gold - lag(SMA15_Gold, n = 1),
         SMA30x_Gold = SMA30_Gold - lag(SMA30_Gold, n = 1),
         EMA3x_Gold = EMA3_Gold - lag(EMA3_Gold, n = 1),
         EMA7x_Gold = EMA7_Gold - lag(EMA7_Gold, n = 1),
         EMA15x_Gold = EMA15_Gold - lag(EMA15_Gold, n = 1),
         EMA30x_Gold = EMA30_Gold - lag(EMA30_Gold, n = 1),
         
         SMA3x_TRE = SMA3_TRE - lag(SMA3_TRE, n = 1),
         SMA7x_TRE = SMA7_TRE - lag(SMA7_TRE, n = 1),
         SMA15x_TRE = SMA15_TRE - lag(SMA15_TRE, n = 1),
         SMA30x_TRE = SMA30_TRE - lag(SMA30_TRE, n = 1),
         EMA3x_TRE = EMA3_TRE - lag(EMA3_TRE, n = 1),
         EMA7x_TRE = EMA7_TRE - lag(EMA7_TRE, n = 1),
         EMA15x_TRE = EMA15_TRE - lag(EMA15_TRE, n = 1),
         EMA30x_TRE = EMA30_TRE - lag(EMA30_TRE, n = 1),
         
         )

tech <- tech %>%
  select(Date, ROC3:macd_Signal, ROC3_ETH:macd_Signal_ETH, ROC3_DJI:macd_Signal_DJI, ROC3_SP:macd_Signal_SP,
         ROC3_NAS:macd_Signal_NAS, ROC3_Gold:macd_Signal_Gold, ROC3_TRE:EMA30x_TRE )

#save
write_csv(tech, file = "tech.csv")



#Combine all Price, Trends and Tech data into one file ----


trends <- read_csv("trends.csv")

BTC_data <- list(prices, tech, trends) %>% reduce(left_join, by = "Date")
BTC_data$High = NULL
BTC_data$Low = NULL
BTC_data$Open = NULL
BTC_data$Close = NULL

BTC_data <- BTC_data %>% 
  relocate(xBTCx, diff1, .after = Date) %>%
  relocate(macd_Signal, macd_Signal_DJI, macd_Signal_ETH, macd_Signal_SP, 
           macd_Signal_NAS, macd_Signal_Gold,
           macd_Signal_TRE, .after = stocksHitsChg) 

# save data
write_csv(BTC_data, file = "BTC_data2016.csv")

#filter and save
BTC_data <- filter(BTC_data, Date >= "2019-03-01")
BTC_data$X = NULL
BTC_data$ETH = NULL
BTC_data$SP = NULL
BTC_data$DJI = NULL
BTC_data$Gold = NULL
BTC_data$NAS = NULL
BTC_data$TRE = NULL
BTC_data$BTCHits = NULL
BTC_data$BuyBTCHits = NULL
BTC_data$WhatBTCHits = NULL
BTC_data$WalletHits = NULL
BTC_data$ETHHits = NULL
BTC_data$BuyEthHits = NULL
BTC_data$cryptoHits = NULL
BTC_data$blockHits = NULL
BTC_data$HODLHits = NULL
BTC_data$altcoinHits = NULL
BTC_data$SatoshiHits = NULL
BTC_data$dipHits = NULL
BTC_data$poolHits = NULL
BTC_data$investHits = NULL
BTC_data$richHits = NULL
BTC_data$recessionHits = NULL
BTC_data$debtHits = NULL
BTC_data$bearHits = NULL
BTC_data$bullHits = NULL
BTC_data$stocksHits = NULL

write_csv(BTC_data, file = "BTC_data.csv")




#Perform Correlation Analysis against xBTCx ----

Corr_Matrix <- cor(BTC_data[,c(2,3:50)])
corrplot(Corr_Matrix, method = "circle", type= "lower" )

Corr_Matrix2 <- cor(BTC_data[,c(2,51:93)])
corrplot(Corr_Matrix2, method = "circle", type= "lower" )

Corr_Matrix3 <- cor(BTC_data[,c(2,94:138)])
corrplot(Corr_Matrix3, method = "circle", type= "lower" )

corr1<- as_tibble(colnames(Corr_Matrix))
corr1 <- cbind(corr1, Corr_Matrix)
corr1 <- corr1[,1:2]

corr2<- as_tibble(colnames(Corr_Matrix2))
corr2 <- cbind(corr2, Corr_Matrix2)
corr2 <- corr2[,1:2]

corr3<- as_tibble(colnames(Corr_Matrix3))
corr3 <- cbind(corr3, Corr_Matrix3)
corr3 <- corr3[,1:2]

Corr_Matrix4 <- cor(BTC_data[,c(2,119:138)])
corrplot(Corr_Matrix4, method = "circle", type= "lower", mar = c(0,0,2,0))

#correlations with categorical variables

#with macd_signal
macd_signal_lm <- lm(BTC_data$xBTCx ~ BTC_data$macd_Signal)
macd_summ <-summary(macd_signal_lm)
macd_cor <- sqrt(macd_summ$r.squared)
macd_cor

#with macd_signal_ETH
macd_eth_lm <- lm(BTC_data$xBTCx ~ BTC_data$macd_Signal_ETH)
macd_eth_summ <-summary(macd_eth_lm)
macd_eth_cor <- sqrt(macd_eth_summ$r.squared)
macd_eth_cor

#with macd_signal_DJI
macd_DJI_lm <- lm(BTC_data$xBTCx ~ BTC_data$macd_Signal_DJI)
macd_DJI_summ <-summary(macd_DJI_lm)
macd_DJI_cor <- sqrt(macd_DJI_summ$r.squared)
macd_DJI_cor

#with macd_signal_SP
macd_SP_lm <- lm(BTC_data$xBTCx ~ BTC_data$macd_Signal_SP)
macd_SP_summ <-summary(macd_SP_lm)
macd_SP_cor <- sqrt(macd_SP_summ$r.squared)
macd_SP_cor

#with macd_signal_NAS
macd_NAS_lm <- lm(BTC_data$xBTCx ~ BTC_data$macd_Signal_NAS)
macd_NAS_summ <-summary(macd_NAS_lm)
macd_NAS_cor <- sqrt(macd_NAS_summ$r.squared)
macd_NAS_cor

#with macd_signal_Gold
macd_gold_lm <- lm(BTC_data$xBTCx ~ BTC_data$macd_Signal_Gold)
macd_gold_summ <-summary(macd_gold_lm)
macd_gold_cor <- sqrt(macd_gold_summ$r.squared)
macd_gold_cor

#with macd_signal_TRE
macd_tre_lm <- lm(BTC_data$xBTCx ~ BTC_data$macd_Signal_TRE)
macd_tre_summ <-summary(macd_tre_lm)
macd_tre_cor <- sqrt(macd_tre_summ$r.squared)
macd_tre_cor

#combine correlation data into a single tibble
cat_cor <- as_tibble(cbind(macd_cor, macd_DJI_cor, macd_eth_cor, macd_gold_cor, macd_NAS_cor,
                           macd_SP_cor, macd_tre_cor))
cat_cor <- cat_cor %>%
  pivot_longer(cols = starts_with("macd"),
               names_to = "value",
               values_to = "xBTCx")

correlations <- rbind(corr1, corr2, corr3, cat_cor)

correlations <- correlations %>%
  rename(Variable = value) %>%
  arrange(desc(xBTCx)) %>%
  slice(-(1:3))

ggplot(data = correlations, mapping = aes(x = Variable, y = xBTCx), fill = Variable ) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_blank(),
        axis.ticks = element_blank()) +
  ylab("Correlation coefficient") +
  xlab("Independent Variables") +
  ggtitle("Correlation Coefficients with dependent variable")

#downloaded correlation data and added types. Re-do plot with color = Type
corr_types <- read_csv("corr_types.csv")

ggplot(data = corr_types, mapping = aes(x = Variable, y = xBTCx, fill = Data) ) +
  geom_bar(stat = "identity") +
  labs(
    title = "Correlation coefficients with dependent variable",
    x = "Independent Variables",
    y = "Correlation coefficient"
  ) +
  theme(axis.text.x = element_blank(),
      axis.ticks = element_blank()) 


#Conclusion: No variables are strongly correlated with the dependent variable. ----
# Remove variables with weakest correlations and variables highly correlated with
# other independent variables.
#
# Removing SP, DJI due to correlation with NAS. GOld and Treasury have little to
# no correlation with BTC. Removing SMA due to correlation with EMA, ROC due to
# correlation with RSI.
# Of trend variables, all of them have weak or very little correlation with BTC.
# Kept the strongest one, "WalletHitsChg."


BTC_data2 <- BTC_data %>%
  select(Date:BTC_diff, Vol_diff:ETH_diff, Eth_vol_diff:TETH_Mcap_diff, NAS_diff:NAS_vol_diff, RSI_EMA, 
         RSI_EMA_ETH, RSI_EMA_NAS, EMA3x:EMA30x, EMA3x_ETH:EMA30x_ETH, EMA3x_NAS:EMA30x_NAS,
         WalletHitsChg, macd_Signal, macd_Signal_ETH, macd_Signal_NAS)

#Correlation matrix of subsetted data

Corr_Matrix4 <- cor(BTC_data2[,c(2:27)])
corrplot(Corr_Matrix4, method = "circle", type= "lower" )

#save
write_csv(BTC_data2, "BTC_data2.csv")


# Model Building ----

#create data sets for building the models.

BTC_data <- read_csv("BTC_data.csv")  #145 variables
BTC_data <- BTC_data %>% mutate_if(is.character, as.factor)

BTC_data2 <- read_csv("BTC_data2.csv")    #30 "best" variables
BTC_data2 <- BTC_data2 %>% mutate_if(is.character, as.factor)

BTC_data3 <- BTC_data2 %>%
  select(Date:BTC_diff, RSI_EMA, EMA3x:EMA30x)               #BTC price variables only

BTC_data4 <- read_csv("prices.csv")                    #for the naive model
BTC_data4 <- BTC_data4 %>%
  select(Date, Close) %>%
  filter(Date >= "2019-02-28")



# # # create the baseline naive model off of just the price of BTC # # # ----


# Create a daily Date object 
inds <- seq(as.Date("2019-02-28"), as.Date("2021-02-28"), by = "day")

# Create a time series object
Y <- ts(data = BTC_data4[,2], start = c(2019, as.numeric(format(inds[1], "%j"))), frequency = 365.)

autoplot(Y)
acf(Y)



# Transform daily. Take the first difference of the data (change in daily price)
DY <- diff(Y)

autoplot(DY)
acf(DY)

# Split data into training and test
DY.train <- window(DY, end = c(2020, 240))
DY.test <- window(DY, start = c(2020, 241))

# run the naive model
naive <- naive(DY.train, h = 183)

# plot the model
plot(naive)
lines(DY.test, col = 7, lty = 1, lwd = 1)

#check accuracy of model


DY.metrics <- as_tibble(forecast::accuracy(naive, DY.test)) %>%
  add_column(Model = "Naive", .before = "ME") %>%
  add_column(Data = c("Train", "Test"), .before = "ME")

DY.metrics

# save model metrics
write_csv(DY.metrics, "DY_Metrics.csv")

#  The naive model has an RMSE of 499 vs Training data and 1323 vs Testing data



# # # Trying out the decompose function ----
plot(decompose(Y))
plot(decompose(Y, type = "multiplicative"))

DecomposedY <- decompose(Y, type = "multiplicative")
plot(DecomposedY$seasonal)
plot(DecomposedY$trend)
# # # # # # # # # # # # # #

# # # Build first Random Forest model with full data set. ----

# Split data into training and testing. First 75% of observations in chronological order 
# make up the training set.

set.seed(123)

data_split <- initial_time_split(BTC_data, prop = 3/4)
data_train <- training(data_split)
data_test <- testing(data_split)

# Visualize data split
split_plot <- data_train %>%
  select(Date, xBTCx) %>%
  add_column(Type = "train")

splot_plot2 <- data_test %>%
  select(Date, xBTCx) %>%
  add_column(Type = "test")

split_plot <- bind_rows(split_plot, splot_plot2)

ggplot(split_plot, aes(x = Date, y = xBTCx)) +
  geom_line(aes(color = Type)) +
  ggtitle("Split of data between training and testing")

# Create cross validation set using rolling_origin
set.seed(234)

data_folds <- rolling_origin(data = data_train, initial = 446, assess = 1, cumulative = FALSE)

data_folds

# Create recipe for data preprocessing
btc_recipe <- recipe(formula = xBTCx ~ ., data = data_train) %>%
  update_role(Date, new_role = "ID") 

btc_recipe

btc_prep <- prep(btc_recipe)

summary(btc_prep)

juiced <- juice(btc_prep)

# Create model specification and tune mtry and min-n

btc_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

# Put together into a workflow

btc_wf <- 
  workflow() %>% 
  add_recipe(btc_recipe) %>% 
  add_model(btc_spec) 

# Train hyperparameters

set.seed(345)

doParallel::registerDoParallel(cores = 5)

btc_tune <-
  tune_grid(btc_wf, resamples = data_folds, grid = 10)

# Create metrics

autoplot(btc_tune)

#choose the best model

best_rmse <- select_best(btc_tune, metric = "rmse")

rmse <-show_best(btc_tune, metric = "rmse")

rmse

final_rf <- finalize_model(
  btc_spec,
  best_rmse
)

final_rf

#evaluate final model

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(xBTCx ~ ., data = juice(btc_prep) %>% 
        select(-Date)) %>%
  vip(geom = "point")

#make a final workflow and fit on entire train set/evaluate on test

final_wf <- workflow() %>%
  add_recipe(btc_recipe) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(data_split)

rf_metrics <- final_res %>%
  collect_metrics() %>%
  mutate(Model = "RF_145", Data = "Test") %>%
  relocate(Model, Data, .before = .metric)

rf_metrics

rf_test_pred <- final_res %>% 
  collect_predictions() 

rf_test_pred

rf_test_pred%>%
  ggplot(aes(x = .row, y = .pred)) +
  geom_line(color = "blue", lwd = 1.25) +
  geom_line(aes(y = xBTCx))


# # # The random forest model with all variables has an RMSE of 363 on Training and 1323 on Testing data. Mtry = 5, min_n = 34


# # # Build Random Forest with smaller data set (30 variables) ----

set.seed(123)

data_split2 <- initial_time_split(BTC_data2, prop = 3/4)
data_train2 <- training(data_split2)
data_test2 <- testing(data_split2)

# Create cross validation set using rolling_origin
set.seed(234)

data_folds2 <- rolling_origin(data = data_train2, initial = 446, assess = 1, cumulative = FALSE)

data_folds2

# Create recipe for data preprocessing
btc_recipe2 <- recipe(formula = xBTCx ~ ., data = data_train2) %>%
  update_role(Date, new_role = "ID") 
  
btc_recipe2

btc_prep2 <- prep(btc_recipe2)

summary(btc_prep2)

juiced <- juice(btc_prep2)

# Create model specification and tune mtry and min-n

btc_spec2 <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

# Put together into a workflow

btc_wf2 <- 
  workflow() %>% 
  add_recipe(btc_recipe2) %>% 
  add_model(btc_spec2) 

# Train hyperparameters

set.seed(345)

btc_tune2 <-
  tune_grid(btc_wf2, resamples = data_folds2, grid = 10)


# Create metrics

autoplot(btc_tune2)

#choose the best model

best_rmse2 <- select_best(btc_tune2, metric = "rmse")

rmse2 <-show_best(btc_tune2, metric = "rmse")

rmse2

final_rf2 <- finalize_model(
  btc_spec2,
  best_rmse2
)

final_rf2

#evaluate final model

final_rf2 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(xBTCx ~ ., data = juice(btc_prep2) %>% 
        select(-Date)) %>%
  vip(geom = "point")

#make a final workflow and fit on entire train set/evaluate on test

final_wf2 <- workflow() %>%
  add_recipe(btc_recipe2) %>%
  add_model(final_rf2)

final_res2 <- final_wf2 %>%
  last_fit(data_split2)

rf_metrics2 <- final_res2 %>%
  collect_metrics() %>%
  mutate(Model = "RF_30", Data = "Test") %>%
  relocate(Model, Data, .before = .metric)

rf_metrics2

rf_test_pred2 <- final_res2 %>% 
  collect_predictions() 

rf_test_pred2

rf_test_pred2%>%
  ggplot(aes(x = .row, y = .pred)) +
  geom_point(color = "blue") +
  geom_line(aes(y = xBTCx))

# # # The random forest model with 30 variables has an RMSE of 169 on Training and 1316 on Testing data. mtry = 2 min_n=34




# # # Build Random Forest with BTC price data only (9 variables)

set.seed(123)

data_split3 <- initial_time_split(BTC_data3, prop = 3/4)
data_train3 <- training(data_split3)
data_test3 <- testing(data_split3)

# Create cross validation set using rolling_origin
set.seed(234)

data_folds3 <- rolling_origin(data = data_train3, initial = 446, assess = 1, cumulative = FALSE)

data_folds3

# Create recipe for data preprocessing
btc_recipe3 <- recipe(formula = xBTCx ~ ., data = data_train3) %>%
  update_role(Date, new_role = "ID") 

btc_recipe3

btc_prep3 <- prep(btc_recipe3)

summary(btc_prep3)

juiced <- juice(btc_prep3)

# Create model specification and tune mtry and min-n

btc_spec3 <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

# Put together into a workflow

btc_wf3 <- 
  workflow() %>% 
  add_recipe(btc_recipe3) %>% 
  add_model(btc_spec3) 

# Train hyperparameters

set.seed(345)

doParallel::registerDoParallel(cores = 5)

btc_tune3 <-
  tune_grid(btc_wf3, resamples = data_folds3, grid = 10)


# Create metrics

autoplot(btc_tune3)

#choose the best model

best_rmse3 <- select_best(btc_tune3, metric = "rmse")

rmse3 <-show_best(btc_tune3, metric = "rmse")

rmse3

final_rf3 <- finalize_model(
  btc_spec3,
  best_rmse3
)

final_rf3

#evaluate final model

final_rf3 %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(xBTCx ~ ., data = juice(btc_prep3) %>% 
        select(-Date)) %>%
  vip(geom = "point")

#make a final workflow and fit on entire train set/evaluate on test

final_wf3 <- workflow() %>%
  add_recipe(btc_recipe3) %>%
  add_model(final_rf3)

final_res3 <- final_wf3 %>%
  last_fit(data_split3)

rf_metrics3 <- final_res3 %>%
  collect_metrics() %>%
  mutate(Model = "RF_9", Data = "Test") %>%
  relocate(Model, Data, .before = .metric)

rf_metrics3

rf_test_pred3 <- final_res3 %>% 
  collect_predictions() 

rf_test_pred3

rf_test_pred3%>%
  ggplot(aes(x = .row, y = .pred)) +
  geom_line(color = "blue", lwd = 1.25) +
  geom_line(aes(y = xBTCx))


# # # The random forest model with 9 variables has an RMSE of 172 on Training and 1355 on Testing data. mtry = 1 min_n = 34


all_metrics <- bind_rows(rf_metrics, rf_metrics2, rf_metrics3)

rmseX <- rmse%>%
  mutate(Model = "RF_145", Data = "Train") %>%
  relocate(Model, Data, .before = mtry)


rmse2X <- rmse2%>%
  mutate(Model = "RF_30", Data = "Train") %>%
  relocate(Model, Data, .before = mtry)


rmse3X <- rmse3%>%
  mutate(Model = "RF_9", Data = "Train") %>%
  relocate(Model, Data, .before = mtry)

all_rmse <- bind_rows(rmseX, rmse2X, rmse3X)

#save
write_csv(all_metrics, "test_metrics.csv")
write_csv(all_rmse, "train_metrics.csv")



model_metrics <- read_csv("model_metrics.csv")



train_plots <- ggplot(model_metrics, aes(x = Model, y = Train, fill = Model)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0, 1500) +
  theme(legend.position = "none") +
  labs(title = "RMSE on Training Data") +
  geom_text(aes(label=Train), vjust=-0.3, size=3.5)
  
test_plots <- ggplot(model_metrics, aes(x = Model, y = Test, fill = Model)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  ylab(NULL) +
  ylim(0, 1500) +
  theme(legend.position = "none") +
  labs(title = "RMSE on Testing Data") +
  geom_text(aes(label=Test), vjust=-0.3, size=3.5)

grid.arrange(train_plots, test_plots, ncol = 2)




#Misc analysis

XData <- read_csv("coin_Bitcoin.csv") %>%
  select(Date, Volume) %>%
  filter(Date < "2021-02-26")

Xinds <- seq(as.Date("2013-04-29"), as.Date("2021-02-25"), by = "day")

# Create a time series object
Z <- ts(data = XData[,2], start = c(2013, as.numeric(format(inds[1], "%j"))), frequency = 365.)

autoplot(Z, main = "Bitcoin Volume (2013-2021)", color = "blue", ylab = "Trading Volume (USD)", xlab = NULL)






#combine model predictions and plot ----

test_predictions <- rf_test_pred %>%
  select(.pred, .row, xBTCx) %>%
  rename(RF_145 = .pred, obs = .row)

test_pred2 <- rf_test_pred2 %>%
  select(.pred, .row) %>%
  rename(RF_30 = .pred, obs = .row)

test_pred3 <- rf_test_pred3 %>%
  select(.pred, .row) %>%
  rename(RF_9 = .pred, obs = .row)

dates <- BTC_data %>%
  select(Date) %>%
  filter(Date >="2020-08-28")

test_predictions <- test_predictions %>%
  left_join(test_pred2, by = "obs") %>%
  left_join(test_pred3, by = "obs") %>%
  bind_cols(dates) %>%
  add_column(naive = -165) %>%
  relocate(Date, obs, xBTCx, naive, .before = RF_145)






plot_data <- test_predictions %>%
  select(Date, xBTCx, naive, RF_30) %>%
  gather(variable, value, -Date) %>%
  rename(Model = variable)

ggplot(plot_data, aes(x = Date, y = value, color = Model)) +
  geom_line(size = 0.7) +
  scale_color_manual(values = c("dodgerblue", "red", "black")) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_classic() +
  labs(
    title = "Model Predictions vs Truth (xBTCx)",
    x = NULL,
    y = "Change in price"
  )







#Build classifier model

# BTC_class <- BTC_data2 %>%
#   mutate(Direction = as_factor(ifelse(xBTCx >0, "UP", "DN"))) %>%
#   relocate(Direction, .after = Date)
# 
# BTC_class$xBTCx = NULL
# BTC_class$Date = NULL
# 
# 
# set.seed(123)
# 
# class_split <- initial_time_split(BTC_class, prop = 3/4)
# class_train <- training(class_split)
# class_test <- testing(class_split)
# 
# 
# set.seed(678)
# 
# rf <- randomForest(Direction ~ ., data = class_train, ntree = 1000)
# 
# rf
# 
# plot(rf)
# 
# 
# importance(rf)
# varImpPlot(rf)
# 




# Classifier model following IKEA method from Julia Silge
# BTC_class <- BTC_data2 %>%
#   mutate(Direction = as_factor(ifelse(xBTCx >0, "UP", "DN"))) %>%
#   relocate(Direction, .after = Date)
# 
# set.seed(123)
# class_split <- initial_time_split(BTC_class, prop = 3/4)
# class_train <- training(class_split)
# class_test <- testing(class_split)
# 
# set.seed(234)
# class_folds <- rolling_origin(data = class_train, initial = 446, assess = 1, cumulative = FALSE)
# class_folds
# 
# library(usemodels)
# use_ranger(Direction ~ ., data = class_train)
# 
# ranger_recipe <- 
#   recipe(formula = Direction ~ ., data = class_train) %>%
#   update_role(Date, xBTCx, new_role = "ID")
# 
# ranger_spec <- 
#   rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
#   set_mode("classification") %>% 
#   set_engine("ranger") 
# 
# ranger_workflow <- 
#   workflow() %>% 
#   add_recipe(ranger_recipe) %>% 
#   add_model(ranger_spec) 
# 
# set.seed(567)
# ranger_tune <-
#   tune_grid(ranger_workflow, resamples = class_folds, grid = 10)



