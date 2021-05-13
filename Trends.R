library(tidyverse)
options(scipen = 999, digits = 4)

library(gtrendsR)

#create a list of dates to be passed into the gtrends function time argument
dates <- c("2019-02-01 2019-02-28", "2019-03-01 2019-03-31", "2019-04-01 2019-04-30", "2019-05-01 2019-05-31", "2019-06-01 2019-06-30", 
           "2019-07-01 2019-07-31", "2019-08-01 2019-08-31", "2019-09-01 2019-09-30", "2019-10-01 2019-10-31", 
           "2019-11-01 2019-11-30", "2019-12-01 2019-12-31", "2020-01-01 2020-01-31", "2020-02-01 2020-02-29", 
           "2020-03-01 2020-03-31", "2020-04-01 2020-04-30", "2020-05-01 2020-05-31", "2020-06-01 2020-06-30", 
           "2020-07-01 2020-07-31", "2020-08-01 2020-08-31", "2020-09-01 2020-09-30", "2020-10-01 2020-10-31", 
           "2020-11-01 2020-11-30", "2020-12-01 2020-12-31", "2021-01-01 2021-01-31", "2021-02-01 2021-02-28")

#Import dates info and use to join monthly and daily hits 
FOM <- read.csv("FOM.csv")
FOM$date <- as_date(FOM$date)
FOM$FOM <- as_date(FOM$FOM)



#Iterate through dates to compile data. Keyword = "Buy Bitcoin".

#Run "all" period separately
output <- gtrends(keyword = "Buy Bitcoin", time = "all", onlyInterest = TRUE)
BuyBTC_all_trends <- as_tibble(output[[1]])
BuyBTC_all_trends$hits <- as.integer(BuyBTC_all_trends$hits)
BuyBTC_all_trends <-BuyBTC_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

#Loop for daily data
output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "Buy Bitcoin", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
BuyBTC_daily_trends <-as_tibble(output$interest_over_time)
BuyBTC_daily_trends$date <- as_date(BuyBTC_daily_trends$date)

Sys.sleep(10)

#Run each keyword separately. Running too many at once may cause Google API to reject query.


#keyword "Bitcoin" 
output <- gtrends(keyword = "Bitcoin", time = "all", onlyInterest = TRUE)
BTC_all_trends <- as_tibble(output[[1]])
BTC_all_trends$hits <- as.integer(BTC_all_trends$hits)
BTC_all_trends <-BTC_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "Bitcoin", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
BTC_daily_trends <-as_tibble(output$interest_over_time)
BTC_daily_trends$date <- as_date(BTC_daily_trends$date)

Sys.sleep(10)

#Keyword: what is bitcoin


output <- gtrends(keyword = "what is bitcoin", time = "all", onlyInterest = TRUE)
WhatBTC_all_trends <- as_tibble(output[[1]])
WhatBTC_all_trends$hits <- as.integer(WhatBTC_all_trends$hits)
WhatBTC_all_trends <-WhatBTC_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "what is bitcoin", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
WhatBTC_daily_trends <-as_tibble(output$interest_over_time)
WhatBTC_daily_trends$date <- as_date(WhatBTC_daily_trends$date)

Sys.sleep(10)

#Keyword: Bitcoin wallet


output <- gtrends(keyword = "bitcoin wallet", time = "all", onlyInterest = TRUE)
Wallet_all_trends <- as_tibble(output[[1]])
Wallet_all_trends$hits <- as.integer(Wallet_all_trends$hits)
Wallet_all_trends <-Wallet_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "bitcoin wallet", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
Wallet_daily_trends <-as_tibble(output$interest_over_time)
Wallet_daily_trends$date <- as_date(Wallet_daily_trends$date)

Sys.sleep(10)

#Keyword: Ethereum


output <- gtrends(keyword = "Ethereum", time = "all", onlyInterest = TRUE)
ETH_all_trends <- as_tibble(output[[1]])
ETH_all_trends$hits <- as.integer(ETH_all_trends$hits)
ETH_all_trends <-ETH_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "Ethereum", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
ETH_daily_trends <-as_tibble(output$interest_over_time)
ETH_daily_trends$date <- as_date(ETH_daily_trends$date)

Sys.sleep(10)

#Keyword: Buy Ethereum


output <- gtrends(keyword = "buy ethereum", time = "all", onlyInterest = TRUE)
BuyEth_all_trends <- as_tibble(output[[1]])
BuyEth_all_trends$hits <- as.integer(BuyEth_all_trends$hits)
BuyEth_all_trends <-BuyEth_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "buy ethereum", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
BuyEth_daily_trends <-as_tibble(output$interest_over_time)
BuyEth_daily_trends$date <- as_date(BuyEth_daily_trends$date)

Sys.sleep(10)

#Keyword: Crypto


output <- gtrends(keyword = "crypto", time = "all", onlyInterest = TRUE)
crypto_all_trends <- as_tibble(output[[1]])
crypto_all_trends$hits <- as.integer(crypto_all_trends$hits)
crypto_all_trends <-crypto_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "crypto", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
crypto_daily_trends <-as_tibble(output$interest_over_time)
crypto_daily_trends$date <- as_date(crypto_daily_trends$date)

Sys.sleep(10)

#Keyword: blockchain


output <- gtrends(keyword = "blockchain", time = "all", onlyInterest = TRUE)
block_all_trends <- as_tibble(output[[1]])
block_all_trends$hits <- as.integer(block_all_trends$hits)
block_all_trends <-block_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "blockchain", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
block_daily_trends <-as_tibble(output$interest_over_time)
block_daily_trends$date <- as_date(block_daily_trends$date)

Sys.sleep(10)

#Keyword: HODL


output <- gtrends(keyword = "HODL", time = "all", onlyInterest = TRUE)
HODL_all_trends <- as_tibble(output[[1]])
HODL_all_trends$hits <- as.integer(HODL_all_trends$hits)
HODL_all_trends <-HODL_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "HODL", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
HODL_daily_trends <-as_tibble(output$interest_over_time)
HODL_daily_trends$date <- as_date(HODL_daily_trends$date)

Sys.sleep(10)

#Keyword: Altcoin


output <- gtrends(keyword = "altcoin", time = "all", onlyInterest = TRUE)
altcoin_all_trends <- as_tibble(output[[1]])
altcoin_all_trends$hits <- as.integer(altcoin_all_trends$hits)
altcoin_all_trends <-altcoin_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "altcoin", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
altcoin_daily_trends <-as_tibble(output$interest_over_time)
altcoin_daily_trends$date <- as_date(altcoin_daily_trends$date)

Sys.sleep(10)

#Keyword: Satoshi Nakamoto


output <- gtrends(keyword = "Satoshi Nakamoto", time = "all", onlyInterest = TRUE)
Satoshi_all_trends <- as_tibble(output[[1]])
Satoshi_all_trends$hits <- as.integer(Satoshi_all_trends$hits)
Satoshi_all_trends <-Satoshi_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "Satoshi Nakamoto", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
Satoshi_daily_trends <-as_tibble(output$interest_over_time)
Satoshi_daily_trends$date <- as_date(Satoshi_daily_trends$date)

Sys.sleep(10)

#Keyword: buy the dip


output <- gtrends(keyword = "buy the dip", time = "all", onlyInterest = TRUE)
dip_all_trends <- as_tibble(output[[1]])
dip_all_trends$hits <- as.integer(dip_all_trends$hits)
dip_all_trends <-dip_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "buy the dip", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
dip_daily_trends <-as_tibble(output$interest_over_time)
dip_daily_trends$date <- as_date(dip_daily_trends$date)

Sys.sleep(10)

#Keyword: mining pool


output <- gtrends(keyword = "mining pool", time = "all", onlyInterest = TRUE)
pool_all_trends <- as_tibble(output[[1]])
pool_all_trends$hits <- as.integer(pool_all_trends$hits)
pool_all_trends <-pool_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "mining pool", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
pool_daily_trends <-as_tibble(output$interest_over_time)
pool_daily_trends$date <- as_date(pool_daily_trends$date)

Sys.sleep(10)

#Keyword: what to invest in


output <- gtrends(keyword = "what to invest in", time = "all", onlyInterest = TRUE)
invest_all_trends <- as_tibble(output[[1]])
invest_all_trends$hits <- as.integer(invest_all_trends$hits)
invest_all_trends <-invest_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "what to invest in", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
invest_daily_trends <-as_tibble(output$interest_over_time)
invest_daily_trends$date <- as_date(invest_daily_trends$date)

Sys.sleep(10)

#Keyword: get rich


output <- gtrends(keyword = "get rich", time = "all", onlyInterest = TRUE)
rich_all_trends <- as_tibble(output[[1]])
rich_all_trends$hits <- as.integer(rich_all_trends$hits)
rich_all_trends <-rich_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "get rich", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
rich_daily_trends <-as_tibble(output$interest_over_time)
rich_daily_trends$date <- as_date(rich_daily_trends$date)

Sys.sleep(10)

#Keyword: recession (US)


output <- gtrends(keyword = "recession", time = "all", onlyInterest = TRUE, geo = "US")
recession_all_trends <- as_tibble(output[[1]])
recession_all_trends$hits <- as.integer(recession_all_trends$hits)
recession_all_trends <-recession_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "recession", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
recession_daily_trends <-as_tibble(output$interest_over_time)
recession_daily_trends$date <- as_date(recession_daily_trends$date)

Sys.sleep(10)

#Keyword: debt(US)


output <- gtrends(keyword = "debt", time = "all", onlyInterest = TRUE, geo = "US")
debt_all_trends <- as_tibble(output[[1]])
debt_all_trends$hits <- as.integer(debt_all_trends$hits)
debt_all_trends <-debt_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "debt", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
debt_daily_trends <-as_tibble(output$interest_over_time)
debt_daily_trends$date <- as_date(debt_daily_trends$date)

Sys.sleep(10)

#Keyword: bear market (US)


output <- gtrends(keyword = "bear market", time = "all", onlyInterest = TRUE, geo = "US")
bear_all_trends <- as_tibble(output[[1]])
bear_all_trends$hits <- as.integer(bear_all_trends$hits)
bear_all_trends <-bear_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "bear market", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
bear_daily_trends <-as_tibble(output$interest_over_time)
bear_daily_trends$date <- as_date(bear_daily_trends$date)

Sys.sleep(10)

#Keyword: bull market (US)


output <- gtrends(keyword = "bull market", time = "all", onlyInterest = TRUE, geo = "US")
bull_all_trends <- as_tibble(output[[1]])
bull_all_trends$hits <- as.integer(bull_all_trends$hits)
bull_all_trends <-bull_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "bull market", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
bull_daily_trends <-as_tibble(output$interest_over_time)
bull_daily_trends$date <- as_date(bull_daily_trends$date)

Sys.sleep(10)

#Keyword: stocks (US)


output <- gtrends(keyword = "stocks", time = "all", onlyInterest = TRUE, geo = "US")
stocks_all_trends <- as_tibble(output[[1]])
stocks_all_trends$hits <- as.integer(stocks_all_trends$hits)
stocks_all_trends <-stocks_all_trends %>% rename(MoHits = hits)

Sys.sleep(10)

output <- vector("list", length(dates))
for (i in seq_along(dates)) {
  output[[i]] <- gtrends(keyword = "stocks", time = dates[[i]], onlyInterest = TRUE)
}

output <- bind_rows(output)
stocks_daily_trends <-as_tibble(output$interest_over_time)
stocks_daily_trends$date <- as_date(stocks_daily_trends$date)


#Add first-of-month date to each record in daily trends.

BTC_daily_trends <- left_join(BTC_daily_trends, FOM)
BuyBTC_daily_trends <- left_join(BuyBTC_daily_trends, FOM)
WhatBTC_daily_trends <- left_join(WhatBTC_daily_trends, FOM)
Wallet_daily_trends <- left_join(Wallet_daily_trends, FOM)
ETH_daily_trends <- left_join(ETH_daily_trends, FOM)
BuyEth_daily_trends <- left_join(BuyEth_daily_trends, FOM)
crypto_daily_trends  <- left_join(crypto_daily_trends, FOM)
block_daily_trends <- left_join(block_daily_trends, FOM) 
HODL_daily_trends <- left_join(HODL_daily_trends, FOM)
altcoin_daily_trends <- left_join(altcoin_daily_trends, FOM)
Satoshi_daily_trends <- left_join(Satoshi_daily_trends, FOM)
dip_daily_trends <- left_join(dip_daily_trends, FOM)
pool_daily_trends <- left_join(pool_daily_trends, FOM)
invest_daily_trends <- left_join(invest_daily_trends, FOM)
rich_daily_trends <- left_join(rich_daily_trends, FOM)
recession_daily_trends <- left_join(recession_daily_trends, FOM)
debt_daily_trends <- left_join(debt_daily_trends, FOM)
bear_daily_trends <- left_join(bear_daily_trends, FOM)
bull_daily_trends <- left_join(bull_daily_trends, FOM)
stocks_daily_trends <- left_join(stocks_daily_trends, FOM)

BuyBTC_daily_trends$FOM <- as_date(BuyBTC_daily_trends$FOM)
BTC_daily_trends$FOM <- as_date(BTC_daily_trends$FOM)
WhatBTC_daily_trends$FOM <- as_date(WhatBTC_daily_trends$FOM)
Wallet_daily_trends$FOM <- as_date(Wallet_daily_trends$FOM)
ETH_daily_trends$FOM <- as_date(ETH_daily_trends$FOM)
BuyEth_daily_trends$FOM <- as_date(BuyEth_daily_trends$FOM)
crypto_daily_trends$FOM  <- as_date(crypto_daily_trends$FOM)
block_daily_trends$FOM <- as_date(block_daily_trends$FOM) 
HODL_daily_trends$FOM <- as_date(HODL_daily_trends$FOM)
altcoin_daily_trends$FOM <- as_date(altcoin_daily_trends$FOM)
Satoshi_daily_trends$FOM <- as_date(Satoshi_daily_trends$FOM)
dip_daily_trends$FOM <- as_date(dip_daily_trends$FOM)
pool_daily_trends$FOM <- as_date(pool_daily_trends$FOM)
invest_daily_trends$FOM <- as_date(invest_daily_trends$FOM)
rich_daily_trends$FOM <- as_date(rich_daily_trends$FOM)
recession_daily_trends$FOM <- as_date(recession_daily_trends$FOM)
debt_daily_trends$FOM <- as_date(debt_daily_trends$FOM)
bear_daily_trends$FOM <- as_date(bear_daily_trends$FOM)
bull_daily_trends$FOM <- as_date(bull_daily_trends$FOM)
stocks_daily_trends$FOM <- as_date(stocks_daily_trends$FOM)

#Add monthly hits to daily trends
BTC_daily_trends <- BTC_daily_trends %>% 
  left_join(select(BTC_all_trends, MoHits, date), by = c("FOM" = "date"))

BuyBTC_daily_trends <- BuyBTC_daily_trends %>% 
  left_join(select(BuyBTC_all_trends, MoHits, date), by = c("FOM" = "date"))

WhatBTC_daily_trends <- WhatBTC_daily_trends %>% 
  left_join(select(WhatBTC_all_trends, MoHits, date), by = c("FOM" = "date"))

Wallet_daily_trends <- Wallet_daily_trends %>% 
  left_join(select(Wallet_all_trends, MoHits, date), by = c("FOM" = "date"))

ETH_daily_trends <- ETH_daily_trends %>% 
  left_join(select(ETH_all_trends, MoHits, date), by = c("FOM" = "date"))

BuyEth_daily_trends <- BuyEth_daily_trends %>% 
  left_join(select(BuyEth_all_trends, MoHits, date), by = c("FOM" = "date"))

crypto_daily_trends <- crypto_daily_trends %>% 
  left_join(select(crypto_all_trends, MoHits, date), by = c("FOM" = "date"))

block_daily_trends <- block_daily_trends %>% 
  left_join(select(block_all_trends, MoHits, date), by = c("FOM" = "date"))

HODL_daily_trends <- HODL_daily_trends %>% 
  left_join(select(HODL_all_trends, MoHits, date), by = c("FOM" = "date"))

altcoin_daily_trends <- altcoin_daily_trends %>% 
  left_join(select(altcoin_all_trends, MoHits, date), by = c("FOM" = "date"))

Satoshi_daily_trends <- Satoshi_daily_trends %>% 
  left_join(select(Satoshi_all_trends, MoHits, date), by = c("FOM" = "date"))

dip_daily_trends <- dip_daily_trends %>% 
  left_join(select(dip_all_trends, MoHits, date), by = c("FOM" = "date"))

pool_daily_trends <- pool_daily_trends %>% 
  left_join(select(pool_all_trends, MoHits, date), by = c("FOM" = "date"))

invest_daily_trends <- invest_daily_trends %>% 
  left_join(select(invest_all_trends, MoHits, date), by = c("FOM" = "date"))

rich_daily_trends <- rich_daily_trends %>% 
  left_join(select(rich_all_trends, MoHits, date), by = c("FOM" = "date"))

recession_daily_trends <- recession_daily_trends %>% 
  left_join(select(recession_all_trends, MoHits, date), by = c("FOM" = "date"))

debt_daily_trends <- debt_daily_trends %>% 
  left_join(select(debt_all_trends, MoHits, date), by = c("FOM" = "date"))

bear_daily_trends <- bear_daily_trends %>% 
  left_join(select(bear_all_trends, MoHits, date), by = c("FOM" = "date"))

bull_daily_trends <- bull_daily_trends %>% 
  left_join(select(bull_all_trends, MoHits, date), by = c("FOM" = "date"))

stocks_daily_trends <- stocks_daily_trends %>% 
  left_join(select(stocks_all_trends, MoHits, date), by = c("FOM" = "date"))


#Add Scaled Hits and Change
BTC_daily_trends <- BTC_daily_trends %>%
  mutate(BTCHits = hits * MoHits / 100) %>%
  mutate(BTCHitsChg = (BTCHits/lag(BTCHits)-1)*100) %>%
  rename(Date=date)

BuyBTC_daily_trends <- BuyBTC_daily_trends %>%
  mutate(BuyBTCHits = hits * MoHits / 100) %>%
  mutate(BuyBTCHitsChg = (BuyBTCHits/lag(BuyBTCHits)-1)*100) %>%
  rename(Date=date)

WhatBTC_daily_trends <- WhatBTC_daily_trends %>%
  mutate(WhatBTCHits = hits * MoHits / 100) %>%
  mutate(WhatBTCHitsChg = (WhatBTCHits/lag(WhatBTCHits)-1)*100) %>%
  rename(Date=date)

Wallet_daily_trends <- Wallet_daily_trends %>%
  mutate(WalletHits = hits * MoHits / 100) %>%
  mutate(WalletHitsChg = (WalletHits/lag(WalletHits)-1)*100) %>%
  rename(Date=date)

ETH_daily_trends <- ETH_daily_trends %>%
  mutate(ETHHits = hits * MoHits / 100) %>%
  mutate(ETHHitsChg = (ETHHits/lag(ETHHits)-1)*100) %>%
  rename(Date=date)

BuyEth_daily_trends <- BuyEth_daily_trends %>%
  mutate(BuyEthHits = hits * MoHits / 100) %>%
  mutate(BuyEthHitsChg = (BuyEthHits/lag(BuyEthHits)-1)*100) %>%
  rename(Date=date)

crypto_daily_trends <- crypto_daily_trends %>%
  mutate(cryptoHits = hits * MoHits / 100) %>%
  mutate(cryptoHitsChg = (cryptoHits/lag(cryptoHits)-1)*100) %>%
  rename(Date=date)

block_daily_trends <- block_daily_trends %>%
  mutate(blockHits = hits * MoHits / 100) %>%
  mutate(blockHitsChg = (blockHits/lag(blockHits)-1)*100) %>%
  rename(Date=date)

HODL_daily_trends <- HODL_daily_trends %>%
  mutate(HODLHits = hits * MoHits / 100) %>%
  mutate(HODLHitsChg = (HODLHits/lag(HODLHits)-1)*100) %>%
  rename(Date=date)

altcoin_daily_trends <- altcoin_daily_trends %>%
  mutate(altcoinHits = hits * MoHits / 100) %>%
  mutate(altcoinHitsChg = (altcoinHits/lag(altcoinHits)-1)*100) %>%
  rename(Date=date)

Satoshi_daily_trends <- Satoshi_daily_trends %>%
  mutate(SatoshiHits = hits * MoHits / 100) %>%
  mutate(SatoshiHitsChg = (SatoshiHits/lag(SatoshiHits)-1)*100) %>%
  rename(Date=date)

dip_daily_trends <- dip_daily_trends %>%
  mutate(dipHits = hits * MoHits / 100) %>%
  mutate(dipHitsChg = (dipHits/lag(dipHits)-1)*100) %>%
  rename(Date=date)

pool_daily_trends <- pool_daily_trends %>%
  mutate(poolHits = hits * MoHits / 100) %>%
  mutate(poolHitsChg = (poolHits/lag(poolHits)-1)*100) %>%
  rename(Date=date)

invest_daily_trends <- invest_daily_trends %>%
  mutate(investHits = hits * MoHits / 100) %>%
  mutate(investHitsChg = (investHits/lag(investHits)-1)*100) %>%
  rename(Date=date)

rich_daily_trends <- rich_daily_trends %>%
  mutate(richHits = hits * MoHits / 100) %>%
  mutate(richHitsChg = (richHits/lag(richHits)-1)*100) %>%
  rename(Date=date)

recession_daily_trends <- recession_daily_trends %>%
  mutate(recessionHits = hits * MoHits / 100) %>%
  mutate(recessionHitsChg = (recessionHits/lag(recessionHits)-1)*100) %>%
  rename(Date=date)

debt_daily_trends <- debt_daily_trends %>%
  mutate(debtHits = hits * MoHits / 100) %>%
  mutate(debtHitsChg = (debtHits/lag(debtHits)-1)*100) %>%
  rename(Date=date)

bear_daily_trends <- bear_daily_trends %>%
  mutate(bearHits = hits * MoHits / 100) %>%
  mutate(bearHitsChg = (bearHits/lag(bearHits)-1)*100) %>%
  rename(Date=date)

bull_daily_trends <- bull_daily_trends %>%
  mutate(bullHits = hits * MoHits / 100) %>%
  mutate(bullHitsChg = (bullHits/lag(bullHits)-1)*100) %>%
  rename(Date=date)

stocks_daily_trends <- stocks_daily_trends %>%
  mutate(stocksHits = hits * MoHits / 100) %>%
  mutate(stocksHitsChg = (stocksHits/lag(stocksHits)-1)*100) %>%
  rename(Date=date)


#Combine into one data set.

trends <- list(BTC_daily_trends, BuyBTC_daily_trends, WhatBTC_daily_trends, Wallet_daily_trends, ETH_daily_trends, BuyEth_daily_trends,
               crypto_daily_trends, block_daily_trends, HODL_daily_trends, altcoin_daily_trends, Satoshi_daily_trends, dip_daily_trends,
               pool_daily_trends, invest_daily_trends, rich_daily_trends, recession_daily_trends, debt_daily_trends, bear_daily_trends,
               bull_daily_trends, stocks_daily_trends) %>%
  reduce(left_join, by = "Date")

trends <- trends[,c(1,10,11,20,21,30,31,40,41,50,51,60,61,70,71,80,81,90,91,100,101,
                    110,111,120,121, 130,131, 140,141, 150,151, 160,161, 170,171, 180,181, 190,191, 200,201)]


#Convert NAs to 0 and calculate column means.

trends <- trends %>% replace(is.na(.), 0)

trends <- do.call(data.frame, lapply(trends, function(x) replace(x, is.infinite(x), NA)))

summary(trends)
colMeans(trends[,c(13,19,21,23,25,27)], na.rm=T)

#Replace Inf values (now NA values) with column means.

trends[,13] <- trends[,13] %>% replace(is.na(.), 13.02)
trends[,19] <- trends[,19] %>% replace(is.na(.), 16.77)
trends[,21] <- trends[,21] %>% replace(is.na(.), 27.88)
trends[,23] <- trends[,23] %>% replace(is.na(.), 31.16)
trends[,25] <- trends[,25] %>% replace(is.na(.), -17.33)
trends[,27] <- trends[,27] %>% replace(is.na(.), 31.03)


summary(trends)

trends <- filter(trends, Date >= "2019-03-01")
write.csv(trends, file = "trends.csv", row.names = F)
























































































































































