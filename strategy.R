wd = 'D:\\Codes\\Code Repositories\\filter'
setwd(wd)

source('filter.R')
source('data.R')
source('analysis.R')

library(quantmod)

##############################
##### input data

hk.stock =getData('data\\trades.csv')

getSymbols('^GSPC', from = '1998-01-01', to = '2011-01-01')
head(GSPC)
nrow(GSPC)
sp500 = GSPC$GSPC.Adjusted
plot(log(sp500))
date = index(sp500)

getSymbols('DGS10', src = 'FRED')
rf = DGS10[date] / 100
which(is.na(rf))
rf[which(is.na(rf))] = rf[which(is.na(rf)) - 1]
plot(rf)

##############################
##### run backtest







##############################
##### analyze the performance
