wd = 'D:\\Codes\\Code Repositories\\filter'
setwd(wd)

source('data.R')
source('analysis.R')
source('backtest.R')
source('strategy.R')

library(quantmod)
# library(highfrequency)

##############################
##### input data

hk.stock =getData('data\\trades.csv')

getSymbols('^GSPC', from = '1965-01-01', to = '2011-01-01')
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

as.numeric(head(sp500['2000-01-01/'], 1)) / as.numeric(tail(sp500, 1)) - 1
# 
##############################
##### run backtest
w0 = 100
bt.result = backtest(L1.momentum.strat.LT, '2000-01-01', '2011-01-01', sp500, rf, w0)
bt.stats = simple.summary.strat(bt.result)

bt.result = backtest(L1.momentum.strat.GT, '2000-01-01', '2011-01-01', sp500, rf, w0)
bt.stats = simple.summary.strat(bt.result)

bt.result = backtest(L1.momentum.strat.LGT, '2000-01-01', '2011-01-01', sp500, rf, w0)
bt.stats = simple.summary.strat(bt.result)

bt.result = backtest(MA.strat, '2000-01-01', '2011-01-01', sp500, rf, w0)
bt.stats = simple.summary.strat(bt.result)

bt.result = backtest(L2.strat, '2000-01-01', '2011-01-01', sp500, rf, w0)
bt.stats = simple.summary.strat(bt.result)

##############################
##### analyze the performance

##############################
##### high frequency

bt.result = backtest.high.freq(L1.high.freq.strat, hk.stock[['0700.HK']])
bt.stats = hf.summary.strat(bt.result)
