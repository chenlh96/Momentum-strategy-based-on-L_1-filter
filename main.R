wd = 'D:\\Codes\\Code Repositories\\filter'
setwd(wd)

source('data.R')
source('analysis.R')
source('backtest.R')
source('strategy.R')
source('strategy_hf.R')
source('backtest_hf.R')

library(quantmod)
library(highfrequency)

##############################
##### input data

tick =getData('data\\trades0700_1.csv', symbol='0700.HK')

getSymbols('^GSPC', from = '1965-01-01', to = '2011-01-01')
head(GSPC)
nrow(GSPC)
sp500 = GSPC$GSPC.Adjusted
plot(diff(log(sp500['2001/'])))
date = index(sp500)

getSymbols('DGS10', src = 'FRED')
rf = DGS10[date] / 100
which(is.na(rf))
rf[which(is.na(rf))] = rf[which(is.na(rf)) - 1]
plot(rf['2001/'])

w0 = 10000

##############################
##### run backtest
bt.result = backtest(L1.momentum.strat.LT, '2001-01-01', '2011-01-01', sp500, rf, w0)
bt.stats = simple.summary.strat(bt.result)

bt.result = backtest(L1.momentum.strat.GT, '2001-01-01', '2011-01-01', sp500, rf, w0)
bt.stats = simple.summary.strat(bt.result)

bt.result = backtest(L1.momentum.strat.LGT, '2001-01-01', '2011-01-01', sp500, rf, w0)
bt.stats = simple.summary.strat(bt.result)

bt.result = backtest(MA.strat, '2001-01-01', '2011-01-01', sp500, rf, w0)
bt.stats = simple.summary.strat(bt.result)

bt.result = backtest(L2.strat, '2001-01-01', '2011-01-01', sp500, rf, w0)
bt.stats = simple.summary.strat(bt.result)

##############################
##### analyze the performance

##############################
##### high frequency

bt.result = backtest.hf(L1.high.freq.strat, tick[['0700.HK']], w0 =100000,lot.size = 100, k = 1, on = 'minutes')
bt.stats = hf.summary.strat(bt.result)
f 