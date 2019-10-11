wd = 'D:\\Codes\\Code Repositories\\filter'
setwd(wd)
source('filter.R')
source('data.R')
source('analysis.R')

##############################
##### input data

tick =getData('data\\trades.csv')

library(quantmod)

getSymbols('^GSPC', from = '2007-01-01', to = '2011-06-30')
head(GSPC)
nrow(GSPC)
sp500 = log(GSPC$GSPC.Adjusted)
plot(sp500)

##############################
##### test sp500: figure 7

n.period = 2
obs.time = 1:1008
fut.time = 1009:1058
sp500.sp = matrix(log(unclass(sp500))[1:(floor(nrow(sp500[obs.time])/n.period)*n.period)], ncol = n.period)
n.obs = nrow(sp500.sp)
D2 = getD(n.obs, 2)
lamb.max = apply(sp500.sp, 2, function (x) max(abs(solve(D2 %*% t(D2)) %*% D2 %*% x)))
mean(lamb.max)

plot(l1tf.diff1(sp500[obs.time], mean(lamb.max), k=2), type = 'l')

## figure 10
hist.data = sp500[obs.time]
fut.data = sp500[fut.time]
sp500[fut.time]
unclass(sp500)[fut.time[50]] / unclass(sp500)[fut.time[1]] - 1 # simple return
fut.data[50] - fut.data[1] # log return

## estimate the optimal lambda for L1-T filter

trend.sp500 = cv.fit.l1tf(hist.data, fut.data, 400, 50, 12,15)
simple.summary.tf(trend.sp500)
hist.trend.sp500 = l1tf(hist.data, trend.sp500$best.lambda)
plot(hist.trend.sp500, type = 'l')

## estimate the optimal lambda for L1-C filter

trend.sp500 = cv.fit.l1tf(hist.data, fut.data, 400, 50, 12,15, 1)
simple.summary.tf(trend.sp500, ylim=c(7, 7.5))
hist.trend.sp500 = l1tf.diff1(hist.data, trend.sp500$best.lambda, 1)
plot(hist.trend.sp500, type = 'l')

## apply the optimal lambda1 and lambda2 for L1-TC filter

trend.sp500 = cv.fit.l1tf.mix(hist.data, fut.data, 400, 50, 12,15, 2)
simple.summary.tf(trend.sp500)
hist.trend.sp500 = l1tf.mix(hist.data, trend.sp500$best.lambda1, trend.sp500$best.lambda2, 2)
plot(hist.trend.sp500, type = 'l')

## estimate the optimal lambda for HP filter

trend.sp500 = fit.hptf(hist.data, fut.data, 400, 50)
simple.summary.tf(trend.sp500)
hist.trend.sp500 = hptf(hist.data, trend.sp500$best.lambda)
plot(hist.trend.sp500, type = 'l')


##############################
##### select data from special period to test the performance of l1tf
## 0005.HK
head(tick[['0005.HK']])
plot(tick[['0005.HK']]$price)

stock = tick[['0005.HK']]['2019-09-04 14:00/2019-09-04 14:30']$price
nrow(stock)
plot(stock)
trend.l1 = cv.fit.l1tf(stock, NA, 400, 100, 12, 15)
trend.l1 = l1tf(stock, trend.l1$best.lambda)
plot(as.vector(stock), type = 'l', col='blue')
lines(trend.l1, col = "red", type='l')

## 0700.HK
head(tick[['0700.HK']])
plot(tick[['0700.HK']]$price)

stock = tick[['0700.HK']]['2019-09-05 13:30/2019-09-05 14:00']$price
nrow(stock)
plot(stock)
trend.mix = cv.fit.l1tf.mix(stock, NA, 400, 100, 12, 15)
trend.mix = l1tf.mix(stock, trend.mix$best.lambda1, trend.mix$best.lambda2)
plot(as.vector(stock), type = 'l', col='blue')
lines(trend.mix, col = "red", type='l')
