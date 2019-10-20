wd = 'D:\\Codes\\Code Repositories\\filter'
source(paste(wd, 'filter.R', sep = '\\'))

##############################
##### misc strategies

L1.momentum.strat <- function(data, w0) {
  T3 = 130
  T2 = 130 * 4
  T1_3 = T3 * 4
  T1_2 = T2 * 4
  n.roll = 12
  n.lambda = 15
  risk.constrt = 0.5
  
  trd.result = cv.fit.l1tf(x.hist=log(data), T1=T1_3, T2=T3, n.roll=12, n.lambda=15)
  pred.mu = trd.result$predicted.trend[1]
  n.data = length(data)
  vol = mean(log(unclass(data[(n.data - (T2 - 1) + 1):n.data]) / unclass(data[(n.data - (T2 - 1) + 1):n.data - 1]))^2)
  asset.alloc = pred.mu / (risk.constrt * w0 * vol)
  asset.alloc = min(max(asset.alloc, -1), 1)
  return(asset.alloc)
}
