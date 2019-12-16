wd = 'D:\\Codes\\Code Repositories\\filter'
source(paste(wd, 'filter.R', sep = '\\'))

##############################
##### misc strategies

L1.momentum.strat.LT <- function(data, w0, curr.vars=NULL) {
  T2 = 130
  mult = 4
  T1 = mult * T2
  risk = 0.005
  return(L1.momentum.strat.gen(log(data), w0, T1, T2,risk.constrt = risk, curr.vars=curr.vars))
}

L1.momentum.strat.GT <- function(data, w0, curr.vars=NULL) {
  T2 = 130 * 4
  mult = 4
  T1 = mult * T2
  risk = 0.005
  return(L1.momentum.strat.gen(log(data), w0, T1, T2, risk.constrt = risk, curr.vars=curr.vars))
}

L1.momentum.strat.LGT <- function(data, w0, curr.vars=NULL) {
  T3 = 130
  T2 = 130 * 4
  mult = 4
  T1 = mult * T2
  risk = 0.005
  return(L1.momentum.strat.gen(log(data), w0, T1, c(T2, T3), risk.constrt = risk, curr.vars=curr.vars))
}

L1.momentum.strat.gen <- function(data, w0, T1, T2, n.roll=12, n.lambda=15, risk.constrt=1, curr.vars=NULL) {
  if (is.null(curr.vars)) {
    curr.vars = list(count=max(T2) + 1, pred.trend=NA, asset.alloc=NA, max.count=max(T2))
  }
  if (curr.vars$count > curr.vars$max.count) {
    curr.vars$count = 1
    trd.result = cv.fit.l1tf(x.hist=log(data), T1=T1, T2=T2, n.roll=n.roll, n.lambda=n.lambda)
    curr.vars$pred.trend = trd.result$predicted.trend
    curr.vars$max.count = trd.result[['T2']]
  }
  pred.mu = curr.vars$pred.trend[2] - curr.vars$pred.trend[1]
  n.data = length(data)
  vol = mean(diff(log(as.vector(data[(n.data - curr.vars$max.count + 1 - 1):n.data])))^2)
  asset.alloc = pred.mu / (risk.constrt * w0 * vol)
  curr.vars$asset.alloc = min(max(asset.alloc, -1), 1)
  curr.vars$count = curr.vars$count + 1
  return(curr.vars)
}

MA.strat <- function(data, w0, curr.vars=NULL) {
  T1 = 130 * 4
  T2= 130
  risk.constrt=0.005
  
  if (is.null(curr.vars)) {
    curr.vars = list(count=max(T2) + 1, pred.trend=NA, asset.alloc=NA, max.count=max(T2))
  }
  
  if (curr.vars$count > curr.vars$max.count) {
    curr.vars$count = 1
    trd.result = fit.matf(x.hist=log(data), x.fut=NA, T1 = T1, T2 = T2)
    curr.vars$pred.trend = trd.result$predicted.trend
  }
  
  pred.mu = curr.vars$pred.trend[2] - curr.vars$pred.trend[1]
  n.data = length(data)
  vol = mean(diff(log(as.vector(data[(n.data - T2 + 1 - 1):n.data])))^2)
  asset.alloc = pred.mu / (risk.constrt * w0 * vol)
  curr.vars$asset.alloc = min(max(asset.alloc, -1), 1)
  curr.vars$count = curr.vars$count + 1
  return(curr.vars)
}

L2.strat <- function(data, w0, curr.vars=NULL) {
  T1 = 130 * 4
  T2 = 130
  risk.constrt=0.005
  if (is.null(curr.vars)) {
    curr.vars = list(count=max(T2) + 1, pred.trend=NA, asset.alloc=NA, max.count=max(T2))
  }
  
  if (curr.vars$count > curr.vars$max.count) {
    curr.vars$count = 1
    trd.result = fit.hptf(x.hist=log(data), x.fut=NA, T1 = T1, T2 = T2)
    curr.vars$pred.trend = trd.result$predicted.trend
  }
  
  pred.mu = curr.vars$pred.trend[2] - curr.vars$pred.trend[1]
  n.data = length(data)
  vol = mean(diff(log(as.vector(data[(n.data - T2 + 1 - 1):n.data])))^2)
  asset.alloc = pred.mu / (risk.constrt * w0 * vol)
  curr.vars$asset.alloc = min(max(asset.alloc, -1), 1)
  curr.vars$count = curr.vars$count + 1
  return(curr.vars)
}
