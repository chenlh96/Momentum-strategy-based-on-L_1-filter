wd = 'D:\\Codes\\Code Repositories\\filter'
source(paste(wd, 'filter.R', sep = '\\'))

##############################
##### misc strategies

L1.momentum.strat.LT <- function(data, w0, curr.vars=NULL) {
  data = coredata(data)
  T2 = 130
  mult = 4
  T1 = mult * T2
  risk = 0.001
  return(L1.momentum.strat.gen(log(data), w0, T1, T2,risk.constrt = risk, curr.vars=curr.vars))
}

L1.momentum.strat.GT <- function(data, w0, curr.vars=NULL) {
  data = coredata(data)
  T2 = 130 * 4
  mult = 4
  T1 = mult * T2
  risk = 0.001
  return(L1.momentum.strat.gen(log(data), w0, T1, T2, risk.constrt = risk, curr.vars=curr.vars))
}

L1.momentum.strat.LGT <- function(data, w0, curr.vars=NULL) {
  data = coredata(data)
  T3 = 130
  T2 = 130 * 4
  mult = 4
  T1 = mult * T2
  risk = 0.001
  return(L1.momentum.strat.gen(log(data), w0, T1, c(T2, T3), risk.constrt = risk, curr.vars=curr.vars))
}

L1.momentum.strat.gen <- function(data, w0, T1, T2, n.roll=12, n.lambda=15, risk.constrt=1, curr.vars=NULL) {
  if (is.null(curr.vars)) {
    curr.vars = list(count=max(T2) + 1, pred.trend=NA, asset.alloc=NA, max.count=max(T2))
  }
  if (curr.vars$count > curr.vars$max.count) {
    curr.vars$count = 1
    trd.result = cv.fit.l1tf(x.hist=data, T1=T1, T2=T2, n.roll=n.roll, n.lambda=n.lambda)
    curr.vars$pred.trend = trd.result$predicted.trend
    curr.vars$max.count = trd.result[['T2']]
  }
  pred.mu = curr.vars$pred.trend[2] - curr.vars$pred.trend[1]
  n.data = length(data)
  vol = mean(diff(data[(n.data - curr.vars$max.count + 1 - 1):n.data])^2)
  asset.alloc = pred.mu / (risk.constrt * w0 * vol)
  curr.vars$asset.alloc = min(max(asset.alloc, -1), 1)
  curr.vars$count = curr.vars$count + 1
  return(curr.vars)
}

MA.strat <- function(data, w0, curr.vars=NULL) {
  data = coredata(data)
  T1 = 130 * 4 * 4
  T2 = 130 * 4
  risk.constrt= 0.001
  
  if (is.null(curr.vars)) {
    curr.vars = list(asset.alloc=NA, l = NA)
    l = matrix(0, ncol = T1)
    l[0] = 1 / T1
    l[length(l)] = -1 / T1
    curr.vars$l = l
  }
  
  n.data = length(data)
  pred.mu =as.numeric(curr.vars$l %*% log(data)[(n.data - T1 + 1):n.data])
  vol = mean(diff(log(data[(n.data - T2 + 1 - 1):n.data]))^2)
  asset.alloc = pred.mu / (risk.constrt * w0 * vol)
  curr.vars$asset.alloc = min(max(asset.alloc, -1), 1)
  return(curr.vars)
}

L2.strat <- function(data, w0, curr.vars=NULL) {
  data = coredata(data)
  T1 = 130 * 4 * 4
  T2 = 130 * 4
  risk.constrt=0.1
  if (is.null(curr.vars)) {
    curr.vars = list(asset.alloc=NA, l = NA)
    D =getD(T1, 2)
    ID = diag(T1)
    lb = (T2 / 2 / pi)**4 / 2
    L = solve(ID + 2 * lb * t(D) %*% D)
    l = L[nrow(L),]
    l[2:(length(l))] = l[2:(length(l))] - l[2:(length(l)) - 1]
    l[length(l)] = -l[length(l) - 1]
    curr.vars$l = matrix(l, nrow = 1)
  }
  
  n.data = length(data)
  pred.mu =as.numeric(curr.vars$l %*% log(data)[(n.data - T1 + 1):n.data])
  vol = mean(diff(log(data[(n.data - T2 + 1 - 1):n.data]))^2)
  asset.alloc = pred.mu / (risk.constrt * w0 * vol)
  curr.vars$asset.alloc = min(max(asset.alloc, -1), 1)
  return(curr.vars)
}
