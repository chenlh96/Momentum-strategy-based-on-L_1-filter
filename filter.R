library(quadprog)
library(CVXR)
library(l1tf)

##############################
##### define trend filter
##### function: hp filtering
hptf <- function(x, lambda, diff=2) {
  n.obs = length(x)
  D.hp = getD(n.obs, diff)
  trend.hp = solve(diag(n.obs) + 2 * lambda * t(D.hp) %*% D.hp) %*% x
  trend.hp = trend.hp[,1]
  return(trend.hp)
} 

##### ma filter

matf <- function(x, T) {
  ma.x = sapply((T + 1):length(x), function (t) mean(x[(t - T):(t - 1)]))
  ma.x = c(rep(NA, T), ma.x)
  return(ma.x)
}

##### function: l1 filtering with first difference of ts
l1tf.diff1 <- function(x, lambda, k= 1) {
  n.obs = length(x)
  
  D =getD(n.obs, k)
  Dmat = D %*% t(D)
  dvec = matrix(x, nrow = 1) %*% t(D)
  dvec = dvec[1,]
  Amat = t(rbind(diag(n.obs - k), diag(-1, n.obs - k)))
  bvec = rep(-lambda, 2 * (n.obs - k))
  
  solu = solve.QP(Dmat, dvec, Amat, bvec)
  v = solu[['solution']]
  trend = x - t(D) %*% v
  return(trend[,1])
}

l1tf.mix <- function(x, lambda1, lambda2, k1=1, k2=2) {
  if (length(class(x)) > 1) {
    x = as.vector(x)
  }
  n.obs = length(x)
  
  trend = Variable(n.obs)
  obj.tf = 0.5 * sum_squares(x - trend) + lambda1 * p_norm(diff(trend, differences=k1), 1) + lambda2 * p_norm(diff(trend, differences=k2), 1)
  p.tf = Problem(Minimize(obj.tf))
  trend = solve(p.tf)$getValue(trend)
  return(trend)
}

##### A new trend filter called sparse l1 filter that is porposed in Tibshirani (2014)

l1tf.sparse <- function(x, lambda1, lambda2, k=1) {
  if (length(class(x)) > 1) {
    x = as.vector(x)
  }
  n.obs = length(x)
  
  trend = Variable(n.obs)
  obj.tf = 0.5 * sum_squares(x - trend) + lambda1 * p_norm(diff(trend, differences=k), 1) + lambda2 * p_norm(trend, 1)
  p.tf = Problem(Minimize(obj.tf))
  trend = solve(p.tf)$getValue(trend)
  return(trend)
}

##############################
##### Implement cv procedure for the l1 filter
##### the cv function
cv.fit.l1tf <- function(x.hist, x.fut=NA, T1, T2, n.roll, n.lambda, diff = 2) {
  if (!(all(class(x.hist) %in% c('numeric', 'matrix')))) {
    x.hist = as.vector(x.hist)
  } 
  if (!(all(class(x.fut) %in% c('numeric', 'matrix')) | all(is.na(x.fut)))) {
    x.fut = as.vector(x.fut)
  }
  len.x = length(x.hist)
  Tg = ifelse(length(T2) == 1, T2, max(T2))
  Tl = ifelse(length(T2) == 1, NA, min(T2))
  len.cv = n.roll * Tg
  cv.i = (1:len.x)[(len.x - len.cv + 1):len.x]
  
  cv.i.sp = matrix(cv.i, ncol = n.roll)
  cv.x = apply(cv.i.sp, 2, function (i) x.hist[i])
  tr.x = apply(cv.i.sp, 2, function (i) x.hist[(i[1] - T1):(i[1] - 1)])
  te.v = x.fut[1:Tg]
  tr.v = x.hist[(len.x - T1 + 1):len.x]
  # la.x = matrix(x.hist[1:(floor(nrow(x.hist)/n.roll)*n.roll)], ncol = n.roll)
  
  cv.result = cv.fit.l1tf.run(tr.x, cv.x, tr.v, te.v, T1, Tg, n.roll, n.lambda, diff)
  cv.result[['T2']] = Tg
  if (!is.na(Tl)) {
    cv.xl = apply(cv.i.sp, 2, function (i) x.hist[i[1]:(i[1] + Tl - 1)])
    te.v = x.fut[1:Tl]
    cv.result.lc = cv.fit.l1tf.run(tr.x, cv.xl,tr.v, te.v, T1, Tl, n.roll, n.lambda, diff)
    sd.glo = sd(cv.result$train.data - cv.result$train.trend)
    if (any(abs(cv.result$train.data - cv.result$train.trend) > sd.glo)) {
      cv.result = cv.result.lc
      cv.result[['T2']] = Tl
    }
  }
  
  return(cv.result)
}

##### A core function for cv, can both handle the long and short term trend
cv.fit.l1tf.run <- function(tr.set, cv.set, tr.vec, te.vec, T1, T2, n.roll, n.lambda, diff) {
  D = getD(nrow(cv.set), diff)
  cv.lamb = apply(cv.set, 2, function(x1) max(rowSums(abs(solve(D %*% t(D)) %*% D %*% x1))))
  l.b = c(max(mean(cv.lamb) - 2 * sd(cv.lamb), 1e-1), mean(cv.lamb) + 2 * sd(cv.lamb))
  lambs = l.b[1] * (l.b[2] / l.b[1])**(1:n.lambda/n.lambda)
  
  tr.trend = list()
  if (diff == 2) {
    tf <- function(x, lambda) l1tf(x, lambda, diff)
  } else if (diff == 1 | diff > 2) {
    tf <- function(x, lambda) l1tf.diff1(x, lambda)
  }
  tr.trend = lapply(lambs, function (l) apply(tr.set, 2, function (x1) tf(x1, l)))
  
  predict.tf <- function (x1) {
    pred = (1:T2) * (x1[T1] - x1[T1 - 1]) + x1[T1]
    pred = ifelse(pred > 0, pred, 0)
    return(pred)
  }

  pr.trend = lapply(tr.trend, function (l) apply(l, 2, predict.tf))
  err = sapply(pr.trend, function (l) colMeans((l - cv.set)^2))
  err = colSums(err)
  b.lamb = lambs[which.min(err)]
  # msg = sprintf('best lambda is: %.6f', b.lamb)
  # print(msg)
  
  tr.trd = tf(tr.vec, b.lamb)
  pr.trd = predict.tf(tr.trd)
  
  result = list(lambda.max = cv.lamb, lambdas = lambs, err = err, best.lambda = b.lamb,
                cv.train.data = tr.set, cv.test.data = cv.set, 
                cv.train.trend = tr.trend, cv.predicted.trend = pr.trend,
                train.data = tr.vec, test.data = te.vec, 
                train.trend = tr.trd, predicted.trend = pr.trd)
  
  return(result)
}

##### auxillary function
getD <- function(n, diff) {
  D0 = diag(-1, n)
  D0[col(D0) - row(D0) == 1] = 1
  D = D0[1:(n - 1),]
  if (diff > 1) {
    for (i in 1:(diff - 1)) {
      D = D %*% D0
      D = D[1:(nrow(D) - 1),]
    }
  }
  return(D)
}

##### for other types of trend filter
##### mix l1 filter
cv.fit.l1tf.mix <- function(x.hist, x.fut, T1, T2, n.roll, n.lambda, k1=1, k2=2) {
  if (!(all(class(x.hist) %in% c('numeric', 'matrix')))) {
    x.hist = as.vector(x.hist)
  } 
  if (!(all(class(x.fut) %in% c('numeric', 'matrix')) | all(is.na(x.fut)))) {
    x.fut = as.vector(x.fut)
  }
  # compute lambda 1 and 2 saperately
  cv.result1 = cv.fit.l1tf(x.hist, x.fut, T1, T2, n.roll, n.lambda, k1)
  opt.lamb1 = cv.result1$best.lambda
  cv.result2 = cv.fit.l1tf(x.hist, x.fut, T1, T2, n.roll, n.lambda, k2)
  opt.lamb2 = cv.result2$best.lambda
  
  len.x = length(x.hist)
  T2 = min(cv.result1[['T2']], cv.result2[['T2']])
  te.v = x.fut[1:T2]
  tr.v = x.hist[(len.x - T1 + 1):len.x]
  
  tr.trd = l1tf.mix(tr.v, opt.lamb1, opt.lamb2, k1, k2)
  pr.trd = (1:T2) * (tr.trd[T1] - tr.trd[T1 - 1]) + tr.trd[T1]
  pr.trd = ifelse(pr.trd > 0, pr.trd, 0)
  
  pred.result = list(best.lambda1 = opt.lamb1, best.lambda2 = opt.lamb2,
                     train.data = tr.v, test.data = te.v,
                     train.trend = tr.trd, predicted.trend = pr.trd)
  return(pred.result)
}

##### hp filter
fit.hptf <- function(x.hist, x.fut, T1, T2, diff = 2) {
  if (!(all(class(x.hist) %in% c('numeric', 'matrix')))) {
    x.hist = as.vector(x.hist)
  } 
  if (!(all(class(x.fut) %in% c('numeric', 'matrix')) | all(is.na(x.fut)))) {
    x.fut = as.vector(x.fut)
  }
  len.x = length(x.hist)
  te.v = x.fut[1:T2]
  tr.v = x.hist[(len.x - T1 + 1):len.x]
  b.lamb = (T2 / 2 / pi)^4 / 2 * 10.27
  tr.trd = hptf(tr.v, b.lamb, diff)
  
  pr.trd = (1:T2) * (tr.trd[T1] - tr.trd[T1 - 1]) + tr.trd[T1]
  pr.trd = ifelse(pr.trd > 0, pr.trd, 0)
  
  pred.result = list(best.lambda = b.lamb,
                     train.data = tr.v, test.data = te.v,
                     train.trend = tr.trd, predicted.trend = pr.trd)
  return(pred.result)
}

##### moving average filter
fit.matf <- function(x.hist, x.fut, T1, T2) {
  if (!(all(class(x.hist) %in% c('numeric', 'matrix')))) {
    x.hist = as.vector(x.hist)
  } 
  if (!(all(class(x.fut) %in% c('numeric', 'matrix')) | all(is.na(x.fut)))) {
    x.fut = as.vector(x.fut)
  }
  # len.x = length(x.hist)
  te.v = x.fut[1:T2]
  tr.trd = matf(x.hist, T1)
  
  pr.trd = (1:T2) * (tr.trd[length(x.hist)] - tr.trd[length(x.hist) - 1]) + tr.trd[length(x.hist)]
  pr.trd = ifelse(pr.trd > 0, pr.trd, 0)
  
  pred.result = list(train.data = x.hist, test.data = te.v,
                     train.trend = tr.trd, predicted.trend = pr.trd)
  return(pred.result)
}