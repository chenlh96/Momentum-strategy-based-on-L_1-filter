##############################
##### define trend filter
##### function: hp filtering
hptf <- function(x, lambda, diff=2) {
  n.obs = length(x)
  if (diff == 2) {
    D.hp = diag(-2, n.obs)
    D.hp[abs(row(D.hp) - col(D.hp)) == 1] = 1
    D.hp = D.hp[2:(n.obs - 1),]
  }  else if (diff == 1) {
    D.hp = diag(-1, n.obs)
    D.hp[col(D.hp) - row(D.hp) == 1] = 1
    D.hp = D.hp[1:(n.obs - 1),]
  }
  
  trend.hp = solve(diag(n.obs) + 2 * lambda * t(D.hp) %*% D.hp) %*% x
  trend.hp = trend.hp[,1]
  return(trend.hp)
} 

##### function: l1 filtering with first difference of ts
l1tf.diff1 <- function(x, lambda) {
  n.obs = length(x)
  
  library(quadprog)
  D = diag(-1, n.obs)
  D[col(D) - row(D) == 1] = 1
  D = D[1:(n.obs - 1),]
  Dmat = D %*% t(D)
  dvec = matrix(x, nrow = 1) %*% t(D)
  dvec = dvec[1,]
  Amat = t(rbind(diag(n.obs - 1), diag(-1, n.obs - 1)))
  bvec = rep(-lambda, 2 * (n.obs - 1))
  
  solu = solve.QP(Dmat, dvec, Amat, bvec)
  v = solu[['solution']]
  trend = x - t(D) %*% v
  return(trend[,1])
}

l1tf.mix <- function(x, lambda1, lambda2) {
  n.obs = length(x)
  
  # D11 = diag(-1, n.obs)
  # D11[col(D11) - row(D11) == 1] = 1
  # D1 = D11[1:(n.obs - 1),]
  # D2 = D1 %*% D11
  # D2 = D2[1:(n.obs - 2),]
  # D = rbind(D1, D2)
  # Dmat = D %*% t(D)
  # dvec = matrix(x, nrow = 1) %*% t(D)
  # Amat = rbind(diag(2 * n.obs - 3), diag(-1,2 * n.obs - 3))
  # bvec = c(rep(-lambda1, n.obs - 1), rep(-lambda2, n.obs - 2))
  # bvec = rep(bvec, 2)
  
  library(CVXR)
  # v = Variable(2 * n.obs - 3)
  # obj.tf = 0.5 * quad_form(v, Dmat) - dvec %*% v
  # constrt = list(Amat %*% v >= bvec)
  # p.tf = Problem(Minimize(obj.tf), constrt)
  # v = solve(p.tf)$getValue(v)
  # trend = x - t(D) %*% v
  # return(trend)
  
  trend = Variable(n.obs)
  if (lambda1 != 0 & lambda2 != 0) {
    obj.tf = 0.5 * sum_squares(x - trend) + lambda1 * tv(trend) + lambda2 * p_norm(diff(trend, differences=2), 1)
  } else if (lambda1 == 0 & lambda2 != 0) {
    obj.tf = 0.5 * sum_squares(x - trend) + lambda2 * p_norm(diff(trend, differences=2), 1)
  } else if (lambda1 != 0 & lambda2 == 0) {
    obj.tf = 0.5 * sum_squares(x - trend) + lambda1 * tv(trend)
  }
  
  p.tf = Problem(Minimize(obj.tf))
  trend = solve(p.tf)$getValue(trend)
  return(trend)
}

##############################
##### Implement cv procedure for the l1 filter
##### the cv function
cv.l1tf <- function(x, T1, T2, n.roll, n.lambda, diff = 2) {
  len.x = length(x)
  Tg = ifelse(length(T2) == 1, T2, T2[1])
  Tl = ifelse(length(T2) == 1, 0, T2[2])
  len.cv = n.roll * Tg
  cv.i = (1:len.x)[(len.x - len.cv + 1 - Tg):(len.x - Tg)]
  
  cv.i.sp = matrix(cv.i, ncol = n.roll)
  cv.x = apply(cv.i.sp, 2, function (i) x[i])
  tr.x = apply(cv.i.sp, 2, function (i) x[(i[1] - T1):(i[1] - 1)])
  te.x = x[-Tg]
  tr.trend = cv.l1tf.run(tr.x, cv.x, te.x, T1, Tg, n.roll, n.lambda, diff)
  
  if (Tl != 0 & Tg > Tl) {
    cv.xl = apply(cv.i.sp, 2, function (i) x[i[1]:(i[1] + Tl)])
    te.xl = x[-Tl]
    tr.trend = cv.l1tf.run(tr.x, cv.xl, te.xl, T1, Tl, n.roll, n.lambda, diff)
  }
  
  return(tr.trend)
}

##### A core function for cv, can both handle the long and short term trend
cv.l1tf.run <- function(tr.set, cv.set, te.set, T1, T2, n.roll, n.lambda, diff = 2) {
  D11 = diag(-1, T2)
  D11[col(D11) - row(D11) == 1] = 1
  if (diff == 1) {
    D = D11[1:(T2 - 1),]
  } 
  if (diff == 2) {
    D1 = D11[1:(T2 - 1),]
    D2 = D1 %*% D11
    D = D2[1:(T2 - 2),]
  }
  cv.lamb = apply(cv.set, 2, function(x1) max(solve(D %*% t(D)) %*% D %*% x1))
  lamb.bd = c(max(mean(cv.lamb) - 2 * sd(cv.lamb), 1), mean(cv.lamb) + 2 * sd(cv.lamb))
  lambs = lamb.bd[1] * (lamb.bd[2] / lamb.bd[1])**(1:n.lambda/n.lambda)
  
  tr.trend = list()
  if (diff == 2) {
    library(l1tf)
    tr.trend = lapply(lambs, function (l) apply(tr.set, 2, function (x1) l1tf(x1, l)))
  } else if (diff == 1) {
    tr.trend = lapply(lambs, function (l) apply(tr.set, 2, function (x1) l1tf.diff1(x1, l)))
  }
  
  i.prod = data.matrix(expand.grid(1:n.lambda, 1:n.roll))
  err = matrix(0, n.lambda, n.roll)
  for (i in (1:nrow(i.prod))) {
    tr.trd = tr.trend[[i.prod[i, 1]]][,i.prod[i, 2]]
    te.pri = te.set[i.prod[i, 2]]
    
    # compute predicted trend
  }
  
  return(tr.trend)
}