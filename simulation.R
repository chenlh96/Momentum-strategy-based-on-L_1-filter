wd = 'D:\\Codes\\Code Repositories\\filter'
setwd(wd)
source('filter.R')

##############################
##### simulation model
##### 1. straight trend lines with a white noise perturbation
n.obs = 2000
p = 0.99
b = 0.5
sigma = 15
signal = rep(0, n.obs)
proba.change = rbinom(n.obs, 1, p)
signal[1] = b * (runif(1) - 0.5)
for (i in (2:n.obs)) {
  signal[i] = ifelse(proba.change[i] == 1, signal[i - 1], b * (runif(1) - 0.5))
}
signal[1] = 0
signal = cumsum(signal)
signal.nz = signal + rnorm(n.obs, sd = sigma)

# trend filtering via l1 filter and hp filter
lamb.l1 = 5200
lamb.hp = 1500000

# l1 filter
trend.l1 = l1tf(signal.nz, lambda = 5200)
trend.l1.mix = l1tf.mix(signal.nz, lambda1 = lamb.l1, lambda2 = 5000)
# hp filter
trend.hp = hptf(signal.nz, lamb.hp)

# plot
par(mfrow=c(2,3))
plot(signal, type = 'l', main = 'signal', col="red")
plot(signal.nz, type = 'l', main = 'signal + noise', col="red")
plot(trend.l1, type = 'l', main = 'L1-T', col="blue")
plot(trend.l1.mix, type = 'l', main = 'L1-CT', col="blue")
plot(trend.hp, type = 'l', main = 'HP', col="green")

##### 2. random walk
p = 0.993
b = 5
sigma = 15
signal = rep(0, n.obs)
rnd.walk = rep(0, n.obs)
proba.change = rbinom(n.obs, 1, p)
rnd.walk[1] = b * (runif(1) - 0.5)
for (i in (2:n.obs)) {
  rnd.walk[i] = ifelse(proba.change[i] == 1, rnd.walk[i - 1], b * (runif(1) - 0.5))
  signal[i] = signal[i - 1] + rnd.walk[i] + rnorm(1, sd = sigma)
}
signal.nz = signal  # + rnorm(n.obs, sd = sigma)

# trend filtering via l1 filter and hp filter
lamb.l1 = 5200
lamb.hp = 1500000

# l1 filter
trend.l1 = l1tf(signal.nz, lambda = lamb.l1)
trend.l1.mix = l1tf.mix(signal.nz, lambda1 = lamb.l1, lambda2 = 5000)
# hp filter
trend.hp = hptf(signal.nz, lamb.hp)

# plot
par(mfrow=c(2,3))
plot(signal, type = 'l', main = 'signal', col="red")
plot(signal.nz, type = 'l', main = 'signal + noise', col="red")
plot(trend.l1, type = 'l', main = 'L1-T', col="blue")
plot(trend.l1.mix, type = 'l', main = 'L1-CT', col="blue")
plot(trend.hp, type = 'l', main = 'HP', col="green")

##### 3. step trend lines perturbed by a white nz process
p = 0.998
b = 50
sigma = 8
signal = rep(0, n.obs)
signal[1] = b * (runif(1) - 0.5)
proba.change = rbinom(n.obs, 1, p)
for (i in (2:n.obs)) {
  signal[i] = ifelse(proba.change[i] == 1, signal[i - 1], b * (runif(1) - 0.5))
}
signal.nz = signal + rnorm(n.obs, sd = sigma)

# trend filtering via l1 filter and hp filter
lamb.l1 = 100
lamb.hp = 12000

# l1 filter
trend.l1 = l1tf.diff1(signal.nz, lambda = lamb.l1)
# hp filter
trend.hp = hptf(signal.nz, lamb.hp, diff=1)

# plot
par(mfrow=c(2,2))
plot(signal, type = 'l', main = 'signal', col="red")
plot(signal.nz, type = 'l', main = 'signal + noise', col="red")
plot(trend.l1, type = 'l', main = 'L1-C', col="blue")
plot(trend.hp, type = 'l', main = 'HP', col="green")

##### 4. O-U process
p = 0.9985
b = 20
theta = 0.1
sigma = 2
signal.mean = rep(0, n.obs)
signal = rep(0, n.obs)
signal[1] = b * (runif(1) - 0.5)
signal.mean[1] = b * (runif(1) - 0.5)
proba.change = rbinom(n.obs, 1, p)
for (i in (2:n.obs)) {
  signal.mean[i] = ifelse(proba.change[i] == 1, signal.mean[i - 1], b * (runif(1) - 0.5))
  signal[i] = (1 - theta) * signal[i - 1]
}
signal = signal.mean + theta * signal.mean
signal.nz = signal + rnorm(n.obs, sd = sigma)

# trend filtering via l1 filter and hp filter
lamb.l1 = 800
lamb.hp = 12000

# l1 filter
trend.l1 = l1tf.diff1(signal.nz, lambda = lamb.l1)
# hp filter
trend.hp = hptf(signal.nz, lamb.hp, diff=1)

# plot
par(mfrow=c(2,2))
plot(signal, type = 'l', main = 'signal', col="red")
plot(signal.nz, type = 'l', main = 'signal + noise', col="red")
plot(trend.l1, type = 'l', main = 'L1-C', col="blue")
plot(trend.hp, type = 'l', main = 'HP', col="green")

##### 5. GBM for price
mu = 0.05
sigma = 4
dt = 1 / n.obs
bm = rnorm(n.obs)
price.lat = rep(0, n.obs)
price.lat[1] = 40
for (i in (2:n.obs)) {
  price.lat[i] = price.lat[i - 1] * (1 + mu * dt + sigma * sqrt(dt) * bm[i])
}
price = price.lat + rnorm(n.obs)

# trend filtering via l1 filter and hp filter
lamb.l1 = 2000
lamb.hp = 12000
# l1 filter
trend.l1t = l1tf(price, lambda = lamb.l1)
trend.l1c = l1tf.diff1(price, lambda = lamb.l1 * 5)
trend.l1tc = l1tf.mix(price, lambda1 = lamb.l1, lambda2 = 1000,1, 2)
trend.l1sp = l1tf.sparse(price, lambda1 = 25000, lambda2 = 500, 2)
# hp filter
# trend.hp = hptf(price, lamb.hp, diff=2)

# plot
par(mfrow=c(2,3))
plot(price.lat, type = 'l', main = 'latent price', col="red")
plot(price, type = 'l', main = 'price', col="red")
# plot(trend.hp, type = 'l', main = 'HP')
plot(trend.l1t, type = 'l', main = 'L1-T', col="blue")
plot(trend.l1c, type = 'l', main = 'L1-C', col="blue")
plot(trend.l1tc, type = 'l', main = 'L1-TC', col="blue")


##### 6. Heston model
mu = 0.05
kappa = 10
alpha = 0.1
rho = -0.5
# sigma.jp = 0.015 # for jumps
# lambda.jp = 2
xi = 1
dt = 1 / n.obs

library(MASS)

bm = mvrnorm(n.obs, rep(0, 2), diag(2))
bm.pri = bm[,1]
bm.vol = bm[,2]
price.lat = rep(0, n.obs)
price.lat[1] = log(40)
vol.hest = rep(0, n.obs)
for (i in (2:n.obs)) {
  prev.vol = max(vol.hest[i - 1], 0)
  vol.hest[i] = prev.vol + kappa * (alpha - prev.vol) * dt + xi * sqrt(prev.vol * dt) * bm.vol[i]
  price.lat[i] = price.lat[i - 1] * (1 + (mu - prev.vol / 2) * dt + sqrt(prev.vol * dt) * bm.pri[i])
}
price = exp(price.lat) + rnorm(n.obs)

# trend filtering via l1 filter and hp filter
lamb.l1 = 500
lamb.hp = 12000
# l1 filter
trend.l1t = l1tf(price, lambda = lamb.l1)
trend.l1c = l1tf.diff1(price, lambda = lamb.l1 + 200)
trend.l1tc = l1tf.mix(price, lambda1 = lamb.l1 / 4, lambda2 = lamb.l1 * 2)
# hp filter
trend.hp = hptf(price, lamb.hp)

# plot
par(mfrow=c(2,3)) 
plot(price.lat, type = 'l', main = 'latent price', col="red")
plot(price, type = 'l', main = 'price', col="red")
plot(trend.l1t, type = 'l', main = 'L1-T', col="blue")
plot(trend.l1c, main = 'L1-C', type = 'l', col="blue")
plot(trend.l1tc, type = 'l', main = 'L1-TC', col="blue")