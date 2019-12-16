# Momentum-strategy-based-on-L_1-filter

## Quick introduction

L-1 filter is a special trend filter that has important application in different area: finance, signal processing, etc. It uses L_1 norm of the difference of trend value given a special order as penalty and will find a trend that performs piecewise constant / linear / quadratic / cubic. We will apply the L_1 filter to tick data from HKEX, to gain the trend in tick data and build a momentum strategy.

## Usage

- simulation.R: run simulation for different model.
- empirical.R: apply various trend filters to real data, include S&P 500 index intraday data and tick data from HKEX.
- main.R: main code for backtesting momentum strategy.

## Simulation

We use two classic stochatic process that is used most in the assumption of log-price process to run the simulation, in order to test the performance of the L_1 filter: Geometric brownian motion and Heston model. HP filter, L_1 filter and its multiple variants are examined in sumulation.

- Geometric brownian motion

![GBM](https://github.com/chenlh96/Momentum-strategy-based-on-L_1-filter/raw/master/figures//gbm.png)

- Heston model

![Heston](https://github.com/chenlh96/Momentum-strategy-based-on-L_1-filter/raw/master/figures//heston.png)

## Empirical study

We use the tick data of stock in HKEX to evaluation whether the L_1 filter can obtain a clear trend in tick level data. We use the cross valiation algorithm to choose the best lambda automatically.

- 0005.HK, 2019-09-04 from 14:00 to 14:30, for L_1-T filter

![0005](https://github.com/chenlh96/Momentum-strategy-based-on-L_1-filter/raw/master/figures//0005_l1.png)

- 0007.HK, 2019-09-05 from 13:30 to 14:00, for L_1-TC filter

![0007](https://github.com/chenlh96/Momentum-strategy-based-on-L_1-filter/raw/master/figures//0007_l1_mix.png)

## Momentum strategy

### S&P 500

THe momentum strategy was built on L_1 filter, the preidction trend use linear extrapolation by the closet two estimation trend in training set. The testing period is 2000 to 2010, total 10 years. The results are

Model | trend | Return | Volatility | Sharpe ratio | maxdrawdown
 ---  | ---   | ------ | ---------- | ------------ | -----------
L_2   |       | 0.017  | 0.015      | 1.12         | 0.082
MA    |       | 0.017  | 0.015      | 1.13         | 0.082
L_1T  | LT    | 0.017  | 0.015      | 1.12         | 0.082 
L_1T  | GT    | 0.019  | 0.015      | 1.30         | 0.16
L_1T  | LGT   | 0.017  | 0.015      | 1.12         | 0.082 

### High frequency data

We build a simple trading strategy by taking the following rules:

1. use the previous 1 hour data as historical date (second half hour data as cross validation), to fit a L_1T filter and forecast the trend in next 5 minute
2. consider the slope of the predicted trend
  - if slope > 0, set position = 1
  - if slope < 0, set position = -1

The backtest environment is very simple, with a setting that not consider sllipage and transaction cost. Results are as follow:

- profit per share

Date | 0005.HK | 0700.HK
 --- | ------- | -------
2019-09-04 | 1.35 | -0.3
2019-09-05 | -3.2 | 0.2

## Todo list

2. the equation in email, exactly the same as the backtest problem I seek help from jiayang
3. How to use it to detect the momentem in 1 min data

## Reference

- T.L.Dao, 2014, [Momentum Strategies Based on L_1 Filter](https://arxiv.org/abs/1403.4069)

- S.-J. Kim, K. Koh, S. Boyd, and D. Gorinevsky, SIAM Review, 2009, [L_1 Trend Filtering](https://web.stanford.edu/~boyd/papers/pdf/l1_trend_filter.pdf)
