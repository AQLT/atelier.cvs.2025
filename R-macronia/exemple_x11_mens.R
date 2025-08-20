library(readxl)
macronia <- read_excel("data/macronia.xlsx", 
                       sheet = "IMAE")
View(macronia)
macronia <- ts(macronia[,-1], start = c(1990, 1), frequency = 12)
y <- macronia[,"C5"]

library(rjd3filters)
library(rjd3x11plus)
library(forecast)
autoplot(y)
e1 <- simple_ma(12, - 6)
e2 <- simple_ma(12, - 5)
tc_1 <- M2X12 <- (e1 + e2)/2

autoplot(y) +
  autolayer(M2X12 * y, color = "red")

si_1 <- 1 - tc_1
si_1 * y
autoplot(si_1 * y)

M3X3 <- macurves("S3x3")
M3X3_s <- to_seasonal(M3X3, 12)
s_1 <- M3X3_s * si_1
s_1_norm <- M2X12 * s_1
s_1_norm <- impute_last_obs(s_1_norm, n = 6, nperiod = 1)
s_1_demean <- s_1 - s_1_norm
s_1_f <- impute_last_obs(s_1_demean, n = 6, nperiod = 12)
autoplot(si_1 * y) +
  autolayer(s_1_f * y, color = "red") 

autoplot(s_1 * y) +
  autolayer(s_1_f * y, color = "red") 

sa_1 <- 1- s_1_f
autoplot(y) +
  autolayer(sa_1 * y, color = "red")

h13 <- lp_filter(horizon = 6, ic = 3.5)
tc_2 <- h13 * sa_1
autoplot(sa_1 * y) +
  autolayer(tc_2 * y, color = "red")
  autoplot(M2X12 * y) +
    autolayer(tc_2 * y, color = "red")
    

si_2 <- 1 - tc_2
autoplot(si_2 * y) +
  autolayer(si_1 * y, color = "red")


M3X5 <- macurves("S3x5")
M3X5_s <- to_seasonal(M3X5, 12)
s_2 <- M3X5_s * si_2
s_2_norm <- M2X12 * s_2
s_2_norm <- impute_last_obs(s_2_norm, n = 6, nperiod = 1)
s_2_demean <- s_2 - s_2_norm
s_2_f <- impute_last_obs(s_2_demean, n = 6, nperiod = 12)
autoplot(s_2 * y) +
  autolayer(s_2_f * y, color = "red") 

sa_2 <- 1 - s_2_f
autoplot(sa_2 * y)  +
  autolayer(sa_1 * y, color = "red")

