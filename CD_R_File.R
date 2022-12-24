library(tidyr)
library(dplyr)
data <- read.csv("Batting Data.csv")
backtest <- read.csv("Backtesting Data.csv")

head(backtest)

## Adding free base column
data <- data %>%
  mutate(Free_Bases = rowSums(data[c(13, 14, 16)], na.rm=TRUE)) %>%
  filter(Year != 2020)

head(data)

backtest <- backtest %>%
  mutate(Free_Bases = rowSums(backtest[c(13, 14, 16)], na.rm=TRUE))

head(backtest)

log_data <- data %>%
  mutate(X1B = log(X1B)) %>%
  mutate(X2B = log(X2B)) %>%
  mutate(X3B = log(X3B)) %>%
  mutate(HR = log(HR)) %>%
  mutate(R = log(R)) %>%
  mutate(Free_Bases = log(Free_Bases))

head(log_data)

## Running linear regression
reg1 <- lm(R~X1B+X2B+X3B+HR+Free_Bases, data = log_data)
summary(reg1)

head(data)
head(log_data)

## Predicting Runs scored
log_data <- log_data %>%
  mutate(predicted_log = predict(reg1))

coef_matrix <- summary(reg1)$coefficients
head(coef_matrix)

estimates <- coef_matrix[ , 1]
estimates

B0 = -1.688192878
B1 = .45519343
B2 = .299159812
B3 = .009971875
B4 = .352165295
B5 = .256378709

log_data <- log_data %>%
  mutate(predicted_runs = (exp(B0)*exp(B1*X1B)*exp(B2*X2B)*exp(B3*X3B)*
                             exp(B4*HR)*exp(B5*Free_Bases)))

data <- data %>%
  mutate(predicted_runs = log_data$predicted_runs) %>%
  mutate(resid = abs(R - predicted_runs)) %>%
  mutate(error_percent = abs(1- (predicted_runs/R)))

head(data)

## Checking Significance 
average_resid <- mean(data$resid)
average_error_percent <- mean(data$error_percent)

max_error <- max(data$error_percent)
max_resid <- max(data$resid)

average_resid
average_error_percent

max_resid
max_error

## Back testing ##
backtest <- backtest %>%
  mutate(predicted_runs = (exp(B0) * X1B^B1 * X2B^B2 * X3B^B3 * HR^B4 * Free_Bases^B5 )) %>%
  mutate(resid = abs(R - predicted_runs)) %>%
  mutate(error_percent = abs(1- (predicted_runs/R)))

bt_average_resid <- mean(backtest$resid)
bt_average_error_percent <- mean(backtest$error_percent)

bt_max_error <- max(backtest$error_percent)
bt_max_resid <- max(backtest$resid)

bt_average_resid
bt_average_error_percent

bt_max_resid
bt_max_error

## Train on Triples + Doubles ##

data2 <- data %>%
  mutate(Free_Bases = rowSums(data[c(13, 14, 16)], na.rm=TRUE)) %>%
  mutate(Xtra = rowSums(data[c(8, 9)], na.rm = TRUE)) %>%
  filter(Year != 2020)
head(data2)

log_data2 <- data2 %>%
  mutate(X1B = log(X1B)) %>%
  mutate(Xtra = log(Xtra)) %>%
  mutate(HR = log(HR)) %>%
  mutate(R = log(R)) %>%
  mutate(Free_Bases = log(Free_Bases))

reg2 <- lm(R~X1B+Xtra+HR+Free_Bases, data = log_data2)
summary(reg2)

A0 <- -1.77501
A1 <- .45762
A2 <- .30325
A3 <- .36156
A4 <- .25684

backtest2 <- backtest %>%
  mutate(Xtra = rowSums(backtest[c(8, 9)], na.rm = TRUE)) %>%
  mutate(predicted_runs = (exp(A0) * X1B^A1 * Xtra^A2 * HR^A3 * Free_Bases^A4)) %>%
  mutate(resid = abs(R - predicted_runs)) %>%
  mutate(error_percent = abs(1- (predicted_runs/R)))

bt2_average_resid <- mean(backtest2$resid)
bt2_average_error_percent <- mean(backtest2$error_percent)

bt2_max_error <- max(backtest2$error_percent)
bt2_max_resid <- max(backtest2$resid)
bt2_mdn_resid <- median(backtest2$resid)
bt2_mdn_error <- median(backtest2$error_percent)

bt2_average_resid
bt2_average_error_percent
bt2_mdn_resid
bt2_mdn_error

bt2_max_resid
bt2_max_error

## Run Difference and Summary Stats ##

# Top 10 Filters
# +/-1 Places
bt2_filter1 <- backtest2 %>%
  filter(resid <= 13.1)

nrow(bt2_filter1)/nrow(backtest2)

# +/- 2 Places
bt2_filter2 <- backtest2 %>%
  filter(resid <= 26.2)

nrow(bt2_filter2)/nrow(backtest2)

# League Wide Filters
# +/- 1 Place
bt2_filter3 <- backtest2 %>%
  filter(resid <= 10)

nrow(bt2_filter3)/nrow(backtest2)

# +/- 2 Places
bt2_filter4 <- backtest2 %>%
  filter(resid <= 20)

nrow(bt2_filter4)/nrow(backtest2)

## Quantiles

IQR(backtest2$resid)
quantile(backtest2$resid, .25)
quantile(backtest2$resid, .75)
quantile(backtest2$resid, .67)

## Regression Table ##
reg_table <- as.data.frame(summary(reg2)$coefficients)

reg_table