library(tidyr)
library(dplyr)
data <- read.csv("Batting Data.csv")

## Add Free Base Column, Extra Base Hit Column, and Eliminate Covid Season

data <- data %>%
  mutate(Free_Bases = rowSums(data[c(13, 14, 16)], na.rm=TRUE)) %>%
  filter(Year != 2020) %>%
  mutate(XBH = X2B + X3B)
head(data)
## Create Logs of Hits and Runs

log_data <- data %>%
  mutate(across(c(X1B, X2B, X3B, HR, R, Free_Bases), log))


## Creating List of Years for Later
years <- c(2021, 2019, 2018, 2017, 2015, 2014, 2013, 2012)

## Standard Linear Weights
linear_summaries <- list()

# Create Linear Models
for (year in years) {
  
  linmodel <- lm(R ~ X1B + X2B + X3B + HR + Free_Bases, data = data)
  linmodel_summary <- summary(linmodel)
  
  linear_summaries[[as.character(year)]] <- linmodel_summary
}

# Make Predictions based on Linear Weights
linear_predictions_data <- data.frame()
for (year in years) {
  
  ## Filter Data For Later
  datatemp <- data %>%
    filter(Year == year)
  
  ## Save Coefficients
  model_summary <- model_summaries[[as.character(year)]]
  coefficients <- coef(model_summary)
  
  temp_linear_data <- datatemp %>%
    # Create Predictions using Coefficients
    mutate(predicted_runs = coefficients[1] +
              (coefficients[2]*X1B) +
              (coefficients[3]*X2B) +
              (coefficients[4]*X3B) +
              (coefficients[5]*HR) +
              (coefficients[6]*Free_Bases)) %>%
    # Create Relevant Columns
    mutate(Actual_Runs = datatemp$R) %>%
    mutate(Error = Actual_Runs - predicted_runs) %>%
    mutate(ErrPer = abs(Error / Actual_Runs))
  
  # Find Mean Absolute Error
  mae <- mean(temp_linear_data$ErrPer)
  # Save MAE
  linear_predictions_data <- linear_predictions_data %>%
    bind_rows(data.frame(Year = year, MAEs = mae))
  
}
head(temp_linear_data)
head(linear_predictions_data)
mean(linear_predictions_data$MAEs)

## Cobb-Douglas Function Weights

#  Create and Save Predictions

model_summaries <- list()

for (year in years) {

  CDmodel <- lm(R ~ X1B + X2B + X3B + HR + Free_Bases, data = log_data)
  model_summary <- summary(CDmodel)
  
  model_summaries[[as.character(year)]] <- model_summary
}


# Make predictions using the coefficients and filtered dataset

predictions_data <- data.frame()
for (year in years) {
  
  ## Filter Data For Later
  datatemp <- data %>%
    filter(Year == year)
  
  ## Save Coefficients
  model_summary <- model_summaries[[as.character(year)]]
  coefficients <- coef(model_summary)
  
  
  temp_log_data <- log_data %>%
    # Only Keep Correct Years
    filter(Year == year) %>%
    # Create Predictions using Coefficients
    mutate(predicted_runs = (exp(coefficients[1])*
                               exp(coefficients[2]*X1B)*
                               exp(coefficients[3]*X2B)*
                               exp(coefficients[4]*X3B)*
                               exp(coefficients[5]*HR)*
                               exp(coefficients[6]*Free_Bases))) %>%
    # Create Relevant Columns
    mutate(Actual_Runs = datatemp$R) %>%
    mutate(Error = Actual_Runs - predicted_runs) %>%
    mutate(ErrPer = abs(Error / Actual_Runs))
  
  # Find Mean Absolute Error
  mae <- mean(temp_log_data$ErrPer)
  # Save MAE
  predictions_data <- predictions_data %>%
    bind_rows(data.frame(Year = year, MAEs = mae))
  
}

print(predictions_data)
average_MAE <- mean(predictions_data$MAEs)
print(average_MAE)


## Reg test

