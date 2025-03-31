rm(list = ls())

library(tidyverse)
library(readxl)
library(rstan)
library(bayesforecast)

FinalData <- read_excel("Data/FinalData.xlsx")
FinalData <- FinalData |> dplyr::select(-1) |> dplyr::select(-Licenses) |> dplyr::select(-c(2,3,4,5,6))
FinalData$Lead_STF_Real <- FinalData$Lead_STF_Real * (3.85/4.25)
FinalData$EDUHS[1:12] <- FinalData$EDUHS[1:12] - 5
FinalData$SG[1:12] <- FinalData$SG[1:12] + 5

# Time series object
ts.object <- ts(FinalData$Lead_STF_Real, start = c(2012, 1), frequency = 12)
# Bayesian model
sf1 <- stan_sarima(ts = ts.object, order = c(0,0,0), seasonal = c(1,1,1),
                   prior_mu0 = student(mu = 0.03519496,sd = 0.005429012,df = 155))
# Bayesian forecasting
Stan_SalesTax_Forecast <- forecast(sf1, h = 12)
Stan_SalesTax_Forecast_2025 <- as.data.frame(Stan_SalesTax_Forecast)
View(Stan_SalesTax_Forecast_2025)

forecast_plot1 <- autoplot(Stan_SalesTax_Forecast)
forecast_plot1


# SARIMA object
sf2 <- auto.sarima(ts = FinalData$Lead_STF_Real, seasonal = TRUE, iter = 5000, chains = 4)
# regular SARIMA forecasting model
Sarima_SalesTax_Forecast <- forecast(object = sf2, h = 12)
Sarima_SalesTax_Forecast_2025 <- as.data.frame(Sarima_SalesTax_Forecast)
View(Sarima_SalesTax_Forecast_2025)
forecast_plot2 <- autoplot(Sarima_SalesTax_Forecast)
forecast_plot2

# Using stan_sarima yields more realistic results than auto.sarima.
# However, the minimum sales tax value seems improbable, and might be influenced by the drastic dip from 2020.
# Stan_sarima maintains the upward trend of sales tax, while auto.sarima is more stationary in nature.
# Waiting for the ARMA unit in ECON436