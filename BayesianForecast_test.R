rm(list = ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(bayesforecast)

FinalData <- read_excel("Data/FinalData.xlsx")
# FinalData <- FinalData |> dplyr::select(-1) |> dplyr::select(-Licenses, -Lead_STF)
FinalData <- FinalData |> dplyr::select(-Licenses)
FinalData$Month <- month(FinalData$Month)
FinalData$Lead_STF_Real <- FinalData$Lead_STF_Real * (3.85/4.25)
FinalData$EDUHS[1:12] <- FinalData$EDUHS[1:12] - 5
FinalData$SG[1:12] <- FinalData$SG[1:12] + 5
FinalData <- FinalData[-nrow(FinalData),]


ldf <- log(FinalData)
numrow <- nrow(ldf)
ldf2023 <- ldf[1:(numrow- 12),]

ts.object_2023 <- ts(ldf2023$Lead_STF_Real, start = c(2012, 1), frequency = 12)

sf_2023 <- stan_sarima(ts = ts.object_2023, order = c(0,0,0), seasonal = c(1,1,1),
                   prior_mu0 = student(mu = 0, sd = 1, df = nrow(ldf2023)-1), chains = 5)
# Bayesian forecasting
Stan_SalesTax_Forecast <- forecast(sf_2023, h = 12)
Stan_SalesTax_Forecast_2024 <- as.data.frame(Stan_SalesTax_Forecast)
Stan_SalesTax_Forecast_2024$`Point Forecast` <- exp(Stan_SalesTax_Forecast_2024$`Point Forecast`)

Forecast2024 <- Stan_SalesTax_Forecast_2024[,1]
Forecast2024 <- as.data.frame(Forecast2024)
Forecast2024 <- Forecast2024 * (4.25/3.85)

Actual2024 <- FinalData[(nrow(FinalData) - 11): nrow(FinalData), "Lead_STF_Real"]

AE <- abs(Forecast2024 - Actual2024)
MAE <- mean(AE[1,])
print(round(MAE, 6))
# Error is lower without Lead_STF

# 2025
# Time series object
FinalData <- read_excel("Data/FinalData.xlsx")
FinalData <- FinalData |> dplyr::select(-1) |> dplyr::select(-Licenses, -Lead_STF)
FinalData$Lead_STF_Real <- FinalData$Lead_STF_Real * (3.85/4.25)
FinalData$EDUHS[1:12] <- FinalData$EDUHS[1:12] - 5
FinalData$SG[1:12] <- FinalData$SG[1:12] + 5

# ldf_all <- log(FinalData)
#
# ts.object <- ts(ldf_all$Lead_STF_Real, start = c(2012, 1), frequency = 12)
# # Bayesian model
# sf_2025 <- stan_sarima(ts = ts.object, order = c(0,0,0), seasonal = c(1,1,1),
#                    prior_mu0 = student(mu = 0, sd = 1, df = nrow(FinalData)-1))
# # Bayesian forecasting
# Stan_SalesTax_Forecast <- forecast(sf_2025, h = 11)
# Stan_SalesTax_Forecast_2025 <- as.data.frame(Stan_SalesTax_Forecast)
# Stan_SalesTax_Forecast_2025$`Point Forecast` <- exp(Stan_SalesTax_Forecast_2025$`Point Forecast`)
# Stan_SalesTax_Forecast_2025$`Point Estimate` <- Stan_SalesTax_Forecast_2025$`Point Estimate` * (4.25/3.85)
# View(Stan_SalesTax_Forecast_2025)
#
# forecast_plot1 <- autoplot(Stan_SalesTax_Forecast)
# forecast_plot1


ldf_all <- log(FinalData)

ts.object_2025 <- ts(ldf_all$Lead_STF_Real, start = c(2012, 1), frequency = 12)

sf_2025 <- stan_sarima(ts = ts.object_2025, order = c(0,0,0), seasonal = c(1,1,1),
                       prior_mu0 = student(mu = 0, sd = 1, df = nrow(ldf_all)-1))
# Bayesian forecasting
Stan_SalesTax_Forecast <- forecast(sf_2025, h = 11)
Stan_SalesTax_Forecast_2025 <- as.data.frame(Stan_SalesTax_Forecast)
Stan_SalesTax_Forecast_2025$`Point Forecast` <- exp(Stan_SalesTax_Forecast_2025$`Point Forecast`)

Forecast2025 <- Stan_SalesTax_Forecast_2025[,1]
Forecast2025 <- as.data.frame(Forecast2025)
Forecast2025 <- Forecast2025 * (4.25/3.85)

forecast_plot <- autoplot(Stan_SalesTax_Forecast)
forecast_plot





ts.object_2025_noLog <- ts(FinalData$Lead_STF_Real, start = c(2012, 1), frequency = 12)

sf_2025noLog <- stan_sarima(ts = ts.object_2025noLog, order = c(0,0,0), seasonal = c(1,1,1),
                       prior_mu0 = student(mu = 0, sd = 1, df = nrow(FinalData)-1))
# Bayesian forecasting
Stan_SalesTax_Forecast <- forecast(sf_2025noLog, h = 11)
Stan_SalesTax_Forecast_2025 <- as.data.frame(Stan_SalesTax_Forecast)
#Stan_SalesTax_Forecast_2025$`Point Forecast` <- exp(Stan_SalesTax_Forecast_2025$`Point Forecast`)

Forecast2025 <- Stan_SalesTax_Forecast_2025[,1]
Forecast2025 <- as.data.frame(Forecast2025)
Forecast2025 <- Forecast2025 * (4.25/3.85)

forecast_plot <- autoplot(Stan_SalesTax_Forecast)
forecast_plot

