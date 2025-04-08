rm(list = ls())

library(tidyverse)
library(readxl)
library(lubridate)
library(bayesforecast)

FinalData_raw <- read_excel("Data/FinalData.xlsx")
  FinalData_1 <- FinalData_raw |> dplyr::select(-Licenses)
    FinalData_1$Month <- month(FinalData_1$Month)
      FinalData_1$Lead_STF_Real <- FinalData_1$Lead_STF_Real * (3.85/4.35)
        FinalData_1$EDUHS[1:12] <- FinalData_1$EDUHS[1:12] - 5
          FinalData_1$SG[1:12] <- FinalData_1$SG[1:12] + 5
            FinalData_2 <- FinalData_1[-nrow(FinalData_1),]

# 2024
ldf <- log(FinalData_2)
numrow <- nrow(ldf)
ldf2023 <- ldf[1:(numrow- 12),]

mu0 <- mean(ldf2023$Lead_STF_Real)
sd0 <- sd(ldf2023$Lead_STF_Real)

ts.object_2023 <- ts(ldf2023$Lead_STF_Real, start = c(2012, 1), frequency = 12)

sf_2023 <- stan_sarima(ts = ts.object_2023, order = c(0,0,0), seasonal = c(1,1,1),
                   prior_mu0 = student(mu = mu0, sd = mu0, df = nrow(ldf2023)-1), chains = 5)

# Bayesian forecasting
Stan_SalesTax_Forecast <- forecast(sf_2023, h = 12)
Stan_SalesTax_Forecast_2024 <- as.data.frame(Stan_SalesTax_Forecast)

Stan_SalesTax_Forecast_2024$`Point Forecast` <- exp(Stan_SalesTax_Forecast_2024$`Point Forecast`)

Forecast2024 <- Stan_SalesTax_Forecast_2024[,1]
Forecast2024 <- as.data.frame(Forecast2024)
Forecast2024 <- Forecast2024 * (4.25/3.85)

Actual2024 <- FinalData_1[(nrow(FinalData_1) - 11): nrow(FinalData_1), "Lead_STF_Real"]

df2024 <- cbind(Forecast2024, Actual2024)
df2024$AE <- abs(df2024$Forecast2024 - df2024$Lead_STF_Real)


AE <- abs(Forecast2024 - Actual2024)
MAE <- mean(AE[1,])
cat("MAE:", round(MAE, 6), "\n")
knitr::kable(df2024)


# 2025 Forecast
FinalData_3 <- FinalData_raw |> dplyr::select(-1) |> dplyr::select(-Licenses, -Lead_STF)
  FinalData_3$Lead_STF_Real <- FinalData_3$Lead_STF_Real * (3.85/4.35)
    FinalData_3$EDUHS[1:12] <- FinalData_3$EDUHS[1:12] - 5
      FinalData_3$SG[1:12] <- FinalData_3$SG[1:12] + 5


ldf_all <- log(FinalData_3)

ts.object_2025 <- ts(ldf_all$Lead_STF_Real, start = c(2012, 1), frequency = 12)

sf_2025 <- stan_sarima(ts = ts.object_2025, order = c(0,0,0), seasonal = c(1,1,1),
                       prior_mu0 = student(mu = 0, sd = 1, df = nrow(ldf_all)-1))
# Bayesian forecasting
Stan_SalesTax_Forecast <- forecast(sf_2025, h = 11)
Stan_SalesTax_Forecast_2025 <- as.data.frame(Stan_SalesTax_Forecast)
Stan_SalesTax_Forecast_2025$`Point Forecast` <- exp(Stan_SalesTax_Forecast_2025$`Point Forecast`)

Forecast2025 <- Stan_SalesTax_Forecast_2025[,1]
Forecast2025 <- as.data.frame(Forecast2025)
Forecast2025 <- Forecast2025 * (4.35/3.85)
#Forecast2025 <- Forecast2025 |> mutate(Month = 2:12, .before = Forecast2025)
knitr::kable(Forecast2025)
write.csv(Forecast2025, file = "Data/Forecast2025_2.csv")

forecast_plot <- autoplot(Stan_SalesTax_Forecast, ylab = "Log Lead_STF_Real")
forecast_plot





ts.object_2025_noLog <- ts(FinalData_3$Lead_STF_Real, start = c(2012, 1), frequency = 12)

sf_2025noLog <- stan_sarima(ts = ts.object_2025_noLog, order = c(0,0,0), seasonal = c(1,1,1),
                       prior_mu0 = student(mu = 0, sd = 1, df = nrow(FinalData_3)-1))
# Bayesian forecasting
Stan_SalesTax_Forecast <- forecast(sf_2025noLog, h = 11)
Stan_SalesTax_Forecast_2025 <- as.data.frame(Stan_SalesTax_Forecast)
#Stan_SalesTax_Forecast_2025$`Point Forecast` <- exp(Stan_SalesTax_Forecast_2025$`Point Forecast`)

Forecast2025 <- Stan_SalesTax_Forecast_2025[,1]
Forecast2025 <- as.data.frame(Forecast2025)
Forecast2025 <- Forecast2025 * (4.25/3.85)

forecast_plot <- autoplot(Stan_SalesTax_Forecast)
forecast_plot

