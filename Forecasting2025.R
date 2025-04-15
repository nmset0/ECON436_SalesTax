# Nathan Seto
# 14 April 2025

# Packages
library(readxl)
library(tidyverse)
library(urca) # For ca.jo
library(tsDyn) # For VECM
library(vars)

# Data
FD_raw <- read_excel("~/ECON436_SalesTax/Data/FinalData.xlsx", sheet = 1)
TRAF_raw <- read_excel("~/ECON436_SalesTax/Data/TrafficVolume.xlsx") # Traffic data (external)
# Data from https://dtdapps.coloradodot.info/otis/TrafficData#ui/0/0/1/station/000127/criteria/27425//false/true/


# Adjustments
FD_1 <- FD_raw[1:157,]
  FD_1$Lead_STF_Real[FD_1$year == 2024] <- FD_1$Lead_STF_Real[FD_1$year == 2024] * (3.85/4.35)
    FD_1$SG[1:12]<-FD_1$SG[1:12]+5
      FD_1$G[1:12]<-FD_1$G[1:12]+5
        FD_1$EDUHS[1:12]<-FD_1$EDUHS[1:12]-5

# Cleaning traffic data
TRAF_clean <- TRAF_raw[TRAF_raw$Year >= 2012 & TRAF_raw$Year <= 2024, ]
TRAF_clean <- TRAF_clean[, -c(1, 15)] |> arrange(Year)

TRAF_long <- TRAF_clean |>
  pivot_longer(cols = -Year, names_to = "Month", values_to = "Value") |>
  mutate(
    Month = match(Month, month.abb),
    Month = as.Date(paste(Year, Month, "01", sep = "-")),
  ) |>
  dplyr::select(Month, Year, Value,)

#TRAF_long <- TRAF_long |> mutate(Date = Month, .before = Month)
TRAF_long <- TRAF_long |> mutate(Month = lubridate::month(TRAF_long$Month))

# Binding traffic data and FinalData
DF <- cbind(FD_1[1:nrow(TRAF_long),], TRAF_long$Value)
  DF <- DF |> mutate(Month = month(DF$Month))

# Selecting predictors + Lead_STF_Real
DF_NEW <- DF |> dplyr::select("LH", "INFO","EDUHS", "MAN", "AFS", "RT", "TRAF_long$Value", "Lead_STF_Real")
DF_NEW <- DF_NEW |> rename(traffic_frequency = `TRAF_long$Value`) # rename column

# Log all variables
LDF <- log(DF_NEW)


# Cointegrating vectors
# Conduct the Johansen procedure
JOH <- ca.jo(LDF, ecdet="none", type="eigen", K=12) # K = number of periods in a year (4 if quarterly data, 12 if monthly data)
summary(JOH)

BETA <- round(coefB(JOH, r=6), 3) # r = number of cointegrating vectors
BETA

# Short-run adjustment coefficients
VECM <- VECM(LDF, lag=11, r=6, estim="ML", include = "const") # lag = K-1
summary(VECM)


# vec2var: Transform a VECM to VAR in levels
vec2var_ca.jo <- vec2var(JOH, r=6)

nhor <- 11 # forecasting horizon

# Forecasting
pred_vec2var_ca.jo <- predict(vec2var_ca.jo, n.ahead=nhor)

# Graphical representation
par(mai=rep(0.4, 4)); plot(pred_vec2var_ca.jo)
par(mai=rep(0.4, 4)); fanchart(pred_vec2var_ca.jo)

# Predicted Values
pred_vec2var_ca.jo$fcst$Lead_STF_Real <- exp(pred_vec2var_ca.jo$fcst$Lead_STF_Real)
pred_vec2var_ca.jo$fcst$Lead_STF_Real <- pred_vec2var_ca.jo$fcst$Lead_STF_Real * (4.35/3.85)
print(pred_vec2var_ca.jo$fcst$Lead_STF_Real)


# Saving predictions
# FRAME <- as.data.frame(pred_vec2var_ca.jo$fcst$Lead_STF_Real)
# FRAME <- FRAME |> mutate(year = 2025, month = 1:11, .before = fcst)
# write_csv(FRAME, file = "Forecast2025-3.csv")


