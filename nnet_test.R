library(tidyverse)
library(readxl)
library(neuralnet)

FinalData <- read_excel("Data/FinalData.xlsx") |> select(-Licenses)
FinalData$Lead_STF_Real <- FinalData$Lead_STF_Real* (3.85/4.25)
FinalData$EDUHS[1:12] <- FinalData$EDUHS[1:12] - 5
FinalData$SG[1:12] <- FinalData$SG[1:12] + 5
FinalData <- FinalData |> mutate(Month=lubridate::month(FinalData$Month))
FinalData <- na.omit(FinalData)

##
training <- sample_n(FinalData, nrow(FinalData)*.75)
test <- setdiff(FinalData, training)


nnet <- neuralnet(Lead_STF_Real ~ ., data = training)

# plot(nnet)

predictions <- predict(nnet, test)
##

#todo
library(randomForest)
rf <- randomForest(Lead_STF_Real~., data = FinalData)
imp <- importance(rf) |> order()
print(imp)
