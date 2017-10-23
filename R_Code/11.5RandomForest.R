#############
# 随机森林
#############

# 对入选的变量个数参数进行调优
mtryValues <- c(1:5)
set.seed(100)
rfTune <- train(x = trainx, 
               y = trainy,
               # 指定随机森模型
               method = "rf",
               ntree = 1000,
               tuneGrid = data.frame(.mtry = mtryValues),
               importance = TRUE,
               metric = "ROC",
               trControl = trainControl(method = "cv",
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           savePredictions = TRUE))

library(randomForest)
rfit = randomForest(trainy ~ ., trainx, mtry = 1, ntree = 1000)

importance(rfit)
varImpPlot(rfit)
