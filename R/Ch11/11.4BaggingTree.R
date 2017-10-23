########
# 装袋树
########

library(caret)
library(pROC)
dat <- read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# 将10个问卷调查变量当作自变量
trainx <- dat[, grep("Q", names(dat))]
# 将类别也作为自变量
# 不对消费者类别进行变换
trainx$segment <- dat$segment
# 性别作为应变量
trainy <- dat$gender

set.seed(100)
bagTune <- caret::train(trainx, trainy, 
                           method = "treebag",
                           nbagg = 1000,
                           metric = "ROC",
                           trControl = trainControl(method = "cv",
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE,
                           savePredictions = TRUE))
