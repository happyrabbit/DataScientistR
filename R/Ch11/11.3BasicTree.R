################
## 回归树和决策树
################

##########
### 回归树
##########

library(rpart)
library(tree)
dat <- read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# 对数据进行一些清理，删除错误的样本观测，消费金额不能为负数
dat <- subset(dat, store_exp > 0 & online_exp > 0)
# 将10个问卷调查变量当作自变量
trainx <- dat[, grep("Q", names(dat))]
# 将实体店消费量和在线消费之和当作应变量
# 得到总消费量=实体店消费+在线消费
trainy <- dat$store_exp + dat$online_exp
set.seed(100)
rpartTune <- train(trainx, trainy,
                   method = "rpart2",
                   tuneLength = 10,
                   trControl = trainControl(method = "cv"))
plot(rpartTune)

rpartTree <- rpart(trainy ~ ., data = trainx, maxdepth = 2)

print(rpartTree)

library(partykit)
rpartTree2 <- as.party(rpartTree)
plot(rpartTree2)

##########
### 决策树
##########

library(caret)
library(pROC)
dat <- read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# 将10个问卷调查变量当作自变量
trainx1 <- dat[, grep("Q", names(dat))]
# 将类别也作为自变量
# 这里用两种方法编码分类变量
# trainx1 不对消费者类别进行变换
trainx1$segment <- dat$segment
# trainx2 中的消费者类别被转化成虚拟变量
dumMod<-dummyVars(~.,
                  data=trainx1,
                  # 用原变量名加上因子层级的名称作为新的名义变量名
                  levelsOnly=F)
trainx2 <- predict(dumMod,trainx1)
# 性别作为应变量
trainy <- dat$gender

set.seed(100)
rpartTune1 <- caret::train(trainx1, trainy, method = "rpart",
                       tuneLength = 30,
                       metric = "ROC", 
                       # 规定了预留数据集以及需要计算那些模型表现度量（如敏感度，特异度和AUC）。
                       trControl = trainControl(method = "cv",
                                                summaryFunction = twoClassSummary,
                                                classProbs = TRUE,
                                                savePredictions = TRUE))
rpartTune1


rpartTune2 <- caret::train(trainx2, trainy, method = "rpart",
                       tuneLength = 30,
                       metric = "ROC", 
                       # 规定了预留数据集以及需要计算那些模型表现度量（如敏感度，特异度和AUC）。
                       trControl = trainControl(method = "cv",
                                                summaryFunction = twoClassSummary,
                                                classProbs = TRUE,
                                                savePredictions = TRUE))
rpartRoc <- roc(response = rpartTune1$pred$obs,
                predictor = rpartTune1$pred$Female,
                levels = rev(levels(rpartTune1$pred$obs)))

rpartFactorRoc <- roc(response = rpartTune2$pred$obs,
                      predictor = rpartTune2$pred$Female,
                      levels = rev(levels(rpartTune1$pred$obs)))

plot(rpartRoc, type = "s", print.thres = c(.5),
     print.thres.pch = 3,
     print.thres.pattern = "",
     print.thres.cex = 1.2,
     col = "red", legacy.axes = TRUE,
     print.thres.col = "red")

plot(rpartFactorRoc,
     type = "s",
     add = TRUE,
     print.thres = c(.5),
     print.thres.pch = 16, legacy.axes = TRUE,
     print.thres.pattern = "",
     print.thres.cex = 1.2)

legend(.75, .2,
       c("Grouped Categories", "Independent Categories"),
       lwd = c(1, 1),
       col = c("black", "red"),
       pch = c(16, 3))

library(partykit)
plot(as.party(rpartTune1$finalModel))

