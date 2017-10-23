#######################
# 用CARET包训练神经网络
#######################

################
## 普通神经网络
################

# 这是我自己写的一个包，没有安装的可以通过下面这行代码安装
# devtools::install_github("happyrabbit/DataScienceR")
library(DataScienceR)
data("sim1_da1")
trainx = dplyr::select(sim1_da1, -y)
trainy = paste0("BREAK",sim1_da1$y)

library(caret)
nnetGrid <- expand.grid(decay = c(0, 0.01, .1),
                        size = c(1:10))
# 得到最大的隐变量个数
maxSize <- max(nnetGrid$size)
# 计算最大隐变量个数对应的参数个数
# 之前讲过，参数一共M(p+1)+M+1个
numWts <- 1*(maxSize * (length(trainx) + 1) + maxSize + 1)
# 为了保证结果的可重复性，这里设置一个随机种子
set.seed(2017)

ctrl <- trainControl(method = "cv", 
                     number = 10, 
                     classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

nnetTune <- train(trainx, trainy,
                   method = "nnet",
                   tuneGrid = nnetGrid,
                   trControl = ctrl,
                   preProc = c("center", "scale"),
                   trace = FALSE,
                   MaxNWts = numWts, 
                   maxit = 500)

nnetTune
plot(nnetTune)

#######################
## bootstrap平均神经网络
#######################

# 这里运行速度比较慢
trainx = dplyr::select(sim1_da1, -y)
trainy = paste0("BREAK",sim1_da1$y)

avnnetGrid <- expand.grid(decay = c(0, 0.01, .1),
                        size = c(1:10),
                        bag = TRUE)
                        
avnnetTune <- train(trainx, trainy,
                   method = "avNNet",
                   tuneGrid = avnnetGrid,
                   trControl = ctrl,
                   preProc = c("center", "scale"),
                   trace = FALSE,
                   MaxNWts = numWts, 
                   maxit = 500)
avnnetTune

############
## 模型比较
############

# 随机梯度助推
trainx = dplyr::select(sim1_da1, -y)
trainy = sim1_da1$y

gbmGrid <- expand.grid(interaction.depth = seq(1, 7, by = 2), 
                       n.trees = seq(100, 1000, by = 50),
                       shrinkage = c(0.01, 0.1),
                       n.minobsinnode = 5)
set.seed(2017)
gbmTune <- train(trainx, trainy,
                 method="gbm",
                 tuneGrid = gbmGrid,
                 verbose = FALSE)
library(pROC)
pregbm <- predict(gbmTune, trainx)
roc(pregbm, trainy)

# 群组lasso逻辑回归
# trainx是自变量矩阵，去除应变量列
trainx = dplyr::select(sim1_da1, -y)
# 将应变量存在trainy中
trainy = sim1_da1$y
# 得到关于群组的指针
index <- gsub("\\..*", "", names(trainx))
# 对100个调优参数值进行调优
nlam <- 100
# 设置调优过程中模型的预测类型
# - `link`：返回链结函数的拟合值
# - `response`：返回拟合的概率值
# number of cross-validation folds
kfold <- 10
cv_fit <- cv_glasso(trainx, trainy, nlam = nlam, kfold = kfold)
cv_fit$lambda.max.auc 
