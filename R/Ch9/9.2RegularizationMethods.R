#############
### 岭回归
#############

dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# 对数据进行一些清理，删除错误的样本观测，消费金额不能为负数
dat<-subset(dat,store_exp>0 & online_exp>0)
# 将10个问卷调查变量当作自变量
trainx<-dat[,grep("Q",names(dat))]
# 将实体店消费量和在线消费之和当作应变量
# 得到总消费量=实体店消费+在线消费
trainy<-dat$store_exp+dat$online_exp

ctrl <- trainControl(method = "cv", number = 10)
ridgeGrid <- data.frame(.lambda = seq(0, .1, length = 20))
set.seed(100)

ridgeRegTune <- train(trainx, trainy,
                      method = "ridge",
                      ## 用不同的罚函数值来拟合模型
                      tuneGrid = ridgeGrid,
                      trControl = ctrl,
                      ## 中心化和标度化变量
                      preProc = c("center", "scale"))

ridgefit = enet(x = as.matrix(trainx), y = trainy, lambda = 0.01,
                # 这里设置将自变量标准化
                normalize = TRUE)

ridgePred <- predict(ridgefit, newx = as.matrix(trainx), 
                     s = 1, mode = "fraction", type = "fit")

names(ridgePred)
head(ridgePred$fit)
ridgeCoef<-predict(ridgefit,newx = as.matrix(trainx), 
                   s=1, mode="fraction", type="coefficients")
# 这里不显示结果
RidgeCoef=ridgeCoef$coefficients

################
### Lasso
################

ctrl <- trainControl(method = "cv", number = 10)
lassoGrid <- data.frame(fraction = seq(.8, 1, length = 20))
set.seed(100)
lassoTune <- train(trainx, trainy,
                      method = "lars",
                      ## 用不同的罚函数值来拟合模型
                      tuneGrid = lassoGrid,
                      trControl = ctrl,
                      ## 中心化和标度化变量
                      preProc = c("center", "scale"))
lassoTune

plot(lassoTune)

lassoModel<- enet(x = as.matrix(trainx), y = trainy, lambda = 0, normalize = TRUE)
lassoFit <- predict(lassoModel, newx = as.matrix(trainx),s = .95, mode = "fraction",type = "fit")
head(lassoFit$fit)
lassoCoef<-predict(lassoModel,newx = as.matrix(trainx),s=0.95, mode="fraction", type="coefficients")

LassoCoef=lassoCoef$coefficients

################
### 弹性网络
################

enetGrid <- expand.grid(.lambda = seq(0,0.2,length=20), .fraction = seq(.8, 1, length = 20))
set.seed(100)
enetTune <- train(trainx, trainy,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"))
                  
enetfit = enet(x = as.matrix(trainx), y = trainy, lambda = 0.01,
                # 这里设置将自变量标准化
                normalize = TRUE)

enetPred <- predict(enetfit, newx = as.matrix(trainx), 
                     s = 0.958, mode = "fraction", type = "fit")

enetCoef<-predict(ridgefit,newx = as.matrix(trainx), 
                   s=0.958, mode="fraction", type="coefficients")
