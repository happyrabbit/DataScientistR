########################
## 7.1 回归模型评估度量
########################

# 载入服装消费者数据
sim.dat<-read.csv("/Users/happyrabbit/Documents/GitHub/DataScientistR/Data/SegData.csv")
fit<-lm(income~store_exp+online_exp+store_trans+online_trans,data=sim.dat)
summary(fit)

########################
## 7.2 分类模型评估度量
########################

library(dplyr)
library(randomForest)
library(caret)
library(readr)
# 读取数据
# 载入数据
disease_dat<-read.csv("/Users/happyrabbit/Documents/GitHub/DataScientistR/Data/sim1_da1.csv")
# 可以用glimpse()函数查看数据
# glimpse(disease_dat)

# 划分训练集和测试集
set.seed(2016)
trainIndex<-createDataPartition(disease_dat$y,p=0.8,list=F,times=1)
xTrain<-disease_dat[trainIndex,]%>%
  dplyr::select(-y)
xTest<-disease_dat[-trainIndex,]%>%
  dplyr::select(-y)
# 需要将应变量转化成因子类型
yTrain<-disease_dat$y[trainIndex]%>%as.factor()
yTest<-disease_dat$y[-trainIndex]%>%as.factor()
# 训练随机森林模型
train_rf<-randomForest(yTrain~.,data=xTrain,mtry=trunc(sqrt(ncol(xTrain)-1)),ntree=1000,importance=T)

yhatprob<-predict(train_rf,xTest,"prob")
set.seed(100)
car::some(yhatprob)

yhat<-predict(train_rf,xTest)
car::some(yhat)

### Kappa统计量
table(yhat,yTest)

kt<-fmsb::Kappa.test(table(yhat,yTest))
# 统计量估值在函数返回值的Result对象中
kt$Result$estimate
kt$Judgement

### ROC曲线
library(pROC)
rocCurve<-roc(response=yTest,predictor=yhatprob[,2])
plot(rocCurve,legacy.axes=T)

# 得到AUC的估计
auc(rocCurve)
# DeLong方法得到的AUC置信区间
ci.auc(rocCurve)

### 提升图
modelscore<-yhatprob[,2]
# 随机抽取一些分值
randomscore<-rnorm(length(yTest))
labs<-c(modelscore="Random Forest",
        randomscore="Random Number")

liftCurve<-lift(yTest~modelscore+randomscore,class="1",labels=labs)

xyplot(liftCurve,auto.key=list(columns=2,lines=T,points=F))