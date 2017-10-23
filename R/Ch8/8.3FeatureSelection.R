############
# 特征选择
############

##################
## 过滤法（filter）
##################

dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# 对数据进行一些清理，删除错误的样本观测，消费金额不能为负数
dat<-subset(dat,store_exp>0 & online_exp>0 & age<100)
# 计算Pearson相关性
cor(dat$age,dat$store_exp)

cor(dat$age,dat$online_exp,method="spearman")

cor(dat$age,dat$online_exp,method="kendall")

# 建立loess模型
smoother <- loess(dat$online_exp ~ dat$age)
# 计算loess估值
pred.smoother<-predict(smoother, dat$age)
# 计算loess对应的RSS
rss<-sum((dat$online_exp-pred.smoother)^2)
# 计算因变量的TSS
tss<-sum((dat$online_exp-mean(dat$online_exp))^2)
# 通过RSS和TSS计算R-squared
(r2<-1-rss/tss)

library(lattice)
xyplot(dat$online_exp ~ dat$age,
type = c("p", "smooth"),
xlab = "age",
ylab = "online_exp")

library(ggplot2)
ggplot(dat,aes(age,online_exp))+geom_smooth()

TrainXtrans<-dat[,grep("Q",names(dat))]
TrainY<-dat$store_exp+dat$online_exp

# 选项设置`nonpara = TRUE` (对于非参数回归)
loessResults <- filterVarImp(x = TrainXtrans, y = TrainY, nonpara = TRUE)
head(loessResults)

t.test(online_exp~gender,data=dat)

anova(lm(online_exp~segment,data=dat))

vars<-c("age","income","store_exp","online_exp")
x<-subset(dat,select=vars)
y<-dat$house
rocValues <- filterVarImp(x = x, y = y)
rocValues

y<-dat$segment
rocValues <- filterVarImp(x = x, y = y)
rocValues

##################
##### Relief 算法
##################

# 需要事先安装CORElearn包
library(CORElearn)
infoCore(what="attrEvalReg")

infoCore(what="attrEval")

dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# 删除应变量缺失的行
dat=filter(dat,!is.na(income))
# 选取需要的变量
# 特征矩阵
xtrain=dat%>%
  dplyr::select(num_range("Q", 1:10))
# 应变量
ytrain=dat$income
# 实践Relief算法
reliefValues = attrEval(ytrain ~ ., data = xtrain,
## 有许多计算Relief的方法，这里将k个邻近样本设置为等权重
## 更多信息键入?attrEval
estimator = "RReliefFequalK",
## 测试的数目：
ReliefIterations = 50)

head(reliefValues)

library(AppliedPredictiveModeling)
perm = permuteRelief(x = xtrain, y = ytrain,
nperm = 500,
estimator = "RReliefFequalK",
ReliefIterations = 50)

head(perm$permutations)

lattice::histogram(~ value|Predictor,data = perm$permutations)

head(perm$standardized)

##############
## 绕封法
##############

#############
##### 穷举法
#############

# 需要先安装包
library(leaps)
# 载入服装消费者数据
sim.dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
ytrain<-sim.dat$income
xtrain<-dplyr::select(sim.dat,store_exp,online_exp,store_trans,online_trans,
               Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10)
# 先用所有变量拟合全模型
regfit.full=regsubsets(ytrain~.,xtrain)

summary(regfit.full)

regfit.full=regsubsets(ytrain~.,xtrain,nvmax=10)
reg.summary=summary(regfit.full)

names(reg.summary)

reg.summary$adjr2

par(mfrow=c(2,2))
plot(reg.summary$adjr2,xlab="变量个数",family ="Songti SC",ylab="调整R2",type="l")
# 用which.max()函数找到调整R2最大的点
idx<-which.max(reg.summary$adjr2)
# 用points()函数将调整R2最大的点在图上标出
points(idx,reg.summary$adjr2[idx],col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="变量个数",family ="Songti SC",ylab="Cp",type="l")
# 类似的找到Cp最小的点
idx<-which.min(reg.summary$cp)
# 将Cp最小的点在图上标出
points(idx,reg.summary$cp[idx],col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="变量个数",family ="Songti SC",ylab="BIC",type="l")
# 找到BIC最小的点
idx<-which.min(reg.summary$bic)
# 将Cp最小的点在图上标出
points(idx,reg.summary$bic[idx],col="red",cex=2,pch=20)
plot(reg.summary$rss,xlab="变量个数",family ="Songti SC",ylab="RSS",type="l")
# 找到RSS最小的点
idx<-which.min(reg.summary$rss)
# 将Cp最小的点在图上标出
points(idx,reg.summary$rss[idx],col="red",cex=2,pch=20)

# 需要先安装包
library(leaps)
# 载入服装消费者数据
sim.dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
ytrain<-sim.dat$income
xtrain<-dplyr::select(sim.dat,store_exp,online_exp,store_trans,online_trans,
               Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10)
# 先用所有变量拟合全模型
regfit.fwd=regsubsets(ytrain~.,xtrain,nvmax=10,method="forward")
# 可以用summary()函数得到模型选择
reg.summary=summary(regfit.fwd)

par(mfrow=c(2,2))
plot(reg.summary$adjr2,xlab="变量个数",family ="Songti SC",ylab="调整R2",type="l")
# 用which.max()函数找到调整R2最大的点
idx<-which.max(reg.summary$adjr2)
# 用points()函数将调整R2最大的点在图上标出
points(idx,reg.summary$adjr2[idx],col="red",cex=2,pch=20)
plot(reg.summary$cp,xlab="变量个数",family ="Songti SC",ylab="Cp",type="l")
# 类似的找到Cp最小的点
idx<-which.min(reg.summary$cp)
# 将Cp最小的点在图上标出
points(idx,reg.summary$cp[idx],col="red",cex=2,pch=20)
plot(reg.summary$bic,xlab="变量个数",family ="Songti SC",ylab="BIC",type="l")
# 找到BIC最小的点
idx<-which.min(reg.summary$bic)
# 将Cp最小的点在图上标出
points(idx,reg.summary$bic[idx],col="red",cex=2,pch=20)
plot(reg.summary$rss,xlab="变量个数",family ="Songti SC",ylab="RSS",type="l")
# 找到RSS最小的点
idx<-which.min(reg.summary$rss)
# 将Cp最小的点在图上标出
points(idx,reg.summary$rss[idx],col="red",cex=2,pch=20)

######################
##### 向后选择法
######################

regfit.bwd=regsubsets(ytrain~.,xtrain,nvmax=10,method="backward")
# 可以用summary()函数得到模型选择
reg.summary=summary(regfit.bwd)

library(MASS)
fit=lm(ytrain~.,data=xtrain)
stepAIC(fit)
