## 数据预处理
## ---------------
# 先安装这些包才能用library()函数载入
# caret: 提供获取、使用、评估成百上千个机器学习模型及其拟合效果的系统交互界面
# 为机器学习提供了结构化的方法并且对一系列机器学习过程进行评估
library(caret)
# e1071: 各类计量经济和机器学习的延伸；我们通过naiveBayes()函数进行朴素贝叶斯判别
library(e1071)
# gridExtra: 绘图辅助功能，讲不同的图形组合在一起成为图表
library(gridExtra) 
# lattice: 建立在核心绘图能力上的格子框架图形
library(lattice)
# imputeMissings: 填补缺失值
library(imputeMissings)
# RANN: 应用k邻近算法
library(RANN)
# corrplot: 相关矩阵的高级可视化
library(corrplot)
# nnet: 拟合单个潜层级的神经网络模型
library(nnet)
# car: 回归模型解释和可视化工具，其它附加功能； 其中包括some()和scatterplotMatrix()函数
library(car)
# gpairs: 广义散点图；对混合类别和连续变量产生散点图矩阵
library(gpairs)
# reshape2: 灵活重构和整合数据，主要有两个函数melt()和dcast()
library(reshape2)
# psych: 心理计量学方法和抽样调查分析，尤其是因子分析和项目反应模型；
# 我们会使用包中的describe()函数
library(psych)
# plyr: 可以将数据分割成更小的数据，然后对分割后的数据进行些操作，最后把操作的结果汇总
library(plyr)
# tidyr: 清理揉合数据的包，主要函数是spread()和gather()
library(tidyr)

sim.dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
summary(sim.dat)
## ------------------------------
## 数据清理
# 将错误的年龄观测设置为缺失值
sim.dat$age[which(sim.dat$age>100)]<-NA
# 将错误的实体店购买观测设置为缺失值
sim.dat$store_exp[which(sim.dat$store_exp<0)]<-NA
# 通过summary()函数检查清理情况
summary(subset(sim.dat,select=c("age","income")))
## ------------------------------
## 缺失值填补
### 中位数或众数填补
# 将填补后的数据存在另外一个数据框中
demo_imp<-impute(sim.dat,method="median/mode")
# 只检查前5列，因为后面没有缺失值
summary(demo_imp[,1:5])

imp<-preProcess(sim.dat,method="medianImpute")
demo_imp2<-predict(imp,sim.dat)
summary(demo_imp2[,1:5])

### K-近邻填补

imp<-preProcess(sim.dat,method="knnImpute",k=5)
# 用predict()函数进行KNN填补
# 会有错误提示
demo_imp<-predict(imp,sim.dat)
# 找到因子变量
imp<-preProcess(sim.dat,method="knnImpute",k=5)
idx<-which(lapply(sim.dat,class)=="factor")
demo_imp<-predict(imp,sim.dat[,-idx])
summary(demo_imp[,1:3])
# 这里只显示前3个元素
lapply(sim.dat,class)[1:3]

##---------
temp<-rbind(sim.dat,rep(NA,ncol(sim.dat)))
imp<-preProcess(sim.dat,method="knnImpute",k=5)
idx<-which(lapply(temp,class)=="factor")
demo_imp<-predict(imp,temp[,-idx])
##-----------
idx<-apply(temp,1,function(x) sum(is.na(x)) )
as.vector(which(idx==ncol(temp)))

### 袋状树填补
##-------------------
imp<-preProcess(sim.dat,method="bagImpute")
demo_imp<-predict(imp,sim.dat)
summary(demo_imp[,1:5])

### 中心化和标量化
##-------------------
income<-sim.dat$income
# 变量income的均值，na.rm=T告诉R忽略缺失值
mux<-mean(income,na.rm=T)
# 变量income的标准差，na.rm=T告诉R忽略缺失值
sdx<-sd(income,na.rm=T)
# 中心化
tr1<-income-mux
# 标量化
tr2<-tr1/sdx
summary(data.frame(cbind(income,tr1,tr2)))
##---------------
sdat<-subset(sim.dat,select=c("age","income"))
# method选项用于设置变换的方式，你可以同时进行一系列变换。
# center：中心化
# scale：标量化
trans<-preProcess(sdat,method=c("center","scale"))
# preProcess函数的给出的还不是变换后的结果
# 你需要通过predict函数对你想要变换的数据应用preProcess的结果才能够得到变换后的数据框
transformed<-predict(trans,sdat)
summary(transformed)
##------分位数变换
qscale<-function(dat){
  for (i in 1:ncol(dat)){
    up<-quantile(dat[,i],0.99)
    low<-quantile(dat[,i],0.01)
    diff<-up-low
    dat[,i]<-(dat[,i]-low)/diff
  }
  return(dat)
}
##--------------
demo_imp3<-qscale(subset(demo_imp2,select=c("income","store_exp","online_exp")))
summary(demo_imp3)
## 有偏分布
##------------------
# 需要使用e1071包中的偏度计算函数skewness()
set.seed(1000)
par(mfrow=c(1,2),oma=c(2,2,2,2))
# 抽取1000个自由度为2的开方分布，右偏分布
x1<-rchisq(1000,2, ncp = 0)
# 通过x1得到对应的左偏分布变量x2
x2<-max(x1)-x1
plot(density(x2),family ="Songti SC",main=paste("左偏，偏度＝",round(skewness(x2),2)), xlab="X2")
plot(density(x1),family ="Songti SC",main=paste("右偏，偏度＝",round(skewness(x1),2)), xlab="X1")

describe(sim.dat)

dat_bc<-subset(sim.dat,select=c("store_trans","online_trans"))
(trans<-preProcess(dat_bc,method=c("BoxCox")))

transformed<-predict(trans,dat_bc)
par(mfrow=c(1,2),oma=c(2,2,2,2))
hist(dat_bc$store_trans,main="原始商店消费次数",xlab="store_trans",family ="Songti SC")
hist(transformed$store_trans,main="变换后商店消费次数",xlab="store_trans",family ="Songti SC")

(trans<-BoxCoxTrans(dat_bc$store_trans))

transformed<-predict(trans,dat_bc$store_trans)
skewness(transformed)

## 处理离群点
## ----------------

# 选取数值型非问卷调查变量
sdat<-subset(sim.dat,select=c("age","income","store_exp","online_exp","store_trans","online_trans" ))
# 用car包中的函数scatterplotMatrix()绘制散点图矩阵
par(oma=c(2,2,1,2))
scatterplotMatrix(sdat,diagonal="boxplot",smoother=FALSE)

# 计算商店消费量的绝对离差中位数，这里用na.omit()告诉R忽略缺失值
ymad<-mad(na.omit(sdat$income))
# 计算Z分值
zs<-(sdat$income-mean(na.omit(sdat$income)))/ymad
# 看看有多少个离群点
sum(na.omit(zs>3.5))

# 用KNN填补缺失值
sdat<-sim.dat[,c("income","age")]
imp<-preProcess(sdat,method=c("knnImpute"),k=5)
sdat<-predict(imp,sdat)
transformed <- spatialSign(sdat)
transformed <- as.data.frame(transformed)
par(mfrow=c(1,2),oma=c(2,2,2,2))
plot(income ~ age,data = sdat,col="blue",main="变换前",family ="Songti SC")
plot(income ~ age,data = transformed,col="blue",main="变换后",family ="Songti SC")

## 共线性
## --------------

# 选取数值型非问卷调查变量
sdat<-subset(sim.dat,select=c("age","income","store_exp","online_exp","store_trans","online_trans" ))
# 用装袋树填补，换着用，帮大家练练手：）
imp<-preProcess(sdat,method="bagImpute")
sdat<-predict(imp,sdat)
# 得到相关矩阵
correlation<-cor(sdat)
# 对相关矩阵作图
par(oma=c(2,2,2,2))
corrplot.mixed(correlation,order="hclust",tl.pos="lt",upper="ellipse")

cfit1<-lm(income~age+online_trans+store_exp+store_trans,data=sdat)
summary(cfit1)

cfit2<-lm(income~online_trans+store_exp+store_trans,data=sdat)
summary(cfit2)

(highCorr<-findCorrelation(cor(sdat),cutoff=.75))

# 将高相关的变量删除
sdat<-sdat[-highCorr]
# 查看新的相关矩阵
cor(sdat)

## 稀疏变量
## --------------

# 先备份数据
zero_demo<-sim.dat
# 加上两个稀疏变量
# zero1 的取值全是1
# zero2 除了第一个元素是1以外其余全是0
zero_demo$zero1<-rep(1,nrow(zero_demo))
zero_demo$zero2<-c(1,rep(0,nrow(zero_demo)-1))

nearZeroVar(zero_demo,freqCut = 95/5, uniqueCut = 10)

## 编码名义变量
## -----------------

dumVar<-class.ind(sim.dat$gender)
head(dumVar)

dumMod<-dummyVars(~gender+house+income,
                  data=sim.dat,
                  # 用原变量名加上因子层级的名称作为新的名义变量名
                  levelsOnly=F)
head(predict(dumMod,sim.dat))

dumMod<-dummyVars(~gender+house+income+income:gender,
                  data=sim.dat,
                  levelsOnly=F)
head(predict(dumMod,sim.dat))
