## 数据划分和再抽样
### 划分训练集和测试集
#########################
#### 按照结果变量划分数据
#########################
# 载入数据
sim.dat<-read.csv("/Users/happyrabbit/Documents/GitHub/DataScientistR/Data/SegData.csv")
# 需要caret包
library(caret)
# 设置随机种子这样能得到相同的抽样结果
set.seed(3456)
trainIndex <- createDataPartition(sim.dat$segment, p = .8, list = FALSE, times = 1)
head(trainIndex)

# 得到训练集
datTrain <- sim.dat[ trainIndex,]
# 得到测试集
datTest <- sim.dat[-trainIndex,]

library(plyr)
ddply(datTrain,"segment",summarise,count=length(segment),
      percentage=round( length(segment)/nrow(datTrain),2))

ddply(datTest,"segment",summarise,count=length(segment),
      percentage=round(length(segment)/nrow(datTest),2))

####################
#### 按照自变量划分
####################
# 最大差异度抽样用到proxy包
library(proxy)
# 用lattice包绘制散点图
library(lattice)
# 选取年龄和收入这两个变量
testing<-subset(sim.dat,select=c("age","income" ))

set.seed(5)
# 随机选取5个样本
startSet <- sample(1:dim(testing)[1], 5)
start <- testing[startSet,]
# 剩下的样本存在对象samplePool中
samplePool <- testing[-startSet,]

# 通过最大化差异得到的样本存在数据框new内
# obj = minDiss 表示总体差异度以最小差异度为准
newSamp <- maxDissim(start, samplePool,obj = minDiss, n = 5)
new<-samplePool[newSamp,]

newSet <- sample(1:dim(samplePool)[1], 5)
new2<-testing[newSet,]

fig.align='center',family ="Songti SC"}
start$group<-rep("start",nrow(start))
new$group<-rep("new",nrow(new))
new2$group<-rep("new2",nrow(new2))
xyplot(age~income,data=rbind(start,new,new2),grid = TRUE,
       group = group, auto.key = TRUE
)

####################
#### 按时间序列划分
####################
# 抽取符合AR(1)的时间序列向量
timedata = arima.sim(list(order=c(1,0,0), ar=-.9), n=100)
# 对时间序列作图
plot(timedata, main=(expression(AR(1)~~~phi==-.9)))  

timeSlices <- createTimeSlices(1:length(timedata), 
                               initialWindow = 36, horizon = 12, fixedWindow = T)

# 将训练集索引信息存在trainSlices对象内
trainSlices <- timeSlices[[1]]
# 将测试集索引信息存在testSlices对象内
testSlices <- timeSlices[[2]]
# 分别查看第一个训练集样本和测试集样本
trainSlices[[1]]
testSlices[[1]]

head(trainSlices)
head(testSlices)

#####################
### 重抽样
#####################
#### k折交叉验证
#####################
library(caret)
class<-sim.dat$segment
#k折校验重抽样
set.seed(1)
cv<-createFolds(class,k=10,returnTrain=T)
str(cv)
str(timeSlices,max.level = 1)

#这里只是截取了之前的代码用于展示，并不能独立运行
fit0<-train(trainX,trainY,method="svmRadial",
            tuneLength=15,
            trControl=trainControl(method="cv"))

trainIndex <- createDataPartition(sim.dat$segment, p = .8, list = FALSE, times = 5)
dplyr::glimpse(trainIndex)


