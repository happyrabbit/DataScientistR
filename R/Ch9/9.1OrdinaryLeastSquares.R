###############
# 普通线性回归
###############

#######################
## 最小二乘线性模型
#######################

dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# 在我们开始之前，还需要对数据进行一些清理，删除错误的样本观测，消费金额不能为负数
dat<-subset(dat,store_exp>0 & online_exp>0)
modeldat<-dat[,grep("Q",names(dat))]
# 得到总消费量=实体店消费+在线消费
modeldat$total_exp<-dat$store_exp+dat$online_exp

summary(modeldat)
par(mfrow=c(1,2))
hist(modeldat$total_exp,main="",xlab="total_exp")
boxplot(modeldat$total_exp)

y<-modeldat$total_exp
# 求Z分值
zs<-(y-mean(y))/mad(y)
# 找到Z分值大于3.5的离群点，删除这些观测
modeldat<-modeldat[-which(zs>3.5),]

library(corrplot)
correlation<-cor(modeldat[,grep("Q",names(modeldat))])
corrplot.mixed(correlation,order="hclust",tl.pos="lt",upper="ellipse")

library(caret)
highcor<-findCorrelation(correlation,cutoff=.75)
modeldat<-modeldat[,-highcor]

lmfit<-lm(log(total_exp)~.,data=modeldat)
summary(lmfit)

confint(lmfit,level=0.9)

#######################
## 回归诊断
#######################

par(mfrow=c(2,2))
plot(lmfit,which=1)
plot(lmfit,which=2)
plot(lmfit,which=3)
plot(lmfit,which=4)
library(car)
outlierTest(lmfit) #Bonferroni离群点检验
outlierTest(lmfit)

# 这里数据modeldat的行名是原数据集的行号，所以是字符类型
# 找到相应的观测
idex<-which(row.names(modeldat)=="960")
# 删除离群观测
modeldat=modeldat[-idex,]

lmfit<-lm(log(total_exp)~.,data=modeldat)
outlierTest(lmfit)
