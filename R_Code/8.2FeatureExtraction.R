#######################
## 初步探索特征
#######################

# 可以从网站下载该数据
airline<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/AirlineRating.csv")
glimpse(airline)

library(corrplot)
# 选取其中的问卷调查项
dplyr::select(airline,Easy_Reservation:Recommend)%>%
  # 得到相关矩阵
  cor()%>%
  # 用corrplot()绘制相关图
  # 选项order="hclust"按照变量的相似度，基于系统聚类的结果对行列进行重新排列
  corrplot(,order="hclust")

# 选取其中的问卷调查项和航空公司因子信息
# 即删除ID项
airline.mean<-dplyr::select(airline,-ID)%>%
  # 按Airline对数据进行分组总结
  group_by(Airline)%>%
  # 对每个数值
  summarise_each(funs(mean))%>%
  # 显示数据
  glimpse()
  
# 相关信息见：http://colorbrewer2.org
library(RColorBrewer)
# 将航空公司设置成行名称然后将对应的字符列删除
row.names(airline.mean)<-airline.mean$Airline
airline.mean<-dplyr::select(airline.mean,-Airline)
# 绘制热图
heatmap.2(as.matrix(airline.mean),
          col=brewer.pal(9,"YlGn"),trace="none",key=FALSE,dend="none",cexCol=0.6,cexRow =1)
title(family ="Songti SC",
          main="航空公司问卷调查均值热图")

#######################
## 主成分分析
#######################

airline.pc<-dplyr::select(airline,Easy_Reservation:Recommend)%>%
  prcomp()
summary(airline.pc)

plot(airline.pc,type="l",family ="Songti SC",main="PCA陡坡图")

biplot(airline.pc,family ="Songti SC",main="PCA双标图",cex=c(0.5,1),xlim=c(-0.06,0.04))

airline.mean.pc<-dplyr::select(airline.mean,Easy_Reservation:Recommend)%>%
  prcomp()
biplot(airline.mean.pc,family ="Songti SC",main="聚合后PCA结果双标图",
       cex=0.7, expand=2,xlim=c(-0.8, 1),ylim=c(-0.7,0.8))

airline.mean[3,]-airline.mean[1,]

#######################
## 探索性因子分析
#######################

library(nFactors)
subset(airline,select=Easy_Reservation:Recommend)%>%
  # 转化成数据框格式传递给nScree()函数
  data.frame()%>%
nScree()

# 得到变量相关矩阵的特征值
eigenvalue<-subset(airline,select=Easy_Reservation:Recommend)%>%
  cor()%>%
  eigen()
eigenvalue$values

airline%>%
  subset(select=Easy_Reservation:Recommend)%>%
  factanal(factors=2)

airline%>%
  subset(select=Easy_Reservation:Recommend)%>%
  factanal(factors=3)

library(GPArotation)
airline%>%
  subset(select=Easy_Reservation:Recommend)%>%
  factanal(factors=3,rotation="oblimin")
  
#######################
## 高维标度化
#######################
  
var.dist<-airline%>%
  subset(select=Easy_Reservation:Recommend)%>%
  # 用dist()函数计算评分之间的距离
  dist()
  
var.mds<-cmdscale(var.dist)%>%
  data.frame()
# 重新为列命名
names(var.mds)<-c("Score1","Score2")
head(var.mds)
plot(var.mds)
mean.dist<-airline.mean%>%
    dist()
mean.mds<-cmdscale(mean.dist)%>%
  data.frame()
# 重新为列命名
names(mean.mds)<-c("Score1","Score2")
plot(mean.mds,type="n",xlim=c(-8,6),ylim=c(-5,6))
text(mean.mds,row.names(mean.mds),cex=1)
mean.rank<-lapply(airline.mean,function(x) factor(rank(x),ordered=T))%>%
  data.frame()
glimpse(mean.rank)
library(cluster)
mean.rank.gower<-daisy(mean.rank,metric="gower")

library(MASS)
mean.mds.gower<-isoMDS(mean.rank.gower)

plot(mean.mds.gower$points,type="n",xlim=c(-0.5,0.5),ylim=c(-0.2,0.4))
text(mean.mds.gower$points,row.names(mean.mds),cex=1)







  
  

