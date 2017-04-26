## 数据集:航空公司满意度调查
## ---------------
# 先建立因子载荷矩阵
# 其中前12项符合双因子结构，因为每项对应一个总体因子载荷和某特定因子的载荷
# 比如购票容易度对应总体因子载荷0.33，对因特定购票因子载荷0.58
# 我们可以将结果评分看成是总体因子和特定因子共同作用的结果

loadings <- matrix(c (
  # 购票体验
  .33, .58, .00, .00,  # 购票容易度 
  .35, .55, .00, .00,  # 座椅选择
  .30, .52, .00, .00,  # 航班选择
  .40, .50, .00, .00,  # 票价
  # 机舱设施
  .50, .00, .55, .00,  # 座椅舒适度
  .41, .00, .51, .00,  # 位置前后空间
  .45, .00, .57, .00,  # 随机行李存放
  .32, .00, .54, .00,  # 机舱清洁
  # 空航服务
  .35, .00, .00, .50,  # 礼貌
  .38, .00, .00, .57,  # 友善
  .60, .00, .00, .50,  # 能够提供需要的帮助
  .52, .00, .00, .58,  # 食物饮料服务
  # 总体指数  
  .43, .10, .30, .30,  # 总体满意度
  .35, .50, .40, .20,  # 再次选择次航空公司
  .25, .50, .50, .20), # 向朋友推荐此航空公司
  nrow=15,ncol=4, byrow=TRUE)

# 将载荷矩阵乘以它的转秩，然后将对角线元素设置为1得到相关矩阵
cor_matrix<-loadings %*% t(loadings)
# Diagonal set to ones.
diag(cor_matrix)<-1

# 我们通过mvtnorm包模拟有特定相关矩阵的数据集
library(mvtnorm)
# 设置3个航空公司对应的评分均值向量
mu1=c(5,6,5,6, 7,8,6,7, 5,5,5,5, 6,6,6)
mu2=c(3,3,2,3, 5,4,5,6, 8,8,8,8, 3,3,3)
mu3=c(2,2,2,2, 8,8,8,8, 8,8,8,8, 8,8,8)

#设置随机种子
set.seed(123456) 
# 受访者ID
resp.id <- 1:1000 

library(MASS) 
rating1 <- mvrnorm(length(resp.id),
                   mu=mu1,
                   Sigma=cor_matrix)
rating2 <- mvrnorm(length(resp.id),
                   mu=mu2,
                   Sigma=cor_matrix)
rating3 <- mvrnorm(length(resp.id),
                   mu=mu3,
                   Sigma=cor_matrix)


# 将分值限定在1到9之间
rating1[rating1>9]<-9
rating1[rating1<1]<-1
rating2[rating2>9]<-9
rating2[rating2<1]<-1
rating3[rating3>9]<-9
rating3[rating3<1]<-1

# 将分值转化为整数
rating1<-data.frame(round(rating1,0))
rating2<-data.frame(round(rating2,0))
rating3<-data.frame(round(rating3,0))
rating1$ID<-resp.id
rating2$ID<-resp.id
rating3$ID<-resp.id
rating1$Airline<-rep("AirlineCo.1",length(resp.id))
rating2$Airline<-rep("AirlineCo.2",length(resp.id))
rating3$Airline<-rep("AirlineCo.3",length(resp.id))
rating<-rbind(rating1,rating2,rating3)

# 为数据集的各列命名
names(rating)<-c(
  "Easy_Reservation",
  "Preferred_Seats",
  "Flight_Options",
  "Ticket_Prices",
  "Seat_Comfort",
  "Seat_Roominess",
  "Overhead_Storage",
  "Clean_Aircraft",
  "Courtesy",
  "Friendliness",
  "Helpfulness",
  "Service",
  "Satisfaction",
  "Fly_Again",
  "Recommend",
  "ID",
  "Airline")
# 让我们检查一下抽取的数据集：
str(rating,vec.len=3)
