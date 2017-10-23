## 数据集:生猪疫情风险预测数据
## ---------------

# sim1_da1.csv  模拟的第一个数据集
# similar sim1_da2 and sim1_da3
# sim1.csv  simulated data, the first simulation
# dummy.sim1.csv dummy variables for the first simulated data with all the baseline in
#code for simulation

# setwd(dirname(file.choose()))
# library(grplasso)

nf<-800
for (j in 1:20){
  set.seed(19870+j)
  x<-c('A','B','C')
  sim.da1<-NULL
  for (i in 1:nf){
    # sample(x, 120, replace=TRUE)->sam
    sim.da1<-rbind(sim.da1,sample(x, 120, replace=TRUE))
  }
  
  data.frame(sim.da1)->sim.da1
  paste("Q", 1:120, sep = "")->col
  paste("Farm", 1:nf, sep = "")->row
  colnames(sim.da1)<-col
  rownames(sim.da1)<-row
  
  # 用nnet包中的class.ind()函数将问题回复编码为名义变量
  library(nnet)
  dummy.sim1<-NULL
  for (k in 1:ncol(sim.da1)) {
    tmp=class.ind(sim.da1[,k])
    colnames(tmp)=paste(col[k],colnames(tmp))
    dummy.sim1=cbind(dummy.sim1,tmp)
  }
  data.frame(dummy.sim1)->dummy.sim1
  
  # 每个问题对应的3个名义变量中有重复信息
  # 将C选项设置为基线回复
  # 删除基线名义变量
  
  base.idx<-3*c(1:120)
  dummy1<-dummy.sim1[,-base.idx]
  
  # 对每个r设置依次抽取相应的因变量
  # 每次只对一个r值抽取，将其余代码注释掉
  # 得到r=0.1 时每个农场对应的连接函数值
  c(rep(c(1/10,0,-1/10),40),rep(c(1/10,0,0),40),rep(c(0,0,0),40))->s1
  as.matrix(dummy.sim1)%*%s1-40/3/10->link1
  
  # r=0.25 
  # c(rep(c(1/4,0,-1/4),40),rep(c(1/4,0,0),40),rep(c(0,0,0),40))->s1
  # as.matrix(dummy.sim1)%*%s1-40/3/4->link1
  
  # r=0.5 
  # c(rep(c(1/2,0,-1/2),40),rep(c(1/2,0,0),40),rep(c(0,0,0),40))->s1
  # as.matrix(dummy.sim1)%*%s1-40/3/2->link1
  
  # r=1
  # c(rep(c(1,0,-1),40),rep(c(1,0,0),40),rep(c(0,0,0),40))->s1
  # as.matrix(dummy.sim1)%*%s1-40/3->link1
  
  # r=2
  # c(rep(c(2,0,-2),40),rep(c(2,0,0),40),rep(c(0,0,0),40))->s1
  # as.matrix(dummy.sim1)%*%s1-40/3/0.5->link1
  
  
  # 在连接函数的基础上计算每个农场对应的爆发概率
  exp(link1)/(exp(link1)+1)->hp1
  
  # 基于爆发概率hp1，抽取相应的因变量res
  res<-rep(9,nf)
  for (i in 1:nf){
    sample( c(1,0),1,prob=c(hp1[i],1-hp1[i]))->res[i]
  }
  
  # 这里将数据存成3个不同的版本，只是为了之后不同模型使用方便
  # 3个数据集都含有所有120个问题的回复，但彼此稍微有不同
  
  # da1 含有因变量，但没有名义变量所属问题的信息
  # da2 没有因变量，但最后一行包括的名义变量所属的问题
  # da3 没有因变量，没有名义变量所属问题的信息
  
  dummy1$y<-res
  da1<-dummy1
  y<-da1$y
  ind<-NULL
  for (i in 1:120){
    c(ind,rep(i,2))->ind
  }
  
  da2<-rbind(da1[,1:240],ind)
  da3<-da1[,1:240]
  
  # 将数据集储存起来
  write.csv(da1,paste('sim',j,'_da',1,'.csv',sep=''),row.names=F)
  write.csv(da2,paste('sim',j,'_da',2,'.csv',sep=''),row.names=F)
  write.csv(da3,paste('sim',j,'_da',3,'.csv',sep=''),row.names=F)
  write.csv(sim.da1,paste('sim',j,'.csv',sep=''),row.names=F)
  write.csv(dummy.sim1,paste('dummy.sim',j,'.csv',sep=''),row.names=F)
}
