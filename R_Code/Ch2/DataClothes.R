## 数据集:服装消费者数据
## ---------------
# 设置随机种子，使数据模拟过程可重复
set.seed(12345)
# 定义观测数目
ncust<-1000
# 建立数据框存放模拟观测，初始数据框中只有一列id，即消费者编号
seg_dat<-data.frame(id=as.factor(c(1:ncust)))
# 指定要生成的变量，并为变量命名
vars<-c("age","gender","income","house","store_exp","online_exp","store_trans","online_trans")
# 每个变量对应的数据类型
# norm： 正态分布
# binom: 二项分布
# pois： 泊松分布
vartype<-c("norm","binom","norm","binom","norm","norm","pois","pois")
# 四个消费者分组的名称
group_name<-c("Price","Conspicuous","Quality","Style")
# 各消费者群组的大小
group_size<-c(250,200,200,350)
# group_name和group_size的第一个元素表明，对于“Price”这组消费者，我们将模拟N=250个观测。
## ---------------

# 定义均值矩阵
mus <- matrix( c(
  # 价格敏感（Price）类对应均值
  60, 0.5, 120000,0.9, 500,200,5,2,
  # 炫耀性消费（Conspicuous）类对应均值
  40, 0.7, 200000,0.9, 5000,5000,10,10,
  # 质量（Quality）类对应均值
  36, 0.5, 70000, 0.4, 300, 2000,2,15,
  # 风格（Style）类对应均值
  25, 0.2, 90000, 0.2, 200, 2000,2,20), ncol=length(vars), byrow=TRUE)
## ---------------

# 每类的标准差 (NA = 标准差无定义)
sds<- matrix( c(
  # 价格敏感（Price）类对应均值
  3,NA,8000,NA,100,50,NA,NA,
  # 炫耀性消费（Conspicuous）类对应均值
  5,NA,50000,NA,1000,1500,NA,NA,
  # 质量（Quality）类对应均值
  7,NA,10000,NA,50,200,NA,NA,
  # 风格（Style）类对应均值
  2,NA,5000,NA,10,500,NA,NA), ncol=length(vars), byrow=TRUE)

## ---------------
# 抽取非抽样调查数据
sim.dat<-NULL
set.seed(2016)
# 对消费者类别进行循环（i）
for (i in seq_along(group_name)){
  # 为了核实代码，展示循环运行过程，我们在循环中添加了这样一行代码
  # 函数运行时会打印出正在抽取的样本类名
  cat (i, group_name[i],"\n")
  # 创建一个空矩阵用于存放该类消费者相关数据
  seg<-data.frame(matrix(NA,nrow=group_size[i], ncol=length(vars)))  
  # 在这个类之内，对不同变量迭代，抽取相应的随机数据
  for (j in seq_along(vars)){
    # 在每个变量上迭代
    if (vartype[j]=="norm"){
      # 抽取正态分布变量
      seg[,j]<-rnorm(group_size[i], mean=mus[i,j], sd=sds[i,j])
    } else if (vartype[j]=="pois") {
      # 抽取泊松分布变量
      seg[,j]<-rpois(group_size[i], lambda=mus[i,j])
    } else if (vartype[j]=="binom"){
      # 抽取二项分布变量
      seg[,j]<-rbinom(group_size[i],size=1,prob=mus[i,j])
    } else{
      # 如果变量类型不是上述几种，程序停止运行并提示信息
      stop ("Don't have type:",vartype[j])
    }        
  }
  # 将该消费者类的数据依行添加到总数据集
  sim.dat<-rbind(sim.dat,seg)
}

## -------------
# 指定数据框的列名为我们定义的变量名
names(sim.dat)<-vars
# 加上一个因子列表明每个观测的对应的消费者类别
sim.dat$segment<-factor(rep(group_name,times=group_size))
# 将二项变量转化为贴有标签的因子变量
# Female: 女性
# Male: 男性
sim.dat$gender<-factor(sim.dat$gender, labels=c("Female","Male"))
sim.dat$house<-factor(sim.dat$house, labels=c("No","Yes"))
# 假设在线购买和在实体店购买的次数至少为1，所以这里在原随机值上加1
sim.dat$store_trans<-sim.dat$store_trans+1
sim.dat$online_trans<-sim.dat$online_trans+1
# 年龄为整数
sim.dat$age<-floor(sim.dat$age)

## ------------

# 加入缺失值
idxm <- as.logical(rbinom(ncust, size=1, prob=sim.dat$age/200))
sim.dat$income[idxm]<-NA

## ------------

# 错误输入，离群点
set.seed(123)
idx<-sample(1:ncust,5)
sim.dat$age[idx[1]]<-300
sim.dat$store_exp[idx[2]]<- -500
sim.dat$store_exp[idx[3:5]]<-c(50000,30000,30000)

## ------------

# 抽取问卷调查回复
# 问卷问题数目
nq<-10
# 各类消费者对问卷回复的正态分布均值矩阵
mus2 <- matrix( c(
  # 价格敏感（Price）类对应均值
  5,2,1,3,1,4,1,4,2,4,
  # 炫耀性消费（Conspicuous）类对应均值
  1,4,5,4,4,4,4,1,4,2,
  # 质量（Quality）类对应均值
  5,2,3,4,3,2,4,2,3,3,
  # 风格（Style）类对应均值
  3,1,1,2,4,1,5,3,4,2), ncol=nq, byrow=TRUE)

# 方差假设都是0.2
sd2<-0.2
sim.dat2<-NULL
set.seed(1000)
# 对消费者类别进行循环（i）
for (i in seq_along(group_name)){
  # 为了核实代码，展示循环运行过程，我们在循环中添加了这样一行代码
  # 函数运行时会打印出正在抽取的样本类名，这里不再显示输出
  # cat (i, group_name[i],"\n")
  # 创建一个空矩阵用于存放该类消费者相关数据
  seg<-data.frame(matrix(NA,nrow=group_size[i], ncol=nq))  
  # 在这个类之内，对不同变量迭代，抽取相应的随机数据
  for (j in 1:nq){
    # 抽取正态分布变量
    res<-rnorm(group_size[i], mean=mus2[i,j], sd=sd2)
    # 设置上下限度
    res[res>5]<-5
    res[res<1]<-1
    # 通过 floor()函数将连续值转化成离散整数。
    seg[,j]<-floor(res)
  }
  # 将该消费者类的数据添加到总数据集
  sim.dat2<-rbind(sim.dat2,seg)
}

# 为数据框添加列标签
names(sim.dat2)<-paste("Q",1:10,sep="")
# 合并两部分数据
sim.dat<-cbind(sim.dat,sim.dat2)
# 加上一个因子列表明每个观测的对应的消费者类别
sim.dat$segment<-factor(rep(group_name,times=group_size))

# 检查一下抽取的数据集：
str(sim.dat,vec.len=3)