# 5.2 数据整合
##  base包：apply()
sim.dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
sdat<-sim.dat[,!lapply(sim.dat,class)=="factor"]

apply(sdat, MARGIN=2,function(x) mean(na.omit(x)))

apply(sdat, MARGIN=2,function(x) sd(na.omit(x)))

ddply(sim.dat,"segment",summarize, Age=round(mean(na.omit(age)),0),
      FemalePct=round(mean(gender=="Female"),2),
      HouseYes=round(mean(house=="Yes"),2),
      store_exp=round(mean(na.omit(store_exp),trim=0.1),0),
      online_exp=round(mean(online_exp),0),
      store_trans=round(mean(store_trans),1),
      online_trans=round(mean(online_trans),1))

ddply(sim.dat,"segment",summarize,avg_online=round(sum(online_exp)/sum(online_trans),2),
      avg_store=round(sum(store_exp)/sum(store_trans),2))

library(caret)
set.seed(2016)
trainIndex<-createDataPartition(sim.dat$segment,p=0.01,list=F,times=1)
examp<-sim.dat[trainIndex,c("age","store_exp","segment")]

ddply(examp,"segment",transform,store_pct=round(store_exp/sum(store_exp),2))

ddply(examp,"segment",subset,store_exp>median(store_exp))

## dplyr包
### 数据框显示
dplyr::tbl_df(sim.dat)
dplyr::glimpse(sim.dat)
### 数据截选（按行／列）
# 提取出满足条件的行：收入大于30万的样本
library(magrittr)
library(dplyr)
dplyr::filter(sim.dat, income >300000) %>%
  dplyr::tbl_df()

### %>%
ave_exp <- filter( 
  summarise(
    group_by( 
      filter(
        sim.dat, 
        !is.na(income)
      ), 
      segment
    ), 
    ave_online_exp = mean(online_exp), 
    n = n()
  ), 
  n > 200
) 
##---------------
avg_exp <- sim.dat %>% 
  filter(!is.na(income)) %>% 
  group_by(segment) %>% 
  summarise( 
    ave_online_exp = mean(online_exp), 
    n = n() ) %>% 
  filter(n > 200)

## 删除重复的行
## 这里没有重复的行
dplyr::distinct(sim.dat)

dplyr::sample_frac(sim.dat, 0.5, replace = TRUE) 
dplyr::sample_n(sim.dat, 10, replace = TRUE) 

# 选取sim.dat的10到15行
dplyr::slice(sim.dat, 10:15) 

# 选取收入最高的两个观测
dplyr::top_n(sim.dat,2,income)

# 通过列名选取变量
# 选取 sim.dat数据框中的income，age和store_exp列
dplyr::select(sim.dat,income,age,store_exp)
# 选取列名中含有某字符串（_）的列
# 该命令将选取store_exp，online_exp，store_trans和online_trans
dplyr::select(sim.dat, contains("_"))
# 选取以某字符串（e）结尾的列
# 结果选取了age，income和house
# 类似的starts_with指以某字符串开始的列
dplyr::select(sim.dat, ends_with("e"))
# 选取列Q1，Q2，Q3，Q4和Q5
select(sim.dat, num_range("Q", 1:5)) 
# 选取列名在某字符串中的列
dplyr::select(sim.dat, one_of(c("age", "income")))
# 选取两个列名之间的列，包含头尾两列
dplyr::select(sim.dat, age:online_exp)
# 选出出了某列（age）以外的其它列
dplyr::select(sim.dat, -age)

### 数据总结
# 对列online_trans取均值，返回的是一个单一值
dplyr::summarise(sim.dat, avg_online = mean(online_trans)) 
# 对数据框中的每一列应用函数anyNA()
# 这里可以指定一个函数向量，如c("anyNA","is.factor")
dplyr::summarise_each(sim.dat, funs_(c("anyNA")))

# 对每个消费者类别对应变量应用anyNA()函数
sim.dat %>% group_by(segment) %>% summarise_each(funs_(c("anyNA")))

###  生成新变量
dplyr::mutate(sim.dat, total_exp = store_exp + online_exp)

# 这里的min_rank等价于rank(ties.method = "min")
# mutate_each()对每列应用指定的窗口函数
dplyr::mutate_each(sim.dat, funs(min_rank)) 

dplyr::transmute(sim.dat, total_exp = store_exp + online_exp) 

### 合并数据集

x<-data.frame(cbind(ID=c("A","B","C"),x1=c(1,2,3)))
y<-data.frame(cbind(ID=c("B","C","D"),y1=c(T,T,F)))
x
y

left_join(x,y,by="ID")
inner_join(x,y,by="ID")
full_join(x,y,by="ID")
semi_join(x,y,by="ID")
anti_join(x,y,by="ID")