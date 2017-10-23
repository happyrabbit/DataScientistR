# 5.1 数据读写
## 取代传统数据框的tibble对象

library(tibble)
library(dplyr)
library(ggplot2)
sim.dat<-read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")

# 创建一个小数据框
df=data.frame(x=c(1:5),y=rep("a",5))
as_tibble(df)

##--------------
tibble(x = 1:5,
       y = 1,
       z = x ^ 2 + y
       )
##--------------
tb <- tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
print(tb)
##--------------
tb=tibble(
  `:)` = 1:5,
  ` ` = 1,
  `T_T` = `:)` ^ 2 + ` `
)
ggplot(tb,aes(`:)`,`T_T`))+geom_point()
##--------------
## 输出显示
print(as_tibble(sim.dat))
print(sim.dat,n = 15, width = Inf)

package?tibble

## 截取变量
# 通过变量名截取，不显示输出
# sim.dat$age 
# 通过变量名截取
# sim.dat[["age"]]
# 通过位置截取
# sim.dat[[1]]

library(dplyr)
sim.dat%>%.$age
sim.dat%>%.[["age"]]

sim.dat=as.data.frame(sim.dat)
class(sim.dat)

## 高效数据读写：`readr`包

library(readr)
sim.dat <- read_csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv ")

dat=read_csv("a,b,c
1,2,3
4,5,6")
print(dat)

dat=read_csv("这是个样本数据
          这行只是注释
             x,y,z
             1,2,3", skip = 2)
print(dat)

dat=read_csv("1,2,3\n4,5,6", col_names = FALSE)
print(dat)

dat=read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))
print(dat)

dat=read_csv2("x;y;Z\n1;2;3")
print(dat)

dat=read_tsv("x\ty\tz\n1\t2\t3")
print(dat)

dat=read_delim("x|y|z\n1|2|3", delim = "|")
print(dat)

dat=read_csv("x,y,z\n1,2,99",na="99")
print(dat)

write_csv(sim.dat, "sim.dat.csv")