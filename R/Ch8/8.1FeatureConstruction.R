###########
# 特征构建
###########

library(readr)
library(dplyr)
topic<-read_csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/topic.csv")
glimpse(topic)

# 将发帖时间提取出来，存在posted_at2对象中
posted_at2<-topic$posted_at

# 将截取的年份存在名为year的列中
topic$year<-substr(posted_at2,1,4)
# 看下结果如何
car::some(topic$year)

barplot(table(topic$year),family ="Songti SC", main="年度发帖数目频数直方图")

topic$month<-substr(posted_at2,6,7)
barplot(table(topic$month),family ="Songti SC", main="月度发帖数目频数直方图")

topic$time<-substr(posted_at2,12,13)
barplot(table(topic$time),family ="Songti SC",main="每日不同时间段发帖频数直方图")




