##################
## 初识glmnet
##################

library(glmnet)
dat <- read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# 对数据进行一些清理，删除错误的样本观测，消费金额不能为负数
dat <- subset(dat, store_exp > 0 & online_exp > 0)
# 将10个问卷调查变量当作自变量
trainx <- dat[, grep("Q", names(dat))]
# 将实体店消费量和在线消费之和当作应变量
# 得到总消费量=实体店消费+在线消费
trainy <- dat$store_exp + dat$online_exp
glmfit = glmnet(as.matrix(trainx), trainy)

plot(glmfit, label = T)
print(glmfit)
coef(glmfit, s = 1200)
newdat = matrix(sample(1:9, 30, replace = T), nrow = 3)
predict(glmfit, newdat, s = c(1741, 2000))
cvfit = cv.glmnet(as.matrix(trainx), trainy)
plot(cvfit)

# 最小均方误差对应的参数值
cvfit$lambda.min

# 一个标准差原则下对应的参数值
cvfit$lambda.1se

# 一个标准差原则下对应的回归参数估计
coef(cvfit, s = "lambda.1se")
