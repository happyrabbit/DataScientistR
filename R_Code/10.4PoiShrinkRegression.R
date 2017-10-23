#################
# 泊松收缩回归
#################

library(glmnet)
dat <- read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# 将10个问卷调查变量当作自变量
trainx <- dat[, grep("Q", names(dat))]
# 实体店消费次数当作应变量
trainy <- dat$store_trans
# 拟合泊松模型
fit <- glmnet(as.matrix(trainx), trainy, family = "poisson")

plot(fit, label = T)
cvfit <- cv.glmnet(as.matrix(trainx), trainy, family = "poisson")
plot(cvfit)

coef(fit, s=cvfit$lambda.min)
