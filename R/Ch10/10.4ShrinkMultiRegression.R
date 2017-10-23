#############
# 收缩多项回归
#############

library(glmnet)
dat <- read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/SegData.csv")
# 将10个问卷调查变量当作自变量
trainx <- dat[, grep("Q", names(dat))]
# 将消费者类别当作应变量
trainy <- dat$segment

fit <- glmnet(as.matrix(trainx), trainy, family = "multinomial")
plot(fit, xvar = "lambda", label = T, type.coef = "2norm")

cvfit <- cv.glmnet(as.matrix(trainx), trainy, family = "multinomial")
plot(cvfit)

cvfit$lambda.min

newdat = matrix(sample(1:9, 60, replace = T), nrow = 6)
predict(cvfit, newdat, s = "lambda.min", type = "class")
