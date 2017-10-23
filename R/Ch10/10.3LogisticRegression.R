#################
### 逻辑回归
#################

#################
#### 普通逻辑回归
#################

library(MASS)
dat <- read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/sim1_da1.csv")
fit <- glm(y~., dat, family = "binomial")

#################
#### 收缩逻辑回归
#################

dat <- read.csv("https://raw.githubusercontent.com/happyrabbit/DataScientistR/master/Data/sim1_da1.csv")
trainx = dplyr::select(dat, -y)
trainy = dat$y
fit <- glmnet(as.matrix(trainx), trainy, family = "binomial")
plot(fit, xvar = "dev")

levels(as.factor(trainy))

newdat = as.matrix(trainx[1:3, ])
predict(fit, newdat, type = "link", s = c(2.833e-02, 3.110e-02))

# 老用as.matrix(trainx)确实有些烦人，但是函数要求时矩阵格式。
# 小伙伴可以在一开始选择将trainx直接转化成矩阵格式。
# 这里不这么做的原因是矩阵格式下有的数据框的操作又无法进行
# 且一些数据框的行列信息可能在转化过程中丢失。
# 所以在每次拟合模型的时候临时转化而不更改原数据框
cvfit = cv.glmnet(as.matrix(trainx), trainy, family = "binomial", type.measure = "class")
plot(cvfit)

cvfit$lambda.min
cvfit$lambda.1se
