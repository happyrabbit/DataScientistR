##################
##### 收缩线性回归
##################

# 需要前一个小节的代码得到相应数据
# 这里要用as.matrix(xtrain)将自变量输入转化成矩阵
fit = glmnet(as.matrix(trainx), trainy, alpha = 0.2, nlambda = 20)
# 这里digits=2限制了输出中的小数位数
print(fit, digits = 4)

plot(fit, xvar = "lambda", label = T)
plot(fit, xvar = "dev", label = T)

any(fit$lambda == 1000)
coef.exact = coef(fit, s = 1000, exact = T)
coef.apprx = coef(fit, s = 1000, exact = F)
cbind2(coef.exact, coef.apprx)

# 和之前一样，我们抽取一个小样本最为新自变量观测
newdat = matrix(sample(1:9, 30, replace = T), nrow = 3)
predict(fit, newdat, type = "response", s = 1000)

predict(fit, newdat, type = "coefficients", s = 1000)
predict(fit, newdat, type = "nonzero", s = 1000)
cvfit = cv.glmnet(as.matrix(trainx), trainy, type.measure = "mae", nfolds = 20)

# 抽取一个大样本来展示并行计算对效率的提高
X = matrix(rnorm(1e5*200), 1e5, 200)
Y = rnorm(1e5)
# 不用并行计算
system.time(cv.glmnet(X, Y))

library(doMC)
# 我的电脑是4核的，所以设置cores = 4
registerDoMC(cores = 4)
# 用并行计算
system.time(cv.glmnet(X, Y, parallel = T))

# 这里不展示输出结果
cvfit = cv.glmnet(as.matrix(trainx), trainy, type.measure = "mse", nfolds = 20)
# 最小均方误差对应的参数值
cvfit$lambda.min
# 预测新样本
predict(cvfit, newx = newdat, s= "lambda.min")
# 得到参数估计
coef(cvfit, s = "lambda.min")

# 自定义层级
foldid = sample(1:10, size = length(trainy), replace = T)
# 尝试3个不同的alpha取值：1、0.5、0.2和0
cv1 = cv.glmnet(as.matrix(trainx), trainy, foldid = foldid, alpha = 1)
cv.2 = cv.glmnet(as.matrix(trainx), trainy, foldid = foldid, alpha = .2)
cv.5 = cv.glmnet(as.matrix(trainx), trainy, foldid = foldid, alpha = .5)
cv0 = cv.glmnet(as.matrix(trainx), trainy, foldid = foldid, alpha = 0)
plot(log(cv1$lambda), cv1$cvm, pch = 19, col = 2, xlab = "log(Lambda)", ylab = cv1$name)
points(log(cv.5$lambda), cv.5$cvm, col = 1)
points(log(cv.2$lambda), cv.2$cvm, col = 3)
points(log(cv0$lambda), cv0$cvm, col = 4)
legend("topleft", legend = c("alpha = 1", "alpha = 0.5", "alpha = 0.2", "alpha = 0"), 
       pch = 19, col = c(2, 1, 3, 4))

# 先得到一个下限向量，设置成负无穷大
# 这时等于没有下限
lower.limits <- rep(-Inf, ncol(trainx))
# 在将需要的下限值加入
# Q3的估计是正数，所以下限为0
lower.limits[3] <- 0
# 类似的设置上限向量
upper.limits <- rep(Inf, ncol(trainx))
upper.limits[8] <- 0
boundfit = glmnet(as.matrix(trainx), trainy, lower.limits = lower.limits, upper.limits = upper.limits)
