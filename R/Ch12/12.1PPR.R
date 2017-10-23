#############################################
# 投影寻踪回归（PROJECTION PURSUIT REGRESSION）
#############################################

# 我们用plot3D包绘制3维图
library(plot3D)
# 得到x1和x2
# 注意这里x1和x2要是需要是矩阵的形式
# 你可以分别查看这两个变量会发现：
# x1中每列都是相同的
# x2的每行都是相同的
# 这里的mesh()是plot3D中的函数，用于生成2维或者3维阵列
# 这里不是很好理解，大家可能得稍微想想
M <- mesh(seq(-13.2, 13.2, length.out = 50),
          seq(-37.4, 37.4, length.out = 50))
x1 <- M$x
x2 <- M$y
##----- 第1种函数设置
# 将X映射到w上得到v
v <- (1/2)*x1+(sqrt(3)/2)*x2
# 将函数g()应用于v
g1<-1/(1+exp(-v))
par(mfrow = c(2, 2), mar = c(0, 0, 1, 0))
surf3D(x1,x2,g1,colvar = g1, border = "black", colkey = FALSE, box = FALSE, main = "1")
##----- 第2种函数设置
v <- x1
g2 <- (v+5)*sin(1/(v/3+0.1))
surf3D(x1,x2,g2,colvar = g2, border = "black", colkey = FALSE, box = FALSE, main = "2")
##----- 第3种函数设置
v <- x2
g3 <- exp(v^2/5)
surf3D(x1,x2,g3,colvar = g3, border = "black", colkey = FALSE, box = FALSE, main = "3")

##----- 第4种函数设置
v <- x1
g4 <- (v+0.1)*sin(1/(v/3+0.1))
surf3D(x1,x2,g4,colvar = g4, border = "black", colkey = FALSE, box = FALSE, main = "4")

