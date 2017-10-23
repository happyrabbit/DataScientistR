###########################
#  神经网络(Neural Networks)
###########################

v <- seq(-10,10,length=200)
s1 <- 1
s2 <- 0.3
s3 <- 20
h1 <- 1/(1+exp(-s1*v))
h2 <- 1/(1+exp(-s2*v))
h3 <- 1/(1+exp(-s3*v))
plot(v,h1,type="l",col=1, ylab="h", lty = 1)
lines(v,h2,col=2, lty = 2)
lines(v,h3,col=3, lty = 3)
legend("topleft", c("s=1","s=0.3","s=20"), lty = c(1,2,3), col = c(1,2,3))
