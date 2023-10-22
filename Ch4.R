#Part 1
#MANOVA
x1bar = matrix(c(2.006,0.48,0.082,0.36),4,1)
x2bar = matrix(c(2.167,0.596,0.124,0.418),4,1)
x3bar = matrix(c(2.273,0.521,0.125,0.383),4,1)
S1 = matrix(c(0.291,-0.001,0.002,0.01,-0.011,0.011,0.001,0.003,0.002,0.001,0.001,0.001,0.01,0.003,0.001,0.01),4,4)
S2 = matrix(c(0.561,0.001,0.001,0.037,0.011,0.025,0.004,0.007,0.001,0.004,0.005,0.002,0.037,0.007,0.002,0.019),4,4)
S3 = matrix(c(0.261,0.03,0.003,0.018,0.03,0.017,-0.001,0.006,0.003,-0.001,0.004,0.001,0.018,0.006,0.001,0.013),4,4)

W = (271-1)*S1+(138-1)*S2+(107-1)*S3
W
xbar = (271*x1bar+138*x2bar+107*x3bar)/(271+138+107)
xbar

B = (271*(x1bar-xbar)%*%t(x1bar-xbar))+(138*(x2bar-xbar)%*%t(x2bar-xbar))+(107*(x3bar-xbar)%*%t(x3bar-xbar))

lamda = det(W)/det(B+W)
comp_val = ((516-4-2)/4)*((1-sqrt(lamda))/sqrt(lamda))
qf(1-0.05,8,1020)

#part 2
X1 = matrix(c(21,25,20,24,12,8,12,10),4,2)
X2 = matrix(c(31,23,24,28,9,12,13,10),4,2)
X3 = matrix(c(34,29,35,32,10,14,11,13),4,2)
X4 = matrix(c(33,38,34,35,14,12,13,13),4,2)

X1mean = colMeans(X1)
X2mean = colMeans(X2)
X3mean = colMeans(X3)
X4mean = colMeans(X4)

paste('X1mean[1] = ',X1mean[1],'ll X1mean[2] = ',X1mean[2],sep="")
paste('X2mean[1] = ',X2mean[1],'ll X2mean[2] = ',X2mean[2],sep="")
paste('X3mean[1] = ',X3mean[1],'ll X3mean[2] = ',X3mean[2],sep="")
paste('X4mean[1] = ',X4mean[1],'ll X4mean[2] = ',X4mean[2],sep="")

n1 = length(X1[,1])
n2 = length(X2[,1])
n3 = length(X3[,1])
n4 = length(X4[,1])

p = length(X1[1,])
s1 = cov(X1)
s2 = cov(X2)
s3 = cov(X3)
s4 = cov(X4)

W = (n1-1)*s1+(n2-1)*s2+(n3-1)*s3+(n4-1)*s4
xbar = (n1*X1mean+n2*X2mean+n3*X3mean+n4*X4mean)/(n1+n2+n3+n4)
B = (n1*(X1mean-xbar)%*%t(X1mean-xbar))+(n2*(X2mean-xbar)%*%t(X2mean-xbar))+(n3*(X3mean-xbar)%*%t(X3mean-xbar))+(n4*(X1mean-xbar)%*%t(X4mean-xbar))
paste('**************************')

Lamda = det(W)/det(B+W)
paste('Lamda =',Lamda)

#stat test
statistic = (((n1+n2+n3+n4)-4-1)/(4-1))*((1-sqrt(Lamda))/sqrt(Lamda))
paste('statistic =',statistic)
#Ftest at alfa 0.05
cri_val = qf(1-0.05,6,27)
paste('Critical Value = ',cri_val)

x1 = c(21,25,20,24,31,23,24,28,34,29,35,32,33,38,34,35)
x2 = c(12,8,12,10,9,12,13,10,10,14,11,13,13,12,13,13)
g = c(rep(1,4),rep(2,4),rep(3,4),rep(4,4))
x_level = cbind(x1,x2,g)
x_level

library(biotools)
X = x_level[,c(1,2)]
boxM(X,g)

group = as.factor(X_level[,3])
group

fit = manova(X~group)
summary(fit,test = "Wilks")
