#CH1
##สร้าง Matrix
x = matrix(c(2,3,7,4),4,1)
print(x)

#CH3
vecmu = matrix(c(-4,4),2,1)
vecmu

sigma = matrix(c(16,-2,-2,9),2,2)
sigma

##1-sample Hottlling test
library(MASS)
set.seed(123)
n = 15
sample1 = mvrnorm(n,vecmu,sigma)
head(sample1)

install.packages("DescTools")
library(DescTools)
muH0 = c(-1,2)
HotellingsT2Test(sample1, mu = muH0)

muH0 = c(-4,4)
HotellingsT2Test(sample1, mu = muH0)

##2-sample Hottlling test
vecmu1 = matrix(c(-4,4),2,1)
vecmu1

sigma = matrix(c(16,-2,-2,9),2,2)
sigma

#สร้างsample1
n1 = 15
set.seed(123)
sample1 = mvrnorm(n1,vecmu1,sigma)
head(sample1)

#สร้างsample2
n2 = 18
set.seed(321)
vecmu2 = matrix(c(3,2),2,1)
sample2 = mvrnorm(n2,vecmu2,sigma)
head(sample2)

library(corpcor)
library(Hotelling)
Htest = hotelling.test(sample1,sample2,var.equal = TRUE)
Htest

##Box M test
nrow = n1+n2
cl = matrix(0,nrow,1)
cl[1:15] = 1
cl[16:nrow] = 2
head(cl)
tail(cl)
saple_merge = rbind(sample1,sample2)
sample_merge = cbind(saple_merge,cl)
head(sample_merge)

###homogeneity test
library(biotools)

results = boxM(data=sample_merge[,1:2],group=sample_merge[,3])
results

CLab = matrix(c(6,6,18,8,11,34,28,71,43,33,20,27,23,64,44,30,75,26,124,54,30,14),11,2)

SLab = matrix(c(25,28,36,35,15,44,42,54,34,29,39,15,13,22,29,31,64,30,64,56,20,21),11,2)

DLab = SLab - CLab
head(DLab)

muH0 = c(0,0)
Htest = HotellingsT2Test(DLab,mu = muH0)
Htest

##MANOVA
library(readxl)
data = read_excel("C:/Users/COMPUTER/Desktop/DATA/For R/iris_xcel.xlsx")
data$Species = factor(data$Species)

res.man = manova(cbind(Sepal.Length,Sepal.Width,Petal.Length,Petal.Width) ~ Species, data = data)
summary(res.man)
