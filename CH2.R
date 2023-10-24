install.packages("DataExplorer")
install.packages("googledrive")

library(DataExplorer)
library(tidyr)
library(googledrive)
#Credit data
data = CreditApproval

introduce(data)
head(data)
##missing chenk
plot_missing(data)

datac = apply(is.na(data),2,which)
datac = drop_na(data)
head(datac)

plot_bar(data[,c('A1','A4','A5')])

plot_scatterplot(data[,c('A2','A3')],by='A2')

plot_histogram(data[,c('A2')])

summary(data)

install.packages("MASS")
install.packages("MVN")

library(MASS)
library(MVN)
library(ggplot2)

n = 10
mu = c(0,0)
sigma = matrix(c(10,3,3,2),2,2)
set.seed(1)
data2 = mvrnorm(n,mu,sigma)

result = mvn(data2,mvnTest = 'energy')

result = mvn(data2,mvnTest = 'energy',univariatePlot = 'ggplot',multivariatePlot = 'qq')

result = mvn(data2,mvnTest = 'energy', multivariatePlot = 'qq')


#iris data
library(readxl)
iris = iris_xcel
data_iris = iris[(1:4)]
data_iris

##Normality test
result_data_iris = mvn(data_iris,mvnTest = 'energy')
result_data_iris = mvn(data_iris,mvnTest = 'energy',univariatePlot = 'qqplot')
result_data_iris = mvn(data_iris,mvnTest = 'energy',multivariatePlot = 'qq')

