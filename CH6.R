#CH6
install.packages("psych")
library(psych)
install.packages("GPArotation")
library(GPArotation)

dat = EFA

datcorr = cor(dat)
datKMO = KMO(datcorr)
print(datKMO$MSA)

#test by Bartlett Shpericity
datcorr = cor(dat)
datKMO = KMO(datcorr)
n = 90
datBart = cortest.bartlett(datcorr,n)
print(datBart$p.value)

datEig = eigen(datcorr)
datEigmean = mean(datEig$values)
numfac = sum(datEig$values>=1)
print(numfac)

datFa = principal(datcorr,nfactors = 4,rotate = "none")
datFa
