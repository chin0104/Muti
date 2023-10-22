#CH5
library(ggplot2)
library(factoextra)
data("decathlon2")
head(decathlon2)
#data format
decathlon2.active = decathlon2[1:23,1:10]
head(decathlon2.active[,1:6],4)

pairs(decathlon2.active[,1:10], pch = 19,lower.panel = NULL)

res.pca = prcomp(decathlon2.active,scale = TRUE)
print(summary(res.pca))

get_eigenvalue(res.pca)
fviz_eig(res.pca,addlabels = TRUE,ylim = c(0,50))

#graph of variable
var = get_pca_var(res.pca)
var$coord
var$cor
var$cos2

#corrleation circle
fviz_pca_var(res.pca,col.var = "black")

#quality of represemtation
library(corrplot)
corrplot(var$cos2,is.corr = FALSE)
fviz_pca_var(res.pca,col.var = "cos2",
             gradinet.cols = c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE
             )
var$contrib             
fviz_contrib(res.pca,choice = "var",axes = 1,top=10)
fviz_contrib(res.pca,choice = "var",axes = 2,top=10)

#graph of individuals
ind <- get_pca_ind(res.pca)
ind

fviz_pca_ind(res.pca,col.ind = "cos2",
             gradinet.cols = c("#00AFBB","#E7B800","#FC4E07"),
             repel = TRUE
)
head(ind$coord)

fviz_pca_biplot(res.pca,repel = TRUE,
                col.var = "#2E9FDE",
                col.ind = "#696969")

#Tranform the new data by PCA
ind.sup = decathlon2[24:27,1:10]
ind.sup.coord <- predict(res.pca,newdata = ind.sup)
ind.sup.coord
#Supplementary elements
library(FactoMineR)
res.pca <- PCA(decathlon2,ind.sup = 24:27,
               quanti.sup = 11:12,quali.sup = 13,graph = FALSE)
res.pca$quanti.sup
fviz_pca_var(res.pca)

#individuals
res.pca$ind.sup

#How to export results
scree.plot <- fviz_eig(res.pca)
ind.plot <- fviz_pca_ind(res.pca)
var.plot <- fviz_pca_var(res.pca)
library(ggpubr)
ggexport(plotlist = list(scree.plot,ind.plot,var.plot),nrow = 2,ncol = 2, filename = "PCAplot.pdf")
