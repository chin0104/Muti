

library(DataExplorer)
library(ClusterR)
library(cluster)
library(ggfortify)
library(stats)
library(fpc)
library(factoextra)
library(corrplot)

##import##
file = 'C:/Users/COMPUTER/Desktop/DATA/For R/phonP.csv'
data = read.csv(file)
data

introduce(data)

standD_data <- scale(data)
standD_data

res.pca = prcomp(standD_data, scale = TRUE)
print(summary(res.pca))

fviz_eig(res.pca,addlabels = TRUE)
var = get_pca_var(res.pca)

fviz_pca_var(res.pca, col.var = "black")

corrplot(var$cos2, is.corr = FALSE)

fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB","#E7B880","#FC4E07"))

ind <- get_pca_ind(res.pca)
fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB","#E7B880","#FC4E07"))

pca_components <- res.pca$x[, 1:14]
pca_components # with PCA
standD_data # without PCA

#------------------------------------------------------------------------------
#Hierarchical no PCA
test = (standD_data[1:50,])
test

distance_mat <- dist(test,method = 'euclidean')
distance_mat
Hierar_cl <- hclust(distance_mat)
Hierar_cl

plot(Hierar_cl)

plot(Hierar_cl)
abline(h=7,col = "red")

fit<- cutree(Hierar_cl,k=10)
fit
plot(Hierar_cl)
table(fit)
rect.hclust(Hierar_cl,k=10,border = "green")

#Hierarchical PCA
test_pca = (pca_components[1:50,])
test_pca

distance_mat_pca <- dist(test_pca,method = 'euclidean')
distance_mat_pca
Hierar_cl_pca <- hclust(distance_mat_pca)
Hierar_cl_pca

plot(Hierar_cl_pca)

plot(Hierar_cl_pca)
abline(h=7,col = "red")

fit<- cutree(Hierar_cl_pca,k=7)
fit
plot(Hierar_cl_pca)
table(fit)
rect.hclust(Hierar_cl_pca,k=7,border = "green")
#------------------------------------------------------------------------------
#K-mean no PCA
SR = 25
sse <-numeric(SR)
for (k in 1:SR) {
  kmeans_model <- kmeans(standD_data, centers = k)
  sse[k] <- kmeans_model$tot.withinss
}

ggplot(data.frame(K = 1:SR, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()
##selec k = 10
kmeans_result <- kmeans(standD_data,center = 10)
kmeans_result

autoplot(kmeans_result,standD_data,frame=TRUE)

#K-mean PCA
SR = 25
sse <-numeric(SR)
for (k in 1:SR) {
  kmeans_model <- kmeans(pca_components, centers = k)
  sse[k] <- kmeans_model$tot.withinss
}

ggplot(data.frame(K = 1:SR, SSE = sse), aes(x = K, y = SSE)) +
  geom_line() +
  geom_point() +
  labs(x = "จำนวนกลุ่ม (K)", y = "SSE") +
  theme_minimal()

kmeans_result <- kmeans(pca_components,center = 10)
kmeans_result

autoplot(kmeans_result,pca_components,frame=TRUE)
#------------------------------------------------------------------------------
##DBScan euclidean without pca
test = (standD_data[1:50,])
dist_matrix <- proxy::dist(standD_data,method = "Euclidean")

Db_cl <- dbscan::dbscan(dist_matrix,eps = 4, minPts = 40)
Db_cl 

Db_cl$cluster

plot(standD_data, col = Db_cl$cluster)

##DBScan euclidean with pca
dist_matrix_pca <- proxy::dist(pca_components,method = "Euclidean")

Db_cl_pca <- dbscan::dbscan(dist_matrix_pca,eps = 4, minPts = 40)
Db_cl_pca 

Db_cl_pca$cluster

plot(standD_data, col = Db_cl_pca$cluster)

