#Reference
#https://rpubs.com/skydome20/R-Note9-Clustering
#https://ithelp.ithome.com.tw/articles/10187906
#http://kanchengzxdfgcv.blogspot.tw/2016/05/r-kmeans-cluster.html
#https://support.sisense.com/hc/en-us/articles/230652888-Anomaly-Detection-with-Sisense-using-R

#---------------check and install require packages-----------------
if( !require(ggplot)){
  install.packages("ggplot")
  require(ggplot)
} # end of if !ggplot

if( !require(factoextra)){
  install.packages("factoextra")
  require(factoextra)
} # end of if !factoextra

if( !require(useful)){
  install.packages("useful")
  require(useful)
} # end of if !useful

#---------Anomaly Detection tool from twitter---------------------
#---------input time and value------------------------------------
#install.packages("devtools")
#devtools::install_github("twitter/AnomalyDetection")
#library(AnomalyDetection)

#-----------------main --------------------------------------------
library(ggplot2)
csv <- read.csv("https://github.com/GlennShih/CS598R/raw/master/dos_20sec.csv", header = TRUE, sep = ",")
#csv <- read.csv("//Users//glennmbp//Desktop//dos_20sec.csv", header = TRUE, sep = ",")

csv_retrived <- csv[,1:6]

#kmeans clustering
##centers >>number of clusters ; �nstart >> repeat how many times
km2 <- kmeans(csv_retrived,centers = 8, nstart = 5)
set.seed(1000)
km2$cluster <- as.factor(km2$cluster)
#plot(csv_retrived,col=km2$cluster)
#plot(km2, data = csv_retrived)
#plot(km2, data = csv_retrived, class = "Cultivar") #not working due to not enought ram
ggplot(csv_retrived, aes(x = Protocol, y = Length )) +geom_point(aes(color = km2$cluster)) +stat_density2d(aes(color =  km2$cluster), h = 0.6)

#---------------Optimal number of clusters--------------------
#---------------A�Average Silhouette Method---------------------
#---------------WSS Method will blow up�炸-----------------------
require(factoextra)
fviz_nbclust(csv_retrived, 
             FUNcluster = kmeans,   # K-Means
             method = "silhouette", # Avg. Silhouette
             k.max = 15             # max number of clusters
) +
  labs(title="Avg.Silhouette Method for K-Means") 

#----------------K-Means---------------------------------------
fviz_cluster(km2,           # ??�群結�??
             data = csv_retrived,              # 資�??
             geom = c("point","text"), # 點�?��?�籤(point & label)
             frame.type = "norm")      # 框架??��??


#----------------Hierarchical Clustering-----------------------
head.csv_retrived= head(csv_retrived)         # ?��?��data?��?��下�?��?�個�?��?��?��?��??

E.dist <- dist(head.csv_retrived, method="euclidean") # 歐�?��?�離
M.dist <- dist(head.csv_retrived, method="manhattan") # ?��??��?��?�離
par(mfrow=c(1,2)) # 讓�?��?�以1x2??�方式�?�現，詳??��?��??(4)繪�??-資�?��?�覺???

# 使用歐�?��?�離?���?��?�群
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="歐�?��?�離")

# 使用?��??��?��?�離?���?��?�群
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="?��??��?��?�離")
#hclust(E.dist, method="single")   # ??�近�??
#hclust(E.dist, method="complete") # ??�??��??
#hclust(E.dist, method="average")  # 平�?��??
#hclust(E.dist, method="centroid") # 中�?��??
hclust(E.dist, method="ward.D2")  # ?��德�??
E.dist <- dist(data, method="euclidean")      # 歐�?��?�離
h.cluster <- hclust(E.dist, method="ward.D2") # ?��德�??

# 視覺???
plot(h.cluster)
abline(h=9, col="red")
cut.h.cluster <- cutree(h.cluster, k=3)  # ??��?��?�群
cut.h.cluster                            # ??�群結�??
table(cut.h.cluster)       # ??�群結�?��?�實??��?��?��?��??
