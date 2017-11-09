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
##centers >>number of clusters ; Œnstart >> repeat how many times
km2 <- kmeans(csv_retrived,centers = 8, nstart = 5)
set.seed(1000)
km2$cluster <- as.factor(km2$cluster)
#plot(csv_retrived,col=km2$cluster)
#plot(km2, data = csv_retrived)
#plot(km2, data = csv_retrived, class = "Cultivar") #not working due to not enought ram
ggplot(csv_retrived, aes(x = Protocol, y = Length )) +geom_point(aes(color = km2$cluster)) +stat_density2d(aes(color =  km2$cluster), h = 0.6)

#---------------Optimal number of clusters--------------------
#---------------AœAverage Silhouette Method---------------------
#---------------WSS Method will blow up†ç‚¸-----------------------
require(factoextra)
fviz_nbclust(csv_retrived, 
             FUNcluster = kmeans,   # K-Means
             method = "silhouette", # Avg. Silhouette
             k.max = 15             # max number of clusters
) +
  labs(title="Avg.Silhouette Method for K-Means") 

#----------------K-Means---------------------------------------
fviz_cluster(km2,           # ??†ç¾¤çµæ??
             data = csv_retrived,              # è³‡æ??
             geom = c("point","text"), # é»žå?Œæ?™ç±¤(point & label)
             frame.type = "norm")      # æ¡†æž¶??‹æ??


#----------------Hierarchical Clustering-----------------------
head.csv_retrived= head(csv_retrived)         # ?¾?œ¨data?ª?‰©ä¸‹å?å?›å€‹æ?„ä?ç?„è?‡æ??

E.dist <- dist(head.csv_retrived, method="euclidean") # æ­å?è?é›¢
M.dist <- dist(head.csv_retrived, method="manhattan") # ?›¼??ˆé?“è?é›¢
par(mfrow=c(1,2)) # è®“å?–ç?‡ä»¥1x2??„æ–¹å¼å?ˆç¾ï¼Œè©³??…è?‹è??(4)ç¹ªå??-è³‡æ?™è?–è¦º???

# ä½¿ç”¨æ­å?è?é›¢?€²è?Œå?†ç¾¤
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="æ­å?è?é›¢")

# ä½¿ç”¨?›¼??ˆé?“è?é›¢?€²è?Œå?†ç¾¤
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="?›¼??ˆé?“è?é›¢")
#hclust(E.dist, method="single")   # ??€è¿‘æ??
#hclust(E.dist, method="complete") # ??€?? æ??
#hclust(E.dist, method="average")  # å¹³å?‡æ??
#hclust(E.dist, method="centroid") # ä¸­å?ƒæ??
hclust(E.dist, method="ward.D2")  # ?¯å¾·æ??
E.dist <- dist(data, method="euclidean")      # æ­å?è?é›¢
h.cluster <- hclust(E.dist, method="ward.D2") # ?¯å¾·æ??

# è¦–è¦º???
plot(h.cluster)
abline(h=9, col="red")
cut.h.cluster <- cutree(h.cluster, k=3)  # ??†æ?ä?‰ç¾¤
cut.h.cluster                            # ??†ç¾¤çµæ??
table(cut.h.cluster)       # ??†ç¾¤çµæ?œå?Œå¯¦??›ç?æ?œæ?”è??
