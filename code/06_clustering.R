#------------ Classification hierarchique ascendante ----------------#
res.daisy<-daisy(data_afm, metric="gower")       # 
res.hc<-hclust(res.daisy, method  = "ward.D2")   #
plot(res.hc, labels=FALSE)                       #
nbclusters <- 4
rect.hclust(res.hc, k=nbclusters, border="red")  #
set.seed(123)
#--------- COnsolidation des clusters avec du kmeans
res.km <-kmeans(res.daisy, centers=nbclusters)
cluster.ind<- res.km$cluster

data_cluster<- as.data.frame(cbind(data_afm, cluster.ind))

#------------------- Visualisation des clusters --------------------------------
fviz_cluster(res.km, data = dh,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


#----------------- Determination du k optimal --------------------------------
k.max <- 10
wss <- sapply(1:k.max, 
              function(k){kmeans(dh, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")







