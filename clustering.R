library (dplyr)
clusterdata<-na.omit(dfgeo2)
clusterdata<-select(clusterdata,c("X","Y"))
#clusterdata<-scale(clusterdata)

#determine number of clusters
wss <- (nrow(clusterdata)-1)*sum(apply(clusterdata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(clusterdata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares") 

#13 clusters
fit <- kmeans(clusterdata, 13) # 5 cluster solution
# get cluster means
aggregate(clusterdata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
clusterdata <- data.frame(clusterdata, fit$cluster) 

library(ggmap)
#baton rouge geocoordinates: -91.221832,30.344436,-90.998726,30.559214
#bbox<-c(-91.221832,30.344436,-90.998726,30.559214)
#load map of Baton Rouge
#brmap<-get_map(location=bbox,source="stamen",maptype="toner-lite")
#plotting crime points on map
clustermap<-ggmap(brmap)+
  geom_point(data=clusterdata,aes(x=X,y=Y,color=fit.cluster),size=0.5,alpha=0.01)+
  labs(title="Crimes Clustered by Geographic Location")+
  theme(legend.position = "none")