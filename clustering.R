#Read in data
data = read.table("data/processedData.txt", header=T)

#DBSCAN - currently we find people who were in the same pace group
#throughout the entire race(at least 2 people to make a pace group)
ds = dbscan(data[,c(paste("split.",1:6,sep=""), "time")], MinPts=2, eps=10)

#Order data by cluster
dsData = cbind(data, ds$cluster)
dsData = dsData[order(ds$cluster),]

#Show clusters
dsData[dsData[,"ds$cluster"] > 0, -c(1:3,5,13:24)]



#Clustering relevance - show that people who were part of a pace
#group were more consistent, experienced and had better results

#Average cluster size - i.e how big the pace groups were on average?
round(mean(table(dsData[,"ds$cluster"])[-1]),0)

#Show that people who were part of a pace group had
#better results than people who went solo
solo = dsData[dsData[,"ds$cluster"] == 0, "time"]
pace = dsData[dsData[,"ds$cluster"] > 0, "time"]
t.test(solo, pace)
dsData$pace = dsData[,"ds$cluster"] > 0
ggplot(dsData, aes(x = pace, y = time, fill = pace)) +
  geom_boxplot() +
  labs(title = "Time per pace group", y = "time (sec)")

# % of people who ran in pace group
length(pace)*100/(length(solo)+length(pace))

# % of the people in pace group who had completed
#at least 1 marathon before
nrow(dsData[dsData[,"ds$cluster"]>1 & dsData[,"particip.time"] > 1, ])*100/length(pace)



#Clustering visualization

#Overall heatmap - normalize by columns to make the splits
#comparable, since time increases with each split
pheatmap(data[,c(paste("split.",1:6,sep=""), "time")], cluster_rows = F,
         cluster_cols = F, scale="column", show_rownames = F)

#Clustered heatmap - hierarchical
pheatmap(data[,c(paste("split.",1:6,sep=""), "time")], cluster_rows = T,
         cluster_cols = F, scale="column", show_rownames = F)

#Clustered heatmap - dbscan(clusters appear at the bottom, rest is noise)
pheatmap(dsData[,c(paste("split.",1:6,sep=""), "time")], cluster_rows = F,
         cluster_cols = F, scale="column", show_rownames = F) 

#Heatmap of dbscan clusters
pheatmap(dsData[dsData[,"ds$cluster"] > 0 ,c(paste("split.",1:6,sep=""), "time")], 
         cluster_rows = F, cluster_cols = F, scale="column", show_rownames = F)


