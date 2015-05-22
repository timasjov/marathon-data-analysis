#Read in data
data = read.table("data/processedData.txt", header=T)

#DBSCAN
ds = dbscan(data[,c(paste("split.",1:6,sep=""), "time")], MinPts=2, eps=5)

dsData = cbind(data, ds$cluster)
dsData = dsData[order(ds$cluster),]

dsData[dsData[,"ds$cluster"] > 0, -c(1:3,5,13:15)]
