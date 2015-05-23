#Read in raw data
data = read.csv2("data/rm_2014_lp.csv", header=T, skip = 5, na.strings="")

#Function to convert time string to seconds
charToSec = function(x){
  if(!is.na(x)){
    incr = c(3600, 60, 1)
    vals = sapply(strsplit(as.character(x),":"), FUN=function(y){as.numeric(y)})
    return(sum(incr*vals))
  }else{
    return(NA)
  }
}

#Convert timestamps to seconds
for(i in 1:6){
  data[, paste("split.",i,sep="")]=sapply(data[,paste("split.",i,sep="")], FUN=function(x){charToSec(x)})  
}
data[, "time"]=sapply(data[, "time"], FUN=function(x){charToSec(x)})  


#Data imputation(currently we just leave rows with missing data out)
data = data[rowSums(is.na(data[,paste("split.",1:6,sep="")]))==0,]

#Add gender
data$gender = 0
data[is.na(data[,"L.place"]),]$gender <- "male"
data[!is.na(data[,"L.place"]),]$gender <- "female"

#Add unisex agegroup
data$age.group2 <- as.numeric(substr(data$age.group, 2, 3))

#Write out preprocessed data
write.table(data, "data/processedData.txt", sep="\t", row.names=F) 

#Write out split distances
dist = data.frame(0.0, 12.3, 22.9, 36.5, 50.6, 66.3, 77.2, 89.0)
splitNames <- c("Start", "Matu", "Ande", "Puka", "Astuvere", "Palu", "Hellenurme", "Finish")
colnames(dist) <- splitNames
write.table(dist, "data/distances.txt", sep="\t", row.names=F) 

#Calculate and write out distances between splits
splits = c(0)
for(i in 2:8){
  splits <-  c(splits, dist[i] - dist[i-1])
}
splits <- as.data.frame(splits)
colnames(splits) <- splitNames
write.table(splits, "data/splits.txt", sep="\t", row.names=F) 
