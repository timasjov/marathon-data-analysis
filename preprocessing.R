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
data$gender <- 0
data[is.na(data[,"L.place"]),]$gender <- "male"
data[!is.na(data[,"L.place"]),]$gender <- "female"

#Add unisex agegroup
data$age.group2 <- as.numeric(substr(data$age.group, 2, 3))

#Add nationality
countries <- levels(data$country)
counties <- countries[c(1,3,4,7,8,11,12,14,15,16,18,22,23,24,26,27)]
data$nationality <- 0
data[is.element(data$country, counties),]$nationality <- "Estonia"
data[!is.element(data$country, counties),]$nationality <- "foreign"

#Split final times into 10 groups
data$timeCategory <- ntile(data$time, 10)

#Split start numbers into 10 groups
data$sNrCategory <- ntile(data$s.nr, 5)

#Split number of participations into 10 groups
data$participTimeCategory <- ntile(data$particip.time, 5)

#Split final place into 10 groups
data$placeCategory <- ntile(data$place, 10)

#Combine all Estonian participants
data$countryCategory <- data$country
levels(data$countryCategory) <- c(levels(data$countryCategory), "Eesti")
data[data$country %in% c(
  "Harju", "Hiiumaa", "Ida-Viru", "Jõgeva", "Järvamaa", "Lääne-Viru", "Läänemaa", 
  "Pärnu", "Rapla", "Saaremaa", "Tallinn", "Tartu", "Valga", "Viljandi", "Võru", "Põlva"), "countryCategory"] <- "Eesti"

#Add county
data$county <- data$country
levels(data$county) <- c(levels(data$county), "other")
data$county[!is.element(data$country, counties)] <- "other"

#Combine age categories
data$ageCategory <- data$age.group2
levels(data$ageCategory) <- c(levels(data$ageCategory), c("17-21", "35-45", "50-60", "65+"))
data[data$age.group2 %in% c("17", "20", "21"), "ageCategory"] <- "17-21"
data[data$age.group2 %in% c("35", "40", "45"), "ageCategory"] <- "35-45"
data[data$age.group2 %in% c("50", "55", "60"), "ageCategory"] <- "50-60"
data[data$age.group2 %in% c("65", "70", "75"), "ageCategory"] <- "65+"


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

#Distance and population data
distance <- c(186,305,130,53,103,123,249,49,174,157,328,186,0,86,78,71)
participants <- c(434,6,40,43,44,103,35,54,88,56,36,755,605,114,67,71)
population <- c(153648,9709,153312,32275,31688,61099,25513,29169,85539,34989,34485,434339,148673,31790,49210,35079)
dist <- data.frame(counties, population, distance, participants)
