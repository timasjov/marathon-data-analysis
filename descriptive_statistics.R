#Read in data
data = read.table("data/processedData.txt", header=T)

#Find correlation between attributes using t-test
t.test(data$s.nr, data$place)

t.test(data$particip.time, data$place)

data$age.group.number = as.numeric(substr(data$age.group, 2, 3))
t.test(data$age.group.number, data$place)

data$country.number = as.numeric(data$country)
t.test(data$country.number, data$place)

men = data[is.na(data[,"L.place"]),]
women = data[!is.na(data[,"L.place"]),]
t.test(men$time, women$time)



#Find average speeds between splits and create plots (meters in sec)
speedSplit1 <- splits$matu  / mean(data$split.1) * 1000
speedSplit2 <- splits$ande / (mean(data$split.2) - mean(data$split.1)) * 1000
speedSplit3 <- splits$puka / (mean(data$split.3) - mean(data$split.2)) * 1000
speedSplit4 <- splits$astuvere / (mean(data$split.4) - mean(data$split.3)) * 1000
speedSplit5 <- splits$palu / (mean(data$split.5) - mean(data$split.4)) * 1000 
speedSplit6 <- splits$hellenurme / (mean(data$split.6) - mean(data$split.5)) * 1000
speedLastSplit <- splits$finish / (mean(data$time) - mean(data$split.6)) * 1000
speeds <- c(speedSplit1, speedSplit2, speedSplit3, speedSplit4, speedSplit5, speedSplit6, speedLastSplit)

plot(as.vector(dist,mode='numeric')[2:8], speeds, type = "l", xlab="Splits (km)", ylab = "Speed (meters/sec)")
