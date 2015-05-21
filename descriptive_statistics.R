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


#Find average speeds between splits and create plots (meters in min)
metersInMin <- function(x) {
  return(x * 1000 * 60)
}

speedSplit1 <- metersInMin(splits$matu  / mean(data$split.1))
speedSplit2 <- metersInMin(splits$ande / (mean(data$split.2) - mean(data$split.1)))
speedSplit3 <- metersInMin(splits$puka / (mean(data$split.3) - mean(data$split.2)))
speedSplit4 <- metersInMin(splits$astuvere / (mean(data$split.4) - mean(data$split.3)))
speedSplit5 <- metersInMin(splits$palu / (mean(data$split.5) - mean(data$split.4)))
speedSplit6 <- metersInMin(splits$hellenurme / (mean(data$split.6) - mean(data$split.5)))
speedLastSplit <- metersInMin(splits$finish / (mean(data$time) - mean(data$split.6)))
speeds <- c(speedSplit1, speedSplit2, speedSplit3, speedSplit4, speedSplit5, speedSplit6, speedLastSplit)

plot(as.vector(dist,mode='numeric')[2:8], speeds, type = "l", col="red", xlab="Splits (km)", ylab = "Speed (meters/min)")
