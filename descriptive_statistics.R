#Read in data
data <- read.table("data/processedData.txt", header=T)

data$gender <- 0
data[is.na(data[,"L.place"]),]$gender <- "male"
data[!is.na(data[,"L.place"]),]$gender <- "female"

# Statistical analysis
var.test(data$time~data$gender)
### Result: unequal variance

## t-test
t.test(data$time~data$gender, var.equal = F)
### Result: finishing time is significantly different between men and women

## ANOVA
summary(aov(data$time~data$age.group2))
boxplot(data$time~data$age.group2)
### Result: finishing time is significantly different between agegroups
summary(aov(data$time~data$country))
boxplot(data$time~data$country)
### Result: finishing time is significantly different between countries

## Chi-square test
data$age.group2 <- as.numeric(substr(data$age.group, 2, 3))

tbl <- table(data$gender, data$age.group2)
ctbl <- cbind(tbl[,"17"]+tbl[,"20"],
                tbl[,"21"],
                tbl[,"35"],
                tbl[,"40"],
                tbl[,"45"],
                tbl[,"50"]+tbl[,"55"]+tbl[,"60"]+tbl[,"65"]+tbl[,"70"]+tbl[,"75"])
colnames(ctbl) = c("[15-21)","[21-22)","[22-36)","[36-41)","[41-46)","[46-76)")

chisq.test(ctbl)
barplot(ctbl, col = c("red","blue"), legend = T)
### Result: ratio of agegroups do not differ among men and women


## Correlation
cor(data$time, data$age.group2, use = "complete.obs", method = "kendall")

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
