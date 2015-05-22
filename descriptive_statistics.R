#Read in data
data <- read.table("data/processedData.txt", header=T)

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

#alternative
splits = read.table("data/splits.txt", header=T)

#Find speed based on given subset of data
findSpeeds = function(x, splits){
  speed = mean(splits[,1]  / (x[,1]/3600))
  speeds = c(speed, speed)
  for(i in 2:length(splits)){
    speed = mean(splits[,i] / ((x[,i] - x[,i-1])/3600))
    speeds = c(speeds, speed, speed)
  }
  return(speeds)
}

#Calculate speeds for each subset
overallSpeeds = findSpeeds(data[,c(paste("split.",1:6,sep=""),"time")], splits[-1])
menSpeeds = findSpeeds(data[data[,"gender"]=="male",c(paste("split.",1:6,sep=""),"time")], splits[-1])
womenSpeeds = findSpeeds(data[data[,"gender"]=="female",c(paste("split.",1:6,sep=""),"time")], splits[-1])

#Draw plots
xValues = c(0,sort(rep(1:6, 2)),7)
plot(xValues, overallSpeeds, type = "l", col="red", xlab="Splits", ylab = "Speed (km/h)", 
     xaxt="n", main="Average speeds between splits")
axis(1, at=0:7, labels= c("Start", 1:6,"Finish"))

#By gender
plot(xValues, menSpeeds, type = "l", col="red", xlab="Splits", ylab = "Speed (km/h)", 
     xaxt="n", main="Average speeds between splits by gender", ylim=c(18,28))
lines(xValues, womenSpeeds, col="blue")
axis(1, at=0:7, labels= c("Start", 1:6,"Finish"))
legend("topleft", legend = c("Men","Women"), lty=c(1,1), lwd=c(2.5,2.5), col=c("red","blue"), cex=.7)
