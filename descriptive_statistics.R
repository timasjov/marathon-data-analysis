#Read in data
data <- read.table("data/processedData.txt", header=T)

# Statistical analysis

## t-test
t.test(data$time~data$gender)
### Result: finishing time is significantly different between genders
ggplot(data, aes(x = gender, y = time, fill = gender)) +
  geom_boxplot() +
  labs(title = "Time by gender", y = "time (sec)")

t.test(data$time~data$nationality)
### Result: finishing time is significantly different between Estonians and foreigners
ggplot(data, aes(x = nationality, y = time, fill = nationality)) +
  geom_boxplot() +
  labs(title = "Time by nationality", y = "time (sec)")

## ANOVA
summary(aov(data$time~data$age.group2))
### Result: finishing time is significantly different between age groups
ggplot(data, aes(x = factor(age.group2), y = time, fill = gender)) +
  geom_boxplot() +
  labs(title = "Time by age group", x = "age group", y = "time (sec)")

summary(aov(data$time~data$country))
### Result: finishing time is significantly different between countries
ggplot(data, aes(x = factor(country), y = time, fill = country)) +
  geom_boxplot() +
  labs(title = "Time by countries", x = "countries", y = "time (sec)") +
  theme(axis.text.x = element_text(angle = 90))

## Chi-square test
tbl <- table(data$gender, data$age.group2)
ctbl <- cbind(tbl[,"17"]+tbl[,"20"],
                tbl[,"21"],
                tbl[,"35"],
                tbl[,"40"],
                tbl[,"45"],
                tbl[,"50"]+tbl[,"55"]+tbl[,"60"]+tbl[,"65"]+tbl[,"70"]+tbl[,"75"])
colnames(ctbl) = c("[15-21)","[21-22)","[22-36)","[36-41)","[41-46)","[46-76)")

chisq.test(ctbl)
### Result: no significant difference in frequency distribution of age groups between genders
ggplot(data, aes(x = factor(age.group2), fill = gender)) +
  geom_bar(position = "dodge") +
  labs(title = "Cyclists per age group", x = "age group")

barplot(ctbl, col = c("red","blue"), legend = T)

## Correlation
cor(data$time, data$age.group2, use = "complete.obs", method = "kendall")

#alternative
splits <- read.table("data/splits.txt", header=T)

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
axis(1, at=0:7, labels= colnames(splits))

#By gender
plot(xValues, menSpeeds, type = "l", col="red", xlab="Splits", ylab = "Speed (km/h)", 
     xaxt="n", main="Average speeds between splits by gender", ylim=c(18,28))
lines(xValues, womenSpeeds, col="blue")
axis(1, at=0:7, labels= colnames(splits))
legend("topleft", legend = c("Men","Women"), lty=c(1,1), lwd=c(2.5,2.5), col=c("red","blue"), cex=.7)


#Average finish times per age group
meanClass = function(data, class){
  if(class=="Total"){
    res = mean(data[, "time"], na.rm=T)
  }else{
    res = mean(data[data[,"age.group"]==class, "time"], na.rm=T)
  }
  hours = floor(res / 3600)
  minutes = floor((res - (3600*hours))/60)
  return(paste(hours,":",minutes,sep=""))
}

tab = table(data$age.group)
x = c(paste(names(tab),"(" ,tab, ")",sep=""), paste("Total(",sum(tab),")", sep=""))
y = as.POSIXct(sapply(c(names(tab),"Total"), FUN = function(x){meanClass(data, x)}), format="%H:%M")
xy=data.frame(x, y)


ggplot(xy, aes(x=xy$x, y=xy$y, width=0.5)) +
  geom_bar(stat="identity", 
           fill=c(rep("deepskyblue",length(xy$y)-1), "chartreuse"))+
  geom_text(aes(label=substr(xy$y,13,16)), vjust=-1, size=4) +
  xlab("Age groups with counts") + ylab("Time") +
  ggtitle("Average finish times per age group")+ 
  theme_bw()+
  theme(panel.grid.major.x=element_blank(),
        plot.title = element_text(lineheight=.8, face="bold", vjust=1),
        axis.text.x=element_text(angle=45, vjust = 0.7))+
  scale_y_datetime(limits=c(as.POSIXct('0:00',format="%H:%M"),
                            as.POSIXct('6:00',format="%H:%M")))+
  geom_hline(aes(yintercept = as.numeric(y[length(y)])), colour = "chartreuse",size=0.8)
