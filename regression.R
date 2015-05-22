#Read in data
data = read.table("data/processedData.txt", header=T)

#Split final times into 10 groups
data$timeCategory <- ntile(data$time, 10)


#Fit model using ANOVA algorithm
fit <- rpart(timeCategory ~ particip.time + s.nr, data=data)
printcp(fit)
summary(fit)
rpart.plot(fit, type=3, extra=1)


#Fit model using using linear model
lmfit <- lm(time ~ country + age.group, data=data)
form <- as.matrix(coef(lmfit))
rownames(form) <- gsub("try", "try == ", rownames(form) )
rownames(form) <- gsub("oup", "oup == ", rownames(form) )
rownames(form)[1] <- "Base"
cat(paste( form, paste("(", rownames(form), ")" ), sep="*", collapse="+\n") )

