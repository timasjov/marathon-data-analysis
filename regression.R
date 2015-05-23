#Read in data
data = read.table("data/processedData.txt", header=T)

#Split final times into 10 groups
data$timeCategory <- ntile(data$time, 10)

#Split start numbers into 10 groups
data$sNrCategory <- ntile(data$s.nr, 10)

#Split number of participations into 10 groups
data$participTimeCategory <- ntile(data$particip.time, 10)

#Split final place into 10 groups
data$placeCategory <- ntile(data$place, 10)

#Combine all Estonian participants
data$countryCategory <- data$country
levels(data$countryCategory) <- c(levels(data$countryCategory), "Eesti")
data[data$country %in% c(
  "Harju", "Hiiumaa", "Ida-Viru", "Jõgeva", "Järvamaa", "Lääne-Viru", "Läänemaa", 
  "Pärnu", "Rapla", "Saaremaa", "Tallinn", "Tartu", "Valga", "Viljandi"), "countryCategory"] <- "Eesti"

#Combine age categories
data$ageCategory <- data$age.group2
levels(data$ageCategory) <- c(levels(data$ageCategory), c("17-21", "35-45", "50-60", "65+"))
data[data$age.group2 %in% c("17", "20", "21"), "ageCategory"] <- "17-21"
data[data$age.group2 %in% c("35", "40", "45"), "ageCategory"] <- "35-45"
data[data$age.group2 %in% c("50", "55", "60"), "ageCategory"] <- "50-60"
data[data$age.group2 %in% c("65", "70", "75"), "ageCategory"] <- "65+"


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

