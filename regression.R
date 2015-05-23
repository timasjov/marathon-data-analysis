#Read in data
data = read.table("data/processedData.txt", header=T)

#Fit model using using linear model
lmfit <- lm(timeCategory ~ ageCategory + countryCategory + sNrCategory + participTimeCategory, data=data)
form <- as.matrix(coef(lmfit))
rownames(form) <- gsub("try", "try == ", rownames(form) )
rownames(form) <- gsub("oup", "oup == ", rownames(form) )
rownames(form)[1] <- "Base"
cat(paste( form, paste("(", rownames(form), ")" ), sep="*", collapse="+\n") )

#Decision tree
fit <- rpart(placeCategory ~ ageCategory + nationality + participTimeCategory, method="class", minbucket = 75, minsplit = 75, cp=-0.5, data=data)
colors <- c("pink", "palegreen3", "yellow", "lightblue", "blue", "violet", "orange", "cyan", "bisque", "lightCoral")
boxcols <- (colors)[fit$frame$yval]
prp(fit, type=3, extra=100, faclen = 0, cex = 0.75, box.col = boxcols)
legend("bottomright", xpd = TRUE, inset = c(0, 0), cex = 0.7, ncol=3, fill = colors, title="Final place",
       legend = c("1-306", 
                  "307-612", 
                  "613-918", 
                  "919-1223", 
                  "1224-1529", 
                  "1530-1835", 
                  "1836-2140",
                  "2141-2446",
                  "2447-2752",
                  "2753-3057"
                  ))