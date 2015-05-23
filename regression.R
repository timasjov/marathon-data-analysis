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
fit <- rpart(timeCategory ~ ageCategory + nationality + participTimeCategory, method="class", minbucket = 75, minsplit = 75, cp=-0.5, data=data)
colors <- c("pink", "palegreen3", "yellow", "LightSkyBlue", "blue", "orange", "orange", "cyan", "bisque", "OrangeRed ")
boxcols <- (colors)[fit$frame$yval]
prp(fit, type=3, extra=100, faclen = 0, cex = 0.75, box.col = boxcols)
legend("bottomright", xpd = TRUE, inset = c(0, 0), cex = 0.7, ncol=3, fill = colors, title="Final place",
       legend = c("2:29-2:59", 
                  "3:00-3:11", 
                  "3:12-3:21", 
                  "3:22-3:31", 
                  "3:32-3:42", 
                  "3:43-3:53", 
                  "3:54-4:04",
                  "4:05-4:19",
                  "4:20-4:39",
                  "4:40-6:26"
                  ))