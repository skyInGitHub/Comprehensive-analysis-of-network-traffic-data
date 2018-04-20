# classify all without PCA using C5.0

classify.all.C50 <- function(allData, classList, per) {
	library(C50)
	
	names(allData) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "class")
	
	# per for training set; (1-per) for testing set
	# change seed each time to ensure differences
	set.seed(13)
	ind <- sample(2, nrow(allData), replace=TRUE, prob=c(per, (1-per)))
	all.train <- allData[ind==1, 1:21]
	all.test <- allData[ind==2, 1:21]
	all.trainLabel <- allData[ind==1, 22]
	all.testLabel <- allData[ind==2, 22]
	all.testClass <- allData[ind==2, ]
	
	# C5.0 and its table
	all.c50 <- C5.0(all.train, all.trainLabel)
	all.pred <- predict(all.c50, all.test)
	all.c50.table <- table(x=all.pred, y=all.testLabel)
	
	# calculate its performance result
	correct <- 0
	accEach <- c()
	for (i in 1:18) {
		correct <- correct + all.c50.table[i, i]
		accEach[i] <- all.c50.table[i, i]/nrow(subset(all.testClass, class==classList[i]))
	}
	accuracy <- correct/nrow(all.test)
#	accuracySum <- c("AccuracyAll:", accuracy)
	accuracySum <- rbind(accEach, c("AccuracyAll:", accuracy))
	
	# store performance table and accuracy
	accTable <- rbind(all.c50.table, accuracySum)
	accFile <- paste("./performance_summary/accuracy_all_C50_", per, ".csv", sep="")
	write.csv(accTable, file=accFile)
#	write.csv(all.c50.table, file=accFile)
}