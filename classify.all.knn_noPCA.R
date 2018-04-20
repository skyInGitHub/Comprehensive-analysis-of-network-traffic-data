# classify all without PCA using KNN

classify.all.knn <- function(allData, classList, vk, per) {
	library(class)
	
	names(allData) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "class")
	
	# per for training set; (1-per) for testing set
	# change seed each time to ensure differences
	set.seed(3)
	ind <- sample(2, nrow(allData), replace=TRUE, prob=c(per, (1-per)))
	all.train <- allData[ind==1, 1:21]
	all.test <- allData[ind==2, 1:21]
	all.trainLabel <- allData[ind==1, 22]
	all.testLabel <- allData[ind==2, 22]
	all.testClass <- allData[ind==2, ]
	
	# knn and its table
	all.knn <- knn(train=all.train, test=all.test, cl=all.trainLabel, k=vk)
	all.knn.table <- table(x=all.knn, y=all.testLabel)
	
	# calculate its performance result
	correct <- 0
	accEach <- c()
	for (i in 1:18) {
		correct <- correct + all.knn.table[i, i]
		accEach[i] <- all.knn.table[i, i]/nrow(subset(all.testClass, class==classList[i]))
	}
	accuracy <- correct/nrow(all.test)
#	accuracySum <- c("AccuracyAll:", accuracy)
	accuracySum <- rbind(accEach, c("AccuracyAll:", accuracy))
	
	# store summary of entire pca
#	SumT <- summary(allData)
#	write.csv(SumT, file="./performance_summary/sum_all.csv")
	
	# store performance table and accuracy
	accTable <- rbind(all.knn.table, accuracySum)
	accFile <- paste("./performance_summary/accuracy_all_knn", vk, "_", per, ".csv", sep="")
	write.csv(accTable, file=accFile)
#	write.csv(all.knn.table, file=accFile)
}