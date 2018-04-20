# classify all classes

classify.all.C50.PCA <- function(allData, classList, np, per) {
	library(C50)
	
	names(allData) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "class")
	
	# PCA entire data set
	pca.all <- prcomp(allData[, 1:21], scale=TRUE)
	
	# extract np PCs (i.e. 3 PCs)
	p.all <- pca.all$x[, 1:np]
	
	# per for training set; (1-per) for testing set
	# change seed each time to ensure differences
	set.seed(12)
	ind <- sample(2, nrow(p.all), replace=TRUE, prob=c(per, (1-per)))
	all.train <- p.all[ind==1, ]
	all.test <- p.all[ind==2, ]
	all.trainLabel <- allData[ind==1, 22]
	all.testLabel <- allData[ind==2, 22]
	all.testClass <- allData[ind==2, ]
	
	# C5.0 and its table
	all.p.c50 <- C5.0(all.train, all.trainLabel)
	all.p.pred <- predict(all.p.c50, all.test)
	all.p.c50.table <- table(x=all.p.pred, y=all.testLabel)
	
	# calculate its performance result
	correct <- 0
	accEach <- c()
	for (i in 1:18) {
		correct <- correct + all.p.c50.table[i, i]
		accEach[i] <- all.p.c50.table[i, i]/nrow(subset(all.testClass, class==classList[i]))
	}
	accuracy <- correct/nrow(all.test)
#	accuracySum <- c("AccuracyAll:", accuracy)
	accuracySum <- rbind(accEach, c("AccuracyAll:", accuracy))
	
	# store summary of entire pca
	SumT <- summary(pca.all)
	write.csv(SumT$importance, file="./performance_summary/sum_pca_all.csv")
	
	# store performance table and accuracy
	accTable <- rbind(all.p.c50.table, accuracySum)
	accFile <- paste("./performance_summary/accuracy_p", np, "_C50_", per, ".csv", sep="")
	write.csv(accTable, file=accFile)
}