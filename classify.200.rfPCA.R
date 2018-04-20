classify.200.rfPCA <- function(np, per) {
	two.class <- read.csv("./two classes/http_smtp_noPCA_200.csv", header=TRUE)
	two.class <- two.class[, -1]

	library('randomForest')

	classList <- c("http", "smtp")
	names(two.class) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "class")

	pca.all <- prcomp(two.class[, 1:21], scale=TRUE)
	p.all <- pca.all$x[, 1:np]

	set.seed(13)
	ind <- sample(2, nrow(two.class), replace=TRUE, prob=c(per, (1-per)))
	all.train <- p.all[ind==1, ]
	all.test <- p.all[ind==2, ]
	all.trainLabel <- two.class[ind==1, 22]
	all.testLabel <- two.class[ind==2, 22]
	all.testClass <- two.class[ind==2, ]
	
	# RF and its table
	all.rf <- randomForest(all.train, all.trainLabel)
	all.pred <- predict(all.rf, all.test)
	all.rf.table <- table(x=all.pred, y=all.testLabel)
	
	# calculate its performance result
	correct <- 0
	accEach <- c()
	for (i in 1:2) {
		correct <- correct + all.rf.table[i, i]
		accEach[i] <- all.rf.table[i, i]/nrow(subset(all.testClass, class==classList[i]))
	}
	accuracy <- correct/nrow(all.test)
#	accuracySum <- c("AccuracyAll:", accuracy)
	accuracySum <- rbind(accEach, c("AccuracyAll:", accuracy))
	
	# store performance table and accuracy
	accTable <- rbind(all.rf.table, accuracySum)
	accFile <- paste("./performance_summary/accuracy_RF_http_smtp_PC", np,"_per", per,"_200.csv", sep="")
	write.csv(accTable, file=accFile)
}