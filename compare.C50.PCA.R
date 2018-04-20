# two classes compare with C5.0

compare.C50.PCA <- function (classTable, np, per) {
	library(C50)

	classList <- c("bt", "dns", "ebuddy", "edonkey", "ftp", "http", "imap", "msn", "pop3", "rsp", "rtsp", "smb", "smtp", "ssh", "ssl2", "ssl3", "xmpp", "yahoomsg")
	columnList <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "class")
	resultSum <- c("Classes", "Accuracy", "Cumulative Proportion")
	classInd <- 18
	endL <- classInd - 1
	seed <- 1
	
	for (i in 1:endL)
	{
		# combind two classes
		for (j in (i+1):classInd)
		{
			c1 <- classTable[[i]]
			c2 <- classTable[[j]]
			c1.c2 <- rbind(c1, c2)
			names(c1.c2) <- columnList
		
			# PCA entire data sets
			pca.c1c2 <- prcomp(c1.c2[, 1:21], scale=TRUE)
		
			# extract np PCs
			p.c1c2 <- pca.c1c2$x[, 1:np]
		
			# per for training set; (1-per) for testing set
			# change seed each time to ensure differences
			set.seed(seed+1)
			ind <- sample(2, nrow(p.c1c2), replace=TRUE, prob=c(per, (1-per)))
			c1c2.train <- p.c1c2[ind==1, ]
			c1c2.test <- p.c1c2[ind==2, ]
			c1c2.trainLabel <- c1.c2[ind==1, 22]
			c1c2.testLabel <- c1.c2[ind==2, 22]
		
			# C5.0 and its table
			c1c2.p.c50 <- C5.0(c1c2.train, c1c2.trainLabel)
			c1c2.p.pred <- predict(c1c2.p.c50, c1c2.test)
			c1c2.p.c50.table <- table(x=c1c2.p.pred, y=c1c2.testLabel)
			
			# calculate its performance result
			accuracy <- (c1c2.p.c50.table[1,1] + c1c2.p.c50.table[2,2])/nrow(c1c2.test)
		 	
			# store summary of entire pca into a csv file
		#	newSummaryAdd <- paste("./performance_summary/", classList[i], "_", classList[j], "_pca_summary.csv", sep="")
			SumT <- summary(pca.c1c2)
		#	write.csv(SumT$importance, file=newSummaryAdd)
			
			# store their performance results
			newFileAdd <- paste("./performance_summary/", classList[i], "_", classList[j], "_p", np, "_C50_", per, ".csv", sep="")
			#Acc <- paste("Accuracy: ", accuracy, sep="")
			accTable <- rbind(c1c2.p.c50.table, c("Accuracy:", accuracy))
			write.csv(accTable, file=newFileAdd)
			
			# build a dataframe(resultSum) to store accuracy and cumulative pro of all
			c1_c2 <- paste(classList[i], "_", classList[j], sep="")
			thisSum <- c(c1_c2, accuracy, SumT$importance[3, np])
			resultSum <- rbind(resultSum, thisSum)
		}
	}
	
	# store final resultSum into a file
	sumFile <- paste("./performance_summary/", "sum_p", np, "_C50_", per, ".csv", sep="")
	write.csv(resultSum, file=sumFile)
}