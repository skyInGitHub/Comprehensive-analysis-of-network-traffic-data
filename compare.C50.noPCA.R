# Compare two classes using C5.0
compare.C50.noPCA <- function (classTable, per) {
	library(C50)

	classList <- c("bt", "dns", "ebuddy", "edonkey", "ftp", "http", "imap", "msn", "pop3", "rsp", "rtsp", "smb", "smtp", "ssh", "ssl2", "ssl3", "xmpp", "yahoomsg")
	columnList <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "class")
	resultSum <- c("Classes", "Accuracy")
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
		
			# per for training set; (1-per) for testing set
			# change seed each time to ensure differences
			seed <- seed+1
			set.seed(seed)
			ind <- sample(2, nrow(c1.c2), replace=TRUE, prob=c(per, (1-per)))
			c1c2.train <- c1.c2[ind==1, 1:21]
			c1c2.test <- c1.c2[ind==2, 1:21]
			c1c2.trainLabel <- c1.c2[ind==1, 22]
			c1c2.testLabel <- c1.c2[ind==2, 22]
			
			# C5.0 and its table
			c1c2.p.c50 <- C5.0(c1c2.train, c1c2.trainLabel)
			c1c2.p.pred <- predict(c1c2.p.c50, c1c2.test)
			c1c2.p.c50.table <- table(x=c1c2.p.pred, y=c1c2.testLabel)
			
			# calculate its performance result
			accuracy <- (c1c2.p.c50.table[1,1] + c1c2.p.c50.table[2,2])/nrow(c1c2.test)
			
			# store their performance results
			newFileAdd <- paste("./performance_summary/", classList[i], "_", classList[j], "_noPCA_C50_", per, ".csv", sep="")
			#Acc <- paste("Accuracy: ", accuracy, sep="")
			accTable <- rbind(c1c2.p.c50.table, c("Accuracy:", accuracy))
			write.csv(accTable, file=newFileAdd)
			
			# build a dataframe(resultSum) to store accuracy and cumulative pro of all
			c1_c2 <- paste(classList[i], "_", classList[j], sep="")
			thisSum <- c(c1_c2, accuracy)
			resultSum <- rbind(resultSum, thisSum)
		}
	}
	
	# store final resultSum into a file
	sumFile <- paste("./performance_summary/", "sum_noPCA_C50_", per, ".csv", sep="")
	write.csv(resultSum, file=sumFile)
}