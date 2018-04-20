# classify each two classes with knn
# no PCA: percentage of training set[0.1, 0.1, 0.9] and value of k[1, 1, 5]
# do PCA: percentage of training set[0.1, 0.1, 0.9], value of k[1, 1, 5] and number of PCA[2, 1, 21]

classify.two.knn <- function(classTable) {
	library(class)
	
	classList <- c("bt", "dns", "ebuddy", "edonkey", "ftp", "http", "imap", "msn", "pop3", "rsp", "rtsp", "smb", "smtp", "ssh", "ssl2", "ssl3", "xmpp", "yahoomsg")
	columnList <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9", "V10", "V11", "V12", "V13", "V14", "V15", "V16", "V17", "V18", "V19", "V20", "V21", "class")
	classInd <- 18
	endL <- classInd - 1
	
	# create results' fold if not exist (delete all folders otherwise store all)
	path <- "./classify_two_knn"
	subPath_1 <- "./classify_two_knn/classify_two_knn_noPCA"
	subPath_2 <- "./classify_two_knn/classify_two_knn_doPCA"
	if (!file.exists(path)) {
		dir.create(path)
		dir.create(subPath_1)
		dir.create(subPath_2)
	}
	
	### no PCA: change percentage and value of k
	# build a dataframe(resultSum) to store accuracy of all percentages for all two classes
	resultSum <- data.frame(Classes_K=numeric(0), Accuracy_0.1=numeric(0), Accuracy_0.2=numeric(0), Accuracy_0.3=numeric(0), Accuracy_0.4=numeric(0), Accuracy_0.5=numeric(0), Accuracy_0.6=numeric(0), Accuracy_0.7=numeric(0), Accuracy_0.8=numeric(0), Accuracy_0.9=numeric(0))
	classesNum <- 0
	
	for (i in 1:endL) {
		cleanMem <- function(n=10) { for (i in 1:n) gc() }
		
		for (j in (i+1):classInd) {
			cleanMem <- function(n=10) { for (i in 1:n) gc() }
			
			# combind two classes
			c1 <- classTable[[i]]
			c2 <- classTable[[j]]
			c1.c2 <- rbind(c1, c2)
			names(c1.c2) <- columnList
			
			# for each classes, change vk
			for (vk in 1:5) {
				cleanMem <- function(n=10) { for (i in 1:n) gc() }
				
				# build records in resultSum (thisSum) to store accuracy of all percentages for this two classes
				c1_c2 <- paste(classList[i], "_", classList[j], "_k", vk, sep="")
				classesNum <- classesNum+1
				resultSum[classesNum, 1] <- c1_c2
				perNum <- 1
			
				# for each two classes, change per for training set
				for (per10 in 1:9) {
					cleanMem <- function(n=10) { for (i in 1:n) gc() }
					
					per <- per10/10
					
					# per for training set; (1-per) for testing set
					# change seed each time to ensure differences
					set.seed(as.numeric(Sys.time()))
					ind <- sample(2, nrow(c1.c2), replace=TRUE, prob=c(per, (1-per)))
					c1c2.train <- c1.c2[ind==1, 1:21]
					c1c2.test <- c1.c2[ind==2, 1:21]
					c1c2.trainLabel <- c1.c2[ind==1, 22]
					c1c2.testLabel <- c1.c2[ind==2, 22]
					
					# knn and its table
					c1c2.p.knn <- knn(train=c1c2.train, test=c1c2.test, cl=c1c2.trainLabel, k=vk)
					c1c2.p.knn.table <- table(x=c1c2.p.knn, y=c1c2.testLabel)
							
					# calculate its performance result
					accuracy <- (c1c2.p.knn.table[1,1] + c1c2.p.knn.table[2,2])/nrow(c1c2.test)
					
					# store their performance results
					newFileAdd <- paste("./classify_two_knn/classify_two_knn_noPCA/", classList[i], "_", classList[j], "_noPCA_knn_k", vk, "_", per, ".csv", sep="")
					accTable <- rbind(c1c2.p.knn.table, c("Accuracy:", accuracy))
					write.csv(accTable, file=newFileAdd)
					
					# form a record(thisSum) with accuracy of all percentages for this two
					perNum <- perNum+1
					resultSum[classesNum, perNum] <- accuracy
				} # END percentage
			} # END vk			
		} # END j
	} # END i
	
	# store final resultSum into a file
	write.csv(resultSum, file="./classify_two_knn/classify_two_knn_noPCA/sum_noPCA_knn.csv")
	
	### END no PCA
	
	### do PCA: change percentage, value of k and number of PCA
	for (i in 1:endL) {
		cleanMem <- function(n=10) { for (i in 1:n) gc() }
		
		for (j in (i+1):classInd) {
			cleanMem <- function(n=10) { for (i in 1:n) gc() }
			
			# create folders for each classes
			#if (file.exists(subPath_2)) {
				class_path <- paste(subPath_2, "/", classList[i], "_", classList[j], "_knn", sep="")
				dir.create(class_path)
			#}
			
			# combind two classes
			c1 <- classTable[[i]]
			c2 <- classTable[[j]]
			c1.c2 <- rbind(c1, c2)
			names(c1.c2) <- columnList
		
			# PCA entire data sets
			pca.c1c2 <- prcomp(c1.c2[, 1:21], scale=TRUE)
			
			# store summary of entire pca into a csv file
			newSummaryAdd <- paste(class_path, "/", classList[i], "_", classList[j], "_pca_summary.csv", sep="")
			SumT <- summary(pca.c1c2)
			write.csv(SumT$importance, file=newSummaryAdd)
			
			# build a dataframe(resultSum) to store accuracy of all per, vk and np for each two classes
			resultSum <- data.frame(Classes_PCA_K=numeric(0), Accuracy_0.1=numeric(0), Accuracy_0.2=numeric(0), Accuracy_0.3=numeric(0), Accuracy_0.4=numeric(0), Accuracy_0.5=numeric(0), Accuracy_0.6=numeric(0), Accuracy_0.7=numeric(0), Accuracy_0.8=numeric(0), Accuracy_0.9=numeric(0), Cumulative_proportion=numeric(0))
			xNum <- 0
			
			# for each two classes, change value of k
			for (vk in 1:5) {
				cleanMem <- function(n=10) { for (i in 1:n) gc() }
				
				# for each two classes, change number of PCA
				for (np in 2:21) {
					cleanMem <- function(n=10) { for (i in 1:n) gc() }
					
					# extract np PCs
					p.c1c2 <- pca.c1c2$x[, 1:np]
					
					# build records with accuracy of all per for this two in this np
					c1_c2 <- paste(classList[i], "_", classList[j], "_k", vk, "_PCA", np, sep="")
					xNum <- xNum+1
					resultSum[xNum, 1] <- c1_c2
					perNum <- 1
				
					# for each two classes, change percentage
					for (per10 in 1:9) {
						cleanMem <- function(n=10) { for (i in 1:n) gc() }
						
						per <- per10/10
						
						# per for training set; (1-per) for testing set
						# change seed each time to ensure differences
						set.seed(as.numeric(Sys.time()))
						ind <- sample(2, nrow(p.c1c2), replace=TRUE, prob=c(per, (1-per)))
						c1c2.train <- p.c1c2[ind==1, ]
						c1c2.test <- p.c1c2[ind==2, ]
						c1c2.trainLabel <- c1.c2[ind==1, 22]
						c1c2.testLabel <- c1.c2[ind==2, 22]
						
						# knn and its table
						c1c2.p.knn <- knn(train=c1c2.train, test=c1c2.test, cl=c1c2.trainLabel, k=vk)
						c1c2.p.knn.table <- table(x=c1c2.p.knn, y=c1c2.testLabel)
							
						# calculate its performance result
						accuracy <- (c1c2.p.knn.table[1,1] + c1c2.p.knn.table[2,2])/nrow(c1c2.test)
						
						# store their performance results
						newFileAdd <- paste(class_path, "/", classList[i], "_", classList[j], "_PCA_knn_k", vk, "_p",  np, "_", per, ".csv", sep="")
						accTable <- rbind(c1c2.p.knn.table, c("Accuracy:", accuracy))
						write.csv(accTable, file=newFileAdd)
						
						# build a dataframe(resultSum) with accuracy all
						perNum <- perNum+1
						resultSum[xNum, perNum] <- accuracy
					} # END percentage
					
					# ERROR HANDLE & store each cumulative proportion for different number of PCA in each two classes
					try(if((perNum+1)!= 11) stop(paste("error loop in percentage of ", c1_c2, sep="")))
					resultSum[xNum, 11] <- SumT$importance[3, np]
				} # END np
			} # END vk
			
			# store final resultSum into files for each classes
			classFile <- paste(class_path, "/sum_doPCA_knn_", classList[i], "_", classList[j], ".csv", sep="")
			write.csv(resultSum, file=classFile)
			rm(resultSum)
			rm(xNum)
			rm(perNum)
		} # END j
	} # END i
	### END do PCA
}