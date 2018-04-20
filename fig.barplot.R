fig.barplot <- function() {
#########
	aveAccAll <- read.csv("./Results/accuracy_RF_noPC8.csv",header=TRUE)
#########

#	aveAccAll <- subset(aveAccAll[-2,])

	aveAccAll <- as.data.frame(aveAccAll)
	
	# if precentage is 0.99 -> 99%
	for (i in 2:ncol(aveAccAll)) {
		for (j in 1:nrow(aveAccAll)) {
			aveAccAll[j, i] <- aveAccAll[j, i]*100
		}
	}

	aveAccAll <- as.matrix(aveAccAll[,-1])
##############
	#rownames(aveAccAll) <- c("OverallAcc", "FM_HTTP", "FM_SMTP")
	rownames(aveAccAll) <- c("no PCA", "PC8")
	
	#to gain a higher resolution change the .png in fign and savePlot(...type=c(" ")..) to pdf
	fign="./Results/accuracy_RF_noPC8.png"
##############

	png(filename=fign, width=3500, height=3500, units="px", res=800)

	colors <- c("blue", "red")
############
	barplot(aveAccAll,
		main="Example with PC8 VS no PCA", 
		xlab="Classification Performance", 
		ylab = "Accuracy(%)",
		cex.axis = 0.8,
		cex.names = 0.65,
		cex.lab=1,
		col = colors,
		ylim = c(0, 119),
#		xlim = c(500, 1005),
		beside=TRUE
	)
##############
	legend("topright", 
		legend=rownames(aveAccAll),
		col = colors,
		cex=0.35, 
		yjust=120,
		bty="o", pch=22)

#	yaxis <- seq(0, 100, 20)
#	axis(2, at=yaxis, col.axis="black", las=0, cex.lab=0.8)

	abline(h=seq(0, 100, 10), lty=2, lwd=0.8, col="gray")

	dev.off()
}