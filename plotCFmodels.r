#######################################################################
# plotCFmodels
#	plots accuracy and RMSE scores with standard errors for 
#	models fit using cfirt when parameters are stored in an input file
#	
#		Yoav Bergner, 2012
#######################################################################


plotCFmodels <- function(inputfile,outputfile,numTrials) {

CFIRT <- read.table(inputfile, header=T, row.names=1)

pdf(outputfile,width = 6.0, height = 6.0)

par(mar=c(5,4,4,4) + 0.1)

xticks = 1:length(rownames(CFIRT))
modellabels = rownames(CFIRT)

j = 1:nrow(CFIRT)

ylim=c(0.99*range(CFIRT[,"ACC.ts"])[1],1.01*range(CFIRT[,"ACC.ts"])[2])
plot(j,CFIRT[,"ACC.ts"], type="n",
main="CF model performance",
xaxt="n", xlab="",
ylab="Accuracy", 
ylim=ylim
)
points(j,CFIRT[,"ACC.ts"], type="b", col="black", lwd=2)
error.bar(j,CFIRT[,"ACC.ts"],CFIRT[,"ACC.sd"]/sqrt(numTrials),col="black")

par(new=T)
plot(j,CFIRT[,"RMSE.ts"], type="n", axes=F, xaxt="n", xlab='', ylab = '', ylim=c(0.99*range(CFIRT[,"RMSE.ts"])[1],1.01*range(CFIRT[,"RMSE.ts"])[2]))
points(j,CFIRT[,"RMSE.ts"], type="b", col="blue", lwd=2)
error.bar(j,CFIRT[,"RMSE.ts"],CFIRT[,"RMSE.sd"]/sqrt(numTrials),col="blue")

axis(1, at=xticks,labels=modellabels, cex.axis=1, las=2)
axis(4, pretty(c(0.99*range(CFIRT[,"RMSE.ts"])[1],1.01*range(CFIRT[,"RMSE.ts"])[2])))

mtext("RMSE", col="blue", side=4, line=3)
mtext("CF Model (by parameter dimensionality)", side=1, line=4, adj=0.5, cex=1)


dev.off()

}
