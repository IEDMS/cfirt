#############################################################################
#	makeICCs - generates Item Characteristic Curves
#		inputs: U - the dichotomous response matrix, NAs are okay
#				Skills - a vector of abilities found by IRT
#				Diffs - a vector of item difficulties found by IRT
#				Discrims - a vector of discriminations found by IRT
#					will default to vector of ones if missing
#				bins - vector of bin breaks
#				prefix - pathname for file creation, .pdf is added automatically
#				histo - (optional) if set to true, plot histogram of relative bin sizes
#		outputs: prefix.pdf file with ICCs
#				function returns two vectors of chi-squared statistics as a list
#					first is based on actual bin binomial errors
#					the second is based on expected errors
#
#	Yoav Bergner, 2012
#
#############################################################################

makeICCs <- function(U,Skills,Diffs,Discrims=rep(1,length(Diffs)),bins=seq(-3,3,0.2),prefix="ICC",histo=FALSE) {

source("errorbars.R")

numStudents = length(Skills)
numItems = length(Diffs)
if (numItems  == 1) U <- as.matrix(U)	## avoids bug which occurs if you want to plot one item ICC

theta <- seq(-3,3,by=0.1)		
      
numBins = length(bins)-1
corfrac=array(0,dim=c(numBins,numItems))   ## frac who answered correctly
sigmacorfrac=array(0,dim=c(numBins,numItems))  ## Binomial error on fraction

binabil=array(NA,dim=numBins)
binwidth=array(NA,dim=numBins)
binn=array(NA,dim=c(numBins,numItems))
IRF2plCompare=array(0,dim=c(numBins,numItems))
reducedChiSquared = array(0,dim=numItems)	
altredChiSquared = array(0,dim=numItems)	
	
       ## For loop over questions
       for (i in 1:numItems){
        for (j in 1:numBins){
	      	binstudents=which(Skills >= bins[j] &  Skills < bins[j+1])
	      	binn[j,i]=length(binstudents)-length(which(is.na(U[binstudents,i])))        ## number of students in the bin
	      	binabil[j]=(bins[j]+bins[j+1])/2   ## middle of the bin for plotting
	      	binwidth[j]=(bins[j]-bins[j+1])  
 	    	numcor=sum(U[binstudents,i],na.rm=TRUE)
          	corfrac[j,i]   =numcor/binn[j,i]
         	sigmacorfrac[j,i]=sqrt(corfrac[j,i]*(1-corfrac[j,i])/binn[j,i])
           ## Binomial uncertainty is sqrt(pq/n), where n is the number of students in bin
           ## if corfrac = 1 or 0, we use a "standard dodge" below
            if ((binn[j,i] != 0) & (corfrac[j,i] == 0)) {
         		corfrac[j,i] = (1/3)/binn[j,i]
         		sigmacorfrac[j,i] = sqrt(corfrac[j,i]*(1-corfrac[j,i]))
         	} else if ((binn[j,i]!= 0) & (corfrac[j,i] == 1)) {
         		corfrac[j,i] = 1-(1/3)/binn[j,i]
         		sigmacorfrac[j,i] = sqrt(corfrac[j,i]*(1-corfrac[j,i]))
         	}  
       } 

 		## compute fit statistics
        for (j in 1:numBins) {
      	 	if ( (!is.na(corfrac[j,i])) & (sigmacorfrac[j,i] != 0)) {
      	 	Pji <- logistic2pl(binabil[j], Diffs[i],Discrims[i])
  	     		reducedChiSquared[i] = reducedChiSquared[i] + ((Pji-corfrac[j,i])/sigmacorfrac[j,i])^2	## actual bin-count errors
  	     		altredChiSquared[i] = altredChiSquared[i] + (1/(numBins-2))*(binn[j,i]*(Pji-corfrac[j,i])^2)/(Pji*(1-Pji)) ## expected errors
  	     	}
        }
      reducedChiSquared[i] = reducedChiSquared[i]/(length(which(!is.na(corfrac[,i])))-2)
    }


######################################################################
pdf(paste(prefix,".pdf",sep=""),width = 6.0, height = 6.0,
                onefile = TRUE, paper = "special")

   ### Loop through questions to add a plot figure
   for (i in 1:numItems){
		 	
	  plot(binabil,corfrac[,i],type="n",
		xlim=c(-3,3),
		ylim=c(0,1),
		xlab='Student Skill',
		ylab=paste('P(correct) N =',numStudents)
		)
	
		points(binabil,corfrac[,i],pch=1,col="black")	## bin points with error bars
		error.bar(binabil,corfrac[,i],sigmacorfrac[,i],col="black")

		lines(theta,logistic2pl(theta,Diffs[i],Discrims[i]),col="blue",lty=1)	### plot model curve

		if (histo) lines((binabil-binwidth/2),binn[,i]/max(binn),col="red", type="S") ### if you want a histogram of relative bin counts
	   		
		### labels labels labels	
		 text(0,1,colnames(U)[i],font=1,cex=1)
		
		 text(2,0.09,paste("d =",format(Diffs[i],digits=3)),font=1,cex=1,col="black")
		 text(2,0.05,paste("a =",format(Discrims[i],digits=3)),font=1,cex=1,col="black")
 		 text(1.2,0.01,paste("X2.1 =",format(reducedChiSquared[i],digits=2)),font=1,cex=1,col="black")
		 text(2.5,0.01,paste("X2.2 =",format(altredChiSquared[i],digits=2)),font=1,cex=1,col="black")
			
		 if (histo) text(-2,0.02,paste("maxbin =",format(max(binn),digits=3)),font=1,cex=1,col="red")
	
		}  ## end loop through questions
	dev.off()

	list(chi = reducedChiSquared, kie = altredChiSquared)
}
