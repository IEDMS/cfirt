#######################################################################
# example file 1 - Model Comparison
#
#	This example file loads a dichotomous response matrix
#	and runs the collaborative filter on a succession of models
#	specified in the list "models"
#	using the regularization parameter specified in fixlambda
#	(ideally, you determine lambda via cross-validation first)
#
#	provided response matrix must either have NAs or -9 for
#	omitted responses, or edit this in the read-in
#
# other parameters
#	models: specify list of models (see example)
#	mask_fraction: how much of response matrix to hold out from training
#	numTries: how many times to random subsample with each model
#	
#		Yoav Bergner, 2012
#######################################################################

source("cfirt.r")

### filenames
datafile = 'cfirtdata.txt'
modelcompfile = 'modelcompare.txt'
pdffile = 'modelcompare.pdf'

### free parameters
mask_fraction = 0.3	# fraction of response matrix to hold out from training
numTries <- 5	# number of randomized subsamples to take for each model test

### model spec, see documentation for details
models = "ALL"
models = list(list(dimTheta=c(1,1),dimX=c(1,1)),
			list(dimTheta=c(1,1),dimX=c(2,0)),
			list(dimTheta=c(2,1),dimX=c(2,1)),
			list(dimTheta=c(2,1),dimX=c(3,0)),
			list(dimTheta=c(3,1),dimX=c(3,1)),
			list(dimTheta=c(3,1),dimX=c(4,0)),
			list(dimTheta=c(4,1),dimX=c(4,1)),
			list(dimTheta=c(4,1),dimX=c(5,0)))
# 			list(dimTheta=c(5,1),dimX=c(5,1)),
# 			list(dimTheta=c(5,1),dimX=c(6,0)))

### values or regularization parameters
fixlambda = c(1,3,5,6,9,11,13,13,15,15,15,17)	## regularization hyperparameter. Higher dimensional models usually need larger lambda.

#######################################################################
### READ in data and determine matrix R indicating non-omitted
#######################################################################

U <- as.matrix(read.table(datafile, row.names=1,header=TRUE)) ## use this if header row can be parsed by R
U[is.na(U)] <- -9	## replace NAs by numeric value (-9) so that when multiplied by 0 get 0 not NA
numStudents = nrow(U)
numItems = ncol(U)
R <- (U != -9) + 0 # R matrix hold 1 for scored responses 0 if omitted

#######################################################################
### BEGIN scanning model space
#######################################################################


cat('scanning model space on',datafile,'with output to',modelcompfile,'\n')
cat('model\t lambda\t ACC.tr\t ACC.ts\t ACC.sd\t RMSE.tr\t RMSE.ts\t RMSE.sd\n', file=modelcompfile)

for (i in 1:length(models)) {
	trainACC = rep(0,numTries)
	testACC = rep(0,numTries)
	trainRMSE = rep(0,numTries)
	testRMSE = rep(0,numTries)
	dimX <- models[[i]]$dimX
	dimTheta <- models[[i]]$dimTheta
	lambda <- fixlambda[i]	# should really determine this using cross-validation and then fix later
	cat('model',paste(paste(dimTheta[1], dimTheta[2], dimX[1], dimX[2], sep="")))
	for (n in 1:numTries) {
		
		Rmask = (matrix(runif(numStudents*numItems),numStudents) > mask_fraction)
		Rtrain = (R*Rmask == 1);
		Rtest = (R*(1-Rmask) ==1);

		### initialize guess using random normal distributed values for all parameters
		X.start = rnorm(numItems*dimX[1])
		Theta.start = rnorm(numStudents*dimTheta[1])	
		initial_parameters = c(X.start,Theta.start)
		
		### this is where the regularize cost function (-loglikelihood) is minimized
		fn <- function(param.vec) cfirtCost(param.vec, U, Rtrain, dimX, dimTheta, lambda)
		gr <- function(param.vec) cfirtGrad(param.vec, U, Rtrain, dimX, dimTheta, lambda)
		
			### L-BFGS-B is much faster, when it doesn't throw errors, BFGS takes 5-10 times as long but usually works, so
			res <- optim(initial_parameters, fn, gr, method="BFGS", control=list(maxit=500))
			
		if (res$convergence) {
			cat("There were convergence issues (",res$convergence,")...\n")
			}
	
		### just reshaping the vector back into parameter matrices.
		param.matrices <- reshape.params(res$par, numStudents, numItems, dimX, dimTheta)
		X <- param.matrices$X
		Theta <- param.matrices$Theta
		
		expectP <- sigmoid(Theta %*% t(X))
		predictP <- (expectP > 0.5) + 0
		
		trainACC[n] = sum(predictP[Rtrain] == U[Rtrain])/sum(Rtrain)
		testACC[n] = sum(predictP[Rtest] == U[Rtest])/sum(Rtest)

		trainRMSE[n] = sqrt(sum((U[Rtrain]-expectP[Rtrain])^2)/sum(Rtrain))
		testRMSE[n] = sqrt(sum((U[Rtest]-expectP[Rtest])^2)/sum(Rtest))
		cat('.')
	}
	cat('\n')
	cat(paste(paste(dimTheta[1], dimTheta[2], dimX[1], dimX[2], sep=""),
		lambda,
		round(mean(trainACC, na.rm=T), dig=5),
		round(mean(testACC, na.rm=T), dig=5),
		round(sd(testACC, na.rm=T), dig=5),
		round(mean(trainRMSE, na.rm=T), dig=5),
		round(mean(testRMSE, na.rm=T), dig=5),
		round(sd(testRMSE, na.rm=T), dig=5), '\n', sep="\t"), file=modelcompfile, append=T)
}

### plot results in a nice figure
plotCFmodels(modelcompfile,pdffile,numTries)

