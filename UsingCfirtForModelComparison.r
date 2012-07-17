source("cfirt.r")

datafile = 'cfirtdata.txt'
ICCfile = 'testingcfirt'

U <- as.matrix(read.table(datafile, row.names=1,header=TRUE)) ## use this if header row can be parsed by R

# open course, do IRT, get item params
U[is.na(U)] <- -9	## replace NAs by numeric value (-9) so that when multiplied by 0 get 0 not NA
numStudents = nrow(U)
numItems = ncol(U)
R <- (U != -9) + 0 # R matrix hold 1 for scored responses 0 if omitted

### model spec, see documentation for details
### this example is unidimensional 2PL
model = "5d2PL"
dimX <- c(6,0)
dimTheta <- c(5,1)
lambda = 1	## regularization hyperparameter. Higher dimensional models usually need larger lambda.

### initialize guess using random normal distributed values for all parameters
X.start = rnorm(numItems*dimX[1])
Theta.start = rnorm(numStudents*dimTheta[1])	
initial_parameters = c(X.start,Theta.start)

### this is where the regularize cost function (-loglikelihood) is minimized
fn <- function(param.vec) cfirtCost(param.vec, U, R, dimX, dimTheta, lambda)
gr <- function(param.vec) cfirtGrad(param.vec, U, R, dimX, dimTheta, lambda)

	### L-BFGS-B is much faster, when it doesn't throw errors, BFGS takes 5-10 times as long but usually works, so
			
res <-  tryCatch(optim(initial_parameters, fr, gr, method="L-BFGS-B", control=list(trace=1,maxit=500)), 
			error=function(e){
				cat("** L-BFGS-B method failed. Trying BFGS...slow... \n")
				optim(initial_parameters, fr, gr, method="BFGS", control=list(trace=1,maxit=500))})
			
if (res$convergence) cat("There were convergence issues. \n")

### just reshaping the vector back into parameter matrices.
param.matrices <- reshape.params(res$par, numStudents, numItems, dimX, dimTheta)
X <- param.matrices$X
Theta <- param.matrices$Theta

### errors are estimated using Fisher information
ddlogL <- cfirtDD(res$par, U, R, dimX, dimTheta) # for errors
error.est <- reshape.params(1/sqrt(ddlogL), numStudents, numItems, dimX, dimTheta)
X_se = error.est$X
Theta_se = error.est$Theta

### for unidimensional 2PL, rescaling (make skills unit normal) and converting to diff/discrim style

if (model == "2PL") {
	##  without errors
	rescaled <- RescaleSI(Theta[,2],X[,1],X[,2],noerrs=TRUE)
	resX <- cbind(rescaled$X1, rescaled$X2)
	resTheta <- rescaled$Theta
	
	ddstyle <- SI2DD(resX[,1],resX[,2],noerrs=TRUE)
	itemparams <- data.frame(diffs = ddstyle$diffs, discrims = ddstyle$discrims)
	
# 	## with errors, make each parameter a two-column matrix
# 	rescaled <- RescaleSI(cbind(Theta[,2],Theta_se),cbind(X[,1],X_se[,1]),cbind(X[,2],X_se[,2]))
# 	resX.w.err <- cbind(rescaled$X1, rescaled$X2)
# 	resTheta.w.err <- rescaled$Theta
# 	
# 	ddstyle <- SI2DD(cbind(resX.w.err[,1],resX.w.err[,2]),cbind(resX.w.err[,3],resX.w.err[,4]))
# 	itemparams.w.err <- data.frame(diffs = ddstyle$diffs[,1], diffs.se = ddstyle$diffs[,2], discrims = ddstyle$discrims[,1], discrims.se = ddstyle$discrims[,2])

	##	generate ICCs if so desired, see makeICCs.r for details
	U[U == -9] <- NA	# put NAs back in
	ICCitems <- 1:ncol(U)	# defaults to plotting ICCs for all items, otherwise change this

	## 	binning choice: I like to base the binning on dividing the [0,1] interval and using the logit transformation
	x = seq(0.04,0.96,by=0.04)
	bins = log(x/(1-x))
	chiSq <- makeICCs(U[,ICCitems], resTheta, ddstyle$diffs[ICCitems], ddstyle$discrims[ICCitems], prefix=ICCfile, bins=bins) 


}