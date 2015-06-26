#######################################################
#	basic sigmoid and logistic functions for IRT models
#	to use with cfirt code 
#		Yoav Bergner, 2015
#######################################################

sigmoid <- function(z) {
	1/(1+exp(-z))
}

logitRasch <- function(sk, d) {
	sigmoid(sk-d)
}

logistic2pl <- function(sk, d, a) {
	sigmoid(a*(sk-d))
	}

logistic2pl.vec <- function(sk, d, a) {
        ### uses vectors and returns matrix
	smat = matrix(rep(sk,length(d)),nrow=length(sk))
	dmat = matrix(rep(d,length(sk)),nrow=length(sk),byrow=T)
	amat = matrix(rep(a,length(sk)),nrow=length(sk),byrow=T)	
	sigmoid(amat * (smat - dmat))
}


# for simulating data with specified item structure
# and random item parameters
# by default, the data are multi-unidimensional
pseudoM2PLdataSI <- function(numStudents, numItems, mu, Sigma) {
    require(mnormt)
    dimK <- ncol(Sigma)
    theta <- rmnorm(numStudents, mu, Sigma)
   
	# slopes: item structure provides indices for non-zero slopes
    itemstructure <- cbind(1:numItems, sample(1:dimK,numItems, replace=T))
    slopes <- itemstructure*0 
	slopes[itemstructure] <- rlnorm(numItems,0,0.5) 

	intercepts=runif(numItems,min=-3,max=2) ## difficulty param, uniform random
		
	RandomArray=array(runif(numStudents * numItems), dim=c(numStudents, numItems)) # Random between 0 and 1 from uniform distr
	pseudoscores=array(-1, dim=c(numStudents, numItems)) # initialize
    
	# matrix format for intercepts
	I = intercepts*array(1, dim = c(numItems, numStudents))
	
	# matrix format for logit
	Z = theta%*%t(slopes) + t(I)

	# expected response
	P = sigmoid(Z)	

	# observed data
	pseudoscores = ifelse(P > RandomArray, 1, 0)
	
	#return the matrix and the parameters used
	return(list(pseudomatrix=pseudoscores,skills=theta,intercepts=intercepts,slopes=slopes, itemstructure=itemstructure))
	
}
