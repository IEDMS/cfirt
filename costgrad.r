############################################################
#	Cost, Grad and Information Functions for cfirt
#
#		Yoav Bergner, 2012
#
############################################################



############################################################
#	Cost function (minus loglikelihood + regularization terms)
############################################################

cfirtCost <- function(param.vec, U, R, dimX, dimTheta, lambda) {

	param.matrices <- reshape.params(param.vec, nrow(U), ncol(U), dimX, dimTheta)
	X <- param.matrices$X
	Theta <- param.matrices$Theta
	
	ThetaRegTerm = sum(Theta[,(1+dimTheta[2]):dim(Theta)[2]]^2)
	XRegTerm = sum(X[,1:(dim(X)[2]-dimX[2])]^2)

	J=0	
	P = sigmoid(Theta %*% t(X))
	J = -sum(log(P)*U*R + log(1-P)*(1-U)*R) 
			+ lambda/2*(ThetaRegTerm + XRegTerm)

	return(J)
	
}

############################################################
#	Grad function (including regularization terms)
############################################################

cfirtGrad <- function(param.vec, U, R, dimX, dimTheta, lambda) {

	param.matrices <- reshape.params(param.vec, nrow(U), ncol(U), dimX, dimTheta)
	X <- param.matrices$X
	Theta <- param.matrices$Theta
	
	X.grad = array(0,dim(X))
	Theta.grad = array(0,dim(Theta))
	
	P = sigmoid(Theta %*% t(X))
	Theta.grad = ((P - U)*R) %*% X + lambda * Theta
	X.grad = t(R*(P - U)) %*% Theta +lambda * X;

	Theta.grad = Theta.grad[,(1+dimTheta[2]):dim(Theta)[2]] 
	X.grad = X.grad[,1:(dim(X)[2]-dimX[2])]
	
	return(c(c(X.grad),c(Theta.grad)))
}

############################################################
#	Fisher Information function (EXCLUDE regularization terms)
############################################################

cfirtDD <- function(param.vec, U, R, dimX, dimTheta) {	

	param.matrices <- reshape.params(param.vec, nrow(U), ncol(U), dimX, dimTheta)
	X <- param.matrices$X
	Theta <- param.matrices$Theta
			
	X.dd = array(0,dim(X))
	Theta.dd = array(0,dim(Theta))
		
	P = sigmoid(Theta %*% t(X))
	Theta.dd = as.matrix(P*(1-P)*R) %*% X^2
	X.dd = t(as.matrix(P*(1-P)*R)) %*% Theta^2

	Theta.dd = Theta.dd[,(1+dimTheta[2]):dim(Theta)[2]] 
	X.dd = X.dd[,1:(dim(X)[2]-dimX[2])]

	return(c(c(X.dd),c(Theta.dd)))
}



############################################################
#	reshape.params
#		converts between rolled out vector of all item and student
#		params to create separate matrices for each
#		also checks for commensurability in dimensions
#		used by cost, grad and information functions above
############################################################

reshape.params <- function(param.vec, numStudents, numItems, dimX, dimTheta){
	# commensurability check
	if (sum(dimX) != sum(dimTheta)) {
		cat("Error: dimensional mismatch in parameter vectors\n");
	} else if (!is.element(dimX[2],c(0,1)) | !is.element(dimTheta[2],c(0,1))) {
		cat("Error: second component of dimX and dimTheta must be binary 0 or 1 \n")
	} 

	X = matrix(param.vec[1:(numItems*dimX[1])],numItems,dimX[1])
	Theta = matrix(param.vec[(numItems*dimX[1]+1):length(param.vec)],numStudents,dimTheta[1])

	if (dimTheta[2] == 1)	Theta = cbind(rep(1,numStudents),Theta)
	if (dimX[2] == 1)	X = cbind(X, rep(1,numItems))
	
	return(list(X=X, Theta=Theta))
}


############################################################
############################################################
#	Below are option versions if you only want to do 2PL IRT
############################################################
############################################################

IRTCostFunction <- function(param.vec, U, R,lambda) {
	numStudents = nrow(U)
	numItems = ncol(U)		

	#reshape
	X = matrix(param.vec[1:(numItems*2)],numItems,2)
	Theta = matrix(param.vec[(numItems*2+1):length(param.vec)],numStudents,1)
	Theta = cbind(rep(1,numStudents),Theta)
	
	J=0	
	J = -sum(log(sigmoid(Theta %*% t(X)))*U*R + log(1-sigmoid(Theta %*% t(X)))*(1-U)*R) 
			+ lambda/2*(sum(X^2))
			+ lambda/2*(sum(Theta[,2]^2))
		
	return(J)
}

#######################################################

IRTGradFunction <- function(param.vec, U, R, lambda) {	

	numStudents = nrow(U)
	numItems = ncol(U)		
	#reshape
	X = matrix(param.vec[1:(numItems*2)],numItems,2)
	Theta = matrix(param.vec[(numItems*2+1):length(param.vec)],numStudents,1)	
	Theta = cbind(rep(1,numStudents),Theta)
		
	X.grad = matrix(0,numItems,2)
	Theta.grad = matrix(0,numStudents,2)
		
	Theta.grad = ((sigmoid(Theta %*% t(X)) - U)*R) %*% X + lambda * Theta
	X.grad = t(R*(sigmoid(Theta %*% t(X)) - U)) %*% Theta +lambda * X;
	Theta.grad = Theta.grad[,2]
	
	return(c(c(X.grad),c(Theta.grad)))
}

########

IRTvarFunction <- function(param.vec, U, R) {	

	numStudents = nrow(U)
	numItems = ncol(U)		
	#reshape
	X = matrix(param.vec[1:(numItems*2)],numItems,2)
	Theta = matrix(param.vec[(numItems*2+1):length(param.vec)],numStudents,1)	
	Theta = cbind(rep(1,numStudents),Theta)
		
	X.DD = matrix(0,numItems,2)
	Theta.DD = matrix(0,numStudents,2)
		
	P = sigmoid(Theta %*% t(X))
	Theta.DD = as.matrix(P*(1-P)*R) %*% X^2
	X.DD = t(as.matrix(P*(1-P)*R)) %*% Theta^2
	Theta.DD = Theta.DD[,2]

	return(c(c(X.DD),c(Theta.DD)))
}

