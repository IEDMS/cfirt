#######################################################
#	basic sigmoid and logistic functions for IRT models
#	to use with cfirt code 
#		Yoav Bergner, 2012
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
