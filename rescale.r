#######################################################
# unidimensional IRT helper functions
#		useful for unidimensional IRT comparisons
#
#		Yoav Bergner, 2012
#######################################################


#######################################################
# SI2DD converts from Slope-Intercept form of logit variables
#	e.g. X1 + X2*Theta
#	to Difficulty/Discrimination form of variables
#	e.g. Discrim*(Theta-Difficulty)
#
#	the function has an optional argument depending on whether
#	standard errors are included for each of the variables
#

SI2DD <- function(X1,X2,noerrs=FALSE) {
	### to prevent div by zero errors
	if (noerrs) {
		X2[which(abs(X2) < 0.001)] <- 0.001
		diffs = -X1/X2
		list(diffs = diffs, discrims=X2)
	
	} else {
		X2[which(abs(X2[,1]) < 0.001),1] <- 0.001
		diffs = -X1[,1]/X2[,1]
		diffs.se = sqrt((X2[,1]^2*X1[,2]^2 + X1[,1]^2*X2[,2]^2)/X2[,1]^4) 
		list(diffs = cbind(diffs,diffs.se), discrims=X2)
	}
}


#######################################################
# DD2SI converts from Difficulty/Discrimination form of logit variables
#	to Slope-Intercept form of variables
#
#	the function for now requires standard errors to be included
#	see SI2DD

DD2SI <- function(Diff, Discrim) {
	X1 = -Diff[,1]*Discrim[,1]
	X1.se = sqrt(Discrim[,1]^2*Diff[,2]^2 + Diff[,1]^2*Discrim[,2]^2)
	list(X1=cbind(X1,X1.se), X2=Discrim)
}


#######################################################
# RescaleSI rescales the Slope-Intercept form of the logit variables
#	so that the ability distribution is unit normal
#
#	similar to SI2DD, there is an option to include standard errors or not

RescaleSI <- function(Theta, X1, X2, noerrs=FALSE) {
	if (noerrs) {
		if (mean(X2) < 0) {
			X2 = -X2 
			Theta = -Theta
		} 
		sig = sd(Theta)
		mu = mean(Theta)
		Theta <- (Theta-mu)/sig
		X1 <- X1 + mu*X2
		X2 <- sig*X2	
		return(list(Theta=Theta, X1=X1, X2=X2))
	} else {
		if (mean(X2[,1]) < 0) {
			X2[,1] = -X2[,1] 
			Theta[,1] = -Theta[,1]
		} 
		sig = sd(Theta[,1])
		mu = mean(Theta[,1])
		Theta[,1] <- (Theta[,1]-mu)/sig
		Theta[,2] <- Theta[,2]/sig
		X1[,1] <- X1[,1] + mu*X2[,1]
		X1[,2] <- sqrt(X1[,2]^2 + (mu*X2[,2])^2)
		X2[,1] <- sig*X2[,1]
		X2[,2] <- sig*X2[,2]		
		return(list(Theta=Theta, X1=X1, X2=X2))
	}
}


