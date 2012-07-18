cfirt
=====
Collaborative Filtering style of Item Response Theory (IRT)

This is a package of tools for analyzing response data using what IRT-fluent people 
would call Joint Maximum Likelihood Estimation (JMLE) or what machine-learning savvy people 
might call collaborative filtering using regularized logistic regression.

The required inputs for starting are a response matrix, which should be delimited, binary-valued 
(dichotomous), and omitted or missing data coded as NA or -9 (or change the code to use your own).
Headers and row names are optional, though you will have to deal with them in the reading-in part
of the code.

A synthetic data example file is included: cfirtdata.txt

The current list of files included in this repo is:
README.md
cfirt.r	:	a source file for all of the other files
cfirtdata.txt	: 	sample data file (synthetic)
costgrad.r	:	the core vectorized functions for computing the loglikelihood

makeICCs.r	:	for plotting item characteristic curves with data in unidimensional IRT
SAMPLEcfirtICC.pdf	:	sample output from makeICCs.r	
plotCFmodels.r	:	for plotting the accuracy/RMSE performance of multiple CF models
SAMPLEmodelcompare.pdf	:	sample output from plotCFmodels.r	
rescale.r	:	for rescaling parameters to known/assumed ability distribution (post-facto)
SAMPLEmodelcompare.txt	: sample output from ex1\_ModelComparison.r
errorbars.r
usingcfirtfor2PL.r
UsingCfirtForModelComparison.r
ex2\_ModelComparison.r
basicfunctions.r
ex2\_2PL.r


