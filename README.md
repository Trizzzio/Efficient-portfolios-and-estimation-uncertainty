# Efficient-portfolios-and-estimation-uncertainty
The goal of this exercise is to 1) Find the standard (Markowitz) efficient porfolio 2) Show how estimation uncertainty can impact optimal portfolio weights.  The exercise is coded in R using mostly packages within the tidyverse.

LoadingDataplusSharpeRatios:
Script loads data (For this exercise I use a Portfolio of 10 Industries that can be found on Kenneth French's website (https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html)), computes sample mean and Covariance matrix (Note that this sample mean and Variance are going to be assumed to be "true" population mean and variance in the next exercise), as well as Sharpe Ratios for all the portfolios. 

EfficentFrontier:
Script calculates Efficient Frontier, Minimum Variance Portfolio and Tangency Portfolio and Plots it. 

EstimationUncertaintyI: 
This script tries to show how estimation uncertainty can affect the results. First I simulate 100 observations based on the "true" population mean and variance and plot and efficient frontier based on these new sample mean and variance against the true efficient frontier. Second I repeate the same exercise 250 times (with two different sample sizes) and plot all the 250 efficient frontiers against the "true" efficient frontier. 

EstimatioUncertaintyII:
Calcualtes the Sharpe Ratios for all the simulated efficient portolios and visualize them in a histogram. 


