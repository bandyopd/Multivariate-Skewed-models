
## Fitting the SMS (semiparametric multivariate skew) model to simulated data 
## Code written by Apurva Bhingare.
## Last update: August 14, 2018

## Download the files from the SMS folder, and save them in a directory called 'SMS_files'
## Set the working directory as: 'SMS_files'


setwd('SMS_files')


## load and attach add-on packages
library(MASS)
library(R2jags)
library(mcmcplots)


ReadData=read.csv("Simulated_Data.csv", row.names=NULL)

## ReadData contains the following:
## y: bivariate response variable
## X1: binary covariate
## X2: continuous covariate


n= max(ReadData$PSU_ID)   ## sample size
m=2 			  ## dimension of the response


K<-30			  ## truncation bound for stick breaking process
M<-1			  ## concentration/precision parameter



## In the following lines of code, I reformat the dataset to obtain

# Y such that the (i,j)th element corresponds to the jth component of the ith response
# X1j is such that ith element corresponds to the value of covariate X1 for the jth component of the ith response
# X2j is such that ith element corresponds to the value of covariate X2 for the jth component of the ith response


Y=mat.or.vec(n,m)
Yt=mat.or.vec(n,m)
X1=mat.or.vec(n,m)
X2=mat.or.vec(n,m)


X11=mat.or.vec(n,1)
X12=mat.or.vec(n,1)
X21=mat.or.vec(n,1)
X22=mat.or.vec(n,1)


for (i in 1:n)
{
 Y[i,]=ReadData[ReadData$PSU_ID==i,2]
 X1[i,]=ReadData[ReadData$PSU_ID==i,3]
 X2[i,]=ReadData[ReadData$PSU_ID==i,4]
}

X11=X1[,1]
X12=X1[,2] 

X21=X2[,1]
X22=X2[,2]


##########################################################################################################################################
## list of the names of the data objects used by the model
bugsdata <- list ("K","n","m","Y","X11","X12","X21","X22")

## file containing the SMS model written in BUGS
model<-("SMS.txt")

## character vector of the names of the parameters to save which should be monitored
parameters <- c("alpha1","alpha2","rho","beta01","beta11","beta21","beta02","beta12","beta22")

## write a JAGS script, calls the model, and saves the simulations 
out <- jags(bugsdata, inits=NULL, parameters, model.file=model, n.chains=2, n.iter=20000, n.burnin=10000, n.thin=10)
	
## Create an mcmc list 
outJG<-as.mcmc.list(out$BUGSoutput)

## Obtain posterior summary
result=summary(outJG)
result

## generate trace plots and autocorrelation plots
mcmcplot(out)
