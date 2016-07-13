Chapter 7: Missing Data

Data can be downloaded here

Exploring missing values

# load mcar data
mcar.data <- read.table("mcar.dat", sep=",", header=TRUE)

# missing data patterns
library(mice)
md.pattern(mcar.data)

# Little's MCAR test of mean equality
library(BaylorEdPsych)
mcar.little <- LittleMCAR(mcar.data)
mcar.little[c("chi.square", "df", "p.value")]
# Examine equality of means and covariances
library(MissMech)
TestMCARNormality(mcar.data)
Handling missing values

# model specification
complete.model <- '
read =~ a*z1 + b*z2 + c*z3
read ~ g*x1

z1~~d*z1
z2~~e*z2
z3~~f*z3
read~~h*read
'

# listwise deletion
mcarListwise.fit <- sem(complete.model, data=mcar.data)
summary(mcarListwise.fit, rsquare=TRUE, standardized=TRUE)

# pairwise deletion
library(psych)
pairwiseMCAR.cov <- cov(mcar.data, use="pairwise.complete.obs")
pairwiseMCAR.mean <- c(mean(mcar.data$z1,na.rm=TRUE),mean(mcar.data$z2, na.rm=TRUE),mean(mcar.data$z3,na.rm=TRUE),mean(mcar.data$x1, na.rm=TRUE),mean(mcar.data$x2, na.rm=TRUE))
names(pairwiseMCAR.mean)<-colnames(pairwiseMCAR.cov)
mcarPairwise.n <- min(count.pairwise(mcar.data))

mcarPairwise.fit <- sem(complete.model, sample.cov=pairwiseMCAR.cov, sample.nobs=mcarPairwise.n, sample.mean=pairwiseMCAR.mean)
summary(mcarPairwise.fit, rsquare=TRUE, standardized=TRUE)

# mean imputation
library(Hmisc)
mcarMeanI.data <- mcar.data
mcarMeanI.data$z1 <- impute(mcarMeanI.data$z1, fun=mean)
mcarMeanI.data$z2 <- impute(mcarMeanI.data$z2, fun=mean)
mcarMeanI.data$z3 <- impute(mcarMeanI.data$z3, fun=mean)

mcarMeanImputation.fit<-sem(complete.model, data=mcarMeanI.data)
summary(mcarMeanImputation.fit, rsquare=TRUE)

# FIML
mcarFIML.fit <- sem(complete.model, data=mcar.data, missing="fiml")
summary(mcarFIML.fit, rsquare=TRUE, standardized=TRUE)

# multiple imputation using mice
library(semTools)
mcarMI.fit <- runMI(complete.model, data=mcar.data, m=20, miPackage="mice", fun="sem", seed=56587)

# multiple imputation using Amelia
library(Amelia)
mcar.sim <- amelia(MCAR.data,m=20)
mcarMI.fit <- runMI(complete.model, data=MCAR.sim$imputations, fun="sem")
Auxiliary variable

library(semTools)
# FIML with auxiliary variable
mcarFIMLAux2.fit <- auxiliary(complete.model, aux="x2", data=mcar.data, fun="sem")
summary(mcarFIMLAux2.fit, standardized=TRUE)
