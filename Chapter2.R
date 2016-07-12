### Chapter 2: Path Models and Analysis
## ====================================

## Example: Path Analysis using lavaan

# create a correlation matrix
library(lavaan)
regression.cor <- lower2full(c(1.0,0.20,1,0.24,0.30,1,0.70,0.80,0.30,1))
# name the variables in the matrix
colnames(regression.cor) <- rownames(regression.cor) <- c("X1", "X2", "X3", "Y") 

# model syntax
regression.model <-'
# structural model for Y
Y ~ a*X1 + b*X2 + c*X3 
# label the residual variance of Y
Y ~~ z*Y 
'
# fit the model
regression.fit <- sem(regression.model, sample.cov=regression.cor, sample.nobs=1000)
summary(regression.fit, rsquare=TRUE)

## Example: Indirect Effects

# input data
beaujean.cov <- lav_matrix_lower2full(c(648.07, 30.05, 8.64, 140.18, 25.57, 233.21))
colnames(beaujean.cov) <- rownames(beaujean.cov) <- c("salary", "school", "iq")

# specify the path model
beaujean.model <- '
salary ~ a*school + c*iq
iq ~ b*school # this is reversed in first printing of the book 
ind:= b*c 
'
# estimate parameters
beaujean.fit <- sem(beaujean.model, sample.cov=beaujean.cov, sample.nobs=300)
summary(beaujean.fit)

## Create output table
library(xtable)

xtable(parameterEstimates(regression.fit, standardized=TRUE)[,c(1:3,5:6,12)],
       caption="Parameter Estimates from Path Analysis Model.", label="tab:path-analysis-estimates")

