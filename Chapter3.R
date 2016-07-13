### Chapter 3: Basic Latent Variable Models
##  ==============

## Example: Single factor model of WISC-IV data

## Marker variable

# import data
library(lavaan)

# convert vector of correlations into matrix
wisc4.cor <- lav_matrix_lower2full(c(1,0.72,1,0.64,0.63,1,0.51,0.48,0.37,1,0.37,0.38,0.38,0.38,1))

# enter the SDs
wisc4.sd <- c(3.01 , 3.03 , 2.99 , 2.89 , 2.98)

# name the variables
colnames(wisc4.cor) <- rownames(wisc4.cor) <- 
    c("Information", "Similarities", "Word.Reasoning", "Matrix.Reasoning", "Picture.Concepts")

names(wisc4.sd) <-  
    c("Information", "Similarities", "Word.Reasoning", "Matrix.Reasoning", "Picture.Concepts")

# convert correlations and SDs to covarainces
wisc4.cov <- cor2cov(wisc4.cor,wisc4.sd)

# specify single factor model
wisc4.model<-'
    g =~ a*Information + b*Similarities + c*Word.Reasoning + d*Matrix.Reasoning + e*Picture.Concepts'

# fit model
wisc4.fit <- cfa(model=wisc4.model, sample.cov=wisc4.cov, sample.nobs=550,  std.lv=FALSE)

# examine parameter estimates
summary(wisc4.fit,standardized=TRUE)
parameterEstimates(wisc4.fit,standardized=TRUE)

# check model
# model-implied covariances
fitted(wisc4.fit)

# transform model-implied covariances to correlations
wisc4Fit.cov <- fitted(wisc4.fit)$cov
wisc4Fit.cor <- cov2cor(wisc4Fit.cov)

# residual correlations
residuals(wisc4.fit,type="cor")

# measures of model fit 
fitMeasures(wisc4.fit)

# modification indices
modificationIndices(wisc4.fit)

## Standardized latent variable

# method 1
wisc4.model.Std<-'
g =~ NA*Information + a*Information + b*Similarities + c*Word.Reasoning + 
d*Matrix.Reasoning + e*Picture.Concepts
# constrain the LV variance to 1
g~~1*g
'
wisc4.fit.Std <- cfa(wisc4.model.Std, sample.cov=wisc4.cov, sample.nobs=550)
# method 2
wisc4.fit.Std <- cfa(wisc4.model, sample.cov=wisc4.cov, sample.nobs=550, std.lv=TRUE)

## Effects coding

wisc4.model.effects<-'
g =~ NA*Information + a*Information + b*Similarities + c*Word.Reasoning +
 d*Matrix.Reasoning + e*Picture.Concepts
# constrain the loadings to sum to one
a + b + c + d + e == 5
'
wisc4.fit.effects <- cfa(wisc4.model.effects, sample.cov=wisc4.cor, sample.nobs=550)

## Example: Two-factor model of WISC-IV data

wisc4.model2<-'
V =~ a*Information + b*Similarities + c*Word.Reasoning 
F =~ d*Matrix.Reasoning + e*Picture.Concepts
V~~f*F
'
wisc4.fit2 <- cfa(wisc4.model2, sample.cov=wisc4.cov, sample.nobs=550)

## Structure coefficients

wisc4.fit2Alt <- cfa(wisc4.model2, sample.cov=wisc4.cor, sample.nobs=550, std.lv=TRUE)
wisc4.est2 <- inspect(wisc4.fit2Alt, what="coef")
wisc4.structure2 <- wisc4.est2$lambda %*% wisc4.est2$psi

## Example: Structural equation model

wisc4SEM.model <- '
# define latent variables
V =~ a*Information + b*Similarities + c*Word.Reasoning 
F =~ d*Matrix.Reasoning + e*Picture.Concepts
# define structural relations
V~k*F
'
wisc4SEM.fit <- cfa(wisc4SEM.model, sample.cov=wisc4.cov, sample.nobs=550)

