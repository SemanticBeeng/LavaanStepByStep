### Chapter 4: Latent Variable Models with Multiple Groups
##  =============

# Example: Multiple-group model examining invariance

# Input data

# input data
# variable names
wisc3.names <- c("Info", "Sim", "Vocab","Comp", "PicComp", "PicArr", "BlkDsgn", "ObjAsmb")

# group with manic symptoms
## covariances
manic.cov <- c(9.364, 7.777, 12.461, 6.422, 8.756, 10.112, 5.669, 7.445, 6.797, 8.123, 3.048, 
    4.922, 4.513, 4.116, 6.200, 3.505, 4.880, 4.899, 5.178, 5.114, 15.603, 3.690, 5.440, 5.220, 3.151, 3.587, 6.219, 11.223, 3.640, 4.641, 4.877, 3.568, 3.819, 5.811, 6.501, 9.797)
manic.cov <- lav_matrix_lower2full(manic.cov)

## means 
manic.means <- c(10.09, 12.07, 10.25, 9.96, 10.90, 11.24, 10.30, 10.44)
## label the covariances and means
colnames(manic.cov) <- rownames(manic.cov)  <- wisc3.names
names(manic.means) <- wisc3.names

# norming group 
## covariances
norming.cov <- c(9.610, 5.844, 8.410, 6.324, 6.264, 9.000, 4.405, 4.457, 5.046, 8.410, 4.464,  4.547, 4.512, 3.712, 10.240, 3.478, 2.967, 2.970, 2.871, 3.802,  10.890, 5.270, 4.930, 4.080, 3.254, 5.222, 3.590, 11.560, 4.297, 4.594, 4.356, 3.158, 4.963, 3.594, 6.620, 10.890)
norming.cov <- lav_matrix_lower2full(norming.cov) 

## means
norming.means <- c(10.10, 10.30, 9.80, 10.10, 10.10, 10.10, 9.90, 10.20)

## label the covariances and means
colnames(norming.cov) <- rownames(norming.cov)  <- wisc3.names
names(norming.means) <- wisc3.names

# combine the covariance matrices, sample sizes, and means into single list objects
combined.cov <- list(manic=manic.cov, norming=norming.cov)
combined.n <- list(manic=81, norming=200)
combined.means <- list(manic=manic.means, norming=norming.means)

# Fit the model

# specify fit indices of interest
fit.indices <- c("chisq", "df", "cfi", "rmsea", "srmr", "mfi")

# specify model
wisc3.model <-'
  VC =~ Info + Sim + Vocab + Comp 
  VS =~ PicComp + PicArr + BlkDsgn + ObjAsmb
  VC ~~ VS
'

# fit separate models for both groups
# group with manic symptoms
manic.fit <- cfa(wisc3.model, sample.cov=manic.cov, sample.nobs=81, sample.mean=manic.means, meanstructure=TRUE) 
fitMeasures(manic.fit, fit.indices)

# norming group
norming.fit <- cfa(wisc3.model, sample.cov=norming.cov, sample.nobs=200, sample.mean=norming.means, , meanstructure=TRUE) 
fitMeasures(norming.fit, fit.indices)

# configural invariance
configural.fit <- cfa(wisc3.model, sample.cov=combined.cov, sample.nobs=combined.n, sample.mean=combined.means) 
fitMeasures(configural.fit, fit.indices)

# weak invariance
weak.fit <- cfa(wisc3.model, sample.cov=combined.cov, sample.nobs=combined.n, sample.mean=combined.means, group.equal=c("loadings")) 
fitMeasures(weak.fit, fit.indices)

# strong invariance
strong.fit <- cfa(wisc3.model, sample.cov=combined.cov, sample.nobs=combined.n, sample.mean=combined.means, group.equal=c("loadings", "intercepts")) 
fitMeasures(strong.fit, fit.indices)

# show modification indices for strong invariance model in descending order
strong.mi <- modindices(strong.fit)
strong.mi <- strong.mi[strong.mi$op=="~1",]
strong.mi[order(strong.mi$mi, decreasing=TRUE),]

# strong invariance 2: intercepts for Similarities subtest is freely estimated
strong.fit2 <- cfa(wisc3.model, sample.cov=combined.cov, sample.nobs=combined.n,  sample.mean=combined.means, group.equal=c("loadings", "intercepts"), group.partial=c("Sim~1"))
fitMeasures(strong.fit2, fit.indices)

# partial strict invariance
strict.fit <- cfa(wisc3.model, sample.cov=combined.cov, sample.nobs=combined.n,  sample.mean=combined.means, group.equal=c("loadings", "intercepts", "residuals"),  group.partial=c("Sim~1", "Sim~~Sim"))
fitMeasures(strict.fit, fit.indices)

# partial strict invariance 2: residual variances for Picture Completion, Comprehension, and Picture Arrangement subtests are freely estimated
strict.fit2 <- cfa(wisc3.model, sample.cov=combined.cov, sample.nobs=combined.n,  sample.mean=combined.means, group.equal=c("loadings", "intercepts", "residuals"), group.partial=c("Sim~1", "Sim~~Sim", "PicComp~~PicComp", "Comp~~Comp", "PicArr~~PicArr"))
fitMeasures(strict.fit2, fit.indices)

# latent variances
factor.var.fit <- cfa(wisc3.model, sample.cov=combined.cov, sample.nobs=combined.n, sample.mean=combined.means, group.equal=c("loadings", "intercepts", "residuals", "lv.variances"), group.partial=c("Sim~1", "Sim~~Sim", "PicComp~~PicComp", "Comp~~Comp", "PicArr~~PicArr"))
fitMeasures(factor.var.fit, fit.indices)

# latent covariances
factor.covar.fit <- cfa(wisc3.model, sample.cov=combined.cov, sample.nobs=combined.n, sample.mean=combined.means, group.equal=c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances"), group.partial=c("Sim~1", "Sim~~Sim", "PicComp~~PicComp", "Comp~~Comp", "PicArr~~PicArr"))
fitMeasures(factor.covar.fit, fit.indices)

# latent means
factor.means.fit <- cfa(wisc3.model, sample.cov=combined.cov, sample.nobs=combined.n, sample.mean=combined.means, group.equal=c("loadings", "intercepts", "residuals", "lv.variances", "lv.covariances", "means"), group.partial=c("Sim~1", "Sim~~Sim", "PicComp~~PicComp", "Comp~~Comp", "PicArr~~PicArr"))
fitMeasures(factor.means.fit, fit.indices)

# Example: Behavior genetic analysis

# import data
## MZ twins
MZ <- lav_matrix_lower2full(c(.725,.589,.792))
rownames(MZ) <- colnames(MZ) <- c("P1", "P2")

## DZ twins
DZ <- lav_matrix_lower2full(c(.779,.246,.837))
rownames(DZ)  <- colnames(DZ) <- c("P1", "P2")

# combine the covariances and sample sizes
bmi.cov <- list(MZ=MZ,DZ=DZ)
bmi.n <- list(MZ=534,DZ=328)

# specify ADE model 
bmi.ade.model<-'
  # build the factor model with group constraints
  A1 =~ NA*P1 + c(a,a)*P1 + c(.5,.5)*P1
  A2 =~ NA*P2 + c(a,a)*P2 + c(.5,.5)*P2
  D1 =~ NA*P1 + c(d,d)*P1 
  D2 =~ NA*P2 + c(d,d)*P2 
  
  # constrain the factor variances
  A1 ~~ 1*A1
  A2 ~~ 1*A2
  D1 ~~ 1*D1
  D2 ~~ 1*D2
  P1~~c(e2,e2)*P1
  P2~~c(e2,e2)*P2

  # constrain the factor covariances
  A1 ~~ c(1,.5)*A2
  A1 ~~ 0*D1 + 0*D2
  A2 ~~ 0*D1 + 0*D2
  D1 ~~ c(1,.25)*D2 
'
# fit model
bmi.ade.fit <- cfa(bmi.ade.model, sample.cov=bmi.cov, sample.nobs=bmi.n)
summary(bmi.ade.fit, standardized=TRUE)

