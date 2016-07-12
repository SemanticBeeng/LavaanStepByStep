### Chapter 1
## ==============

# load lavaan
library(lavaan)
# input covariances
example.cor <- lav_matrix_lower2full(c(1, 0.85, 1, 0.84, 0.61, 1, 0.68, 0.59, 0.41, 1))
# name the rows and columns
rownames(example.cor) <- colnames(example.cor)  <- c("Var1", "Var2", "Var3", "Var4") 

## Summary statistics 
# load the MLBPitching2011 dataset from the BaylorEdPsych package
library(BaylorEdPsych)
data(MLBPitching2011) 
# summary statistics
summary(MLBPitching2011$ERAP)
library(psych)
describe(MLBPitching2011$ERAP) 
# frequency of each value of ERAP variable
table(MLBPitching2011$ERAP) 
# frequency table
# make cut points for frequency table groupings--here I used 50
boundaries <- seq(0,550,50) 
# frequency table
table(cut(MLBPitching2011$ERAP, boundaries))
# relative frequency table
table(cut(MLBPitching2011$ERAP, boundaries)) / length(MLBPitching2011$ERAP)

# Pearson correlations for losses (L) and Age
cor(MLBPitching2011$Age,MLBPitching2011$L)
# Spearman correlation
cor(MLBPitching2011$Age,MLBPitching2011$L, method="spearman")

# covariance for losses (L) and Age
cov(MLBPitching2011$Age,MLBPitching2011$L)

## Simulated data

# simulate 1000 observations from Normal distribution with a mean of 100, and SD of 15
X.n<-rnorm(1000,mean=100,sd=15) 
# simulate 1000 observations from a Poisson distribution with a mean and variance of 2
X.p<-rpois(1000,lambda=2) 
# calculate mean and variance of Normal data
mean(X.n); var(X.n) 
# calculate mean and variance of Poisson data
mean(X.p); var(X.p)  

# plot the distributions
m1 <- seq(-4,4, .01)*15+100
m2 <- seq(0,15, 1)
plot(m1, dnorm(m1, mean=100, sd=15), type="l", col="black", lwd=3, ylab="Density", xlab="Value", main="Normal")
plot(m2, dpois(m2, lambda=2), type="b", lwd=3, xlim=c(-1,15), xlab="Value", ylab="Density", main="Poisson")

## Z scores using the scale() function

# Normal variable 
Z.X.n <- scale(X.n) 
# Poisson variable
Z.X.p <- scale(X.p) 

# calculate mean and variance of Normal Z-scores
mean(Z.X.n); var(Z.X.n)
# calculate mean and variance of Poisson Z-scores
mean(Z.X.p); var(Z.X.p)  

## Statistical tests

# uses BaylorEdPsych package's MLBPitching2011 data

# Z test
library(TeachingDemos)
z.test(na.omit(MLBPitching2011$WLP), mu=0.50, stdev=sqrt(.08))

# one sample t-test
t.test(MLBPitching2011$WLP, mu=.50, alternative="two.sided", conf.level=.95)
# independent samples t-test
t.test(WLP~Lg, data=MLBPitching2011, na.rm=TRUE, var.equal=TRUE) 
# dependent samples t-test
t.test(MLBPitching2011$W, MLBPitching2011$L, paired=TRUE)

# homogeneity of variance tests
# F-test
var.test(WLP[Lg=="NL"], WLP[Lg=="AL"], na.rm=TRUE) 
# Bartlett's Test
bartlett.test(WLP,Lg) 
library(car) 
# Levene's test
leveneTest(WLP, Lg,center="mean") 
# Brown-Forsyth Test 
leveneTest(WLP,Lg,center="median")

