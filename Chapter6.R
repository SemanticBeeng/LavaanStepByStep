Chapter 6: Models with Dichotomous Indicator Variables

Example: dichotomous indicator variables

Import data

library(psych)
data(lsat6)

# tetrachoric correlations
tetrachoric(lsat6)
Item response theory models

library(ltm)
# logistic distribution
# discrimination and difficulty parameterization
lsat.IRT <- ltm(lsat6~z1, IRT.param=TRUE)
coef(lsat.IRT)

# slope and intercept parameterization
lsat.SI <- ltm(lsat6~z1, IRT.param=FALSE)
coef(lsat.SI)

# item characteristic curves
plot(lsat.IRT)

# normal distributon
lsat.NO <- irt.fa(lsat6, plot=FALSE)
# IRT parameter estimates
lsat.NO$irt
# loadings
lsat.NO$fa
# thresholds
lsat.NO$tau
Item factor analysis models

library(lavaan)
twoP.model<-'
# loadings
Theta =~ l1*Q1 + l2*Q2 + l3*Q3 + l4*Q4 + l5*Q5
# thresholds
Q1 | th1*t1
Q2 | th2*t1
Q3 | th3*t1
Q4 | th4*t1
Q5 | th5*t1
# convert loadings to slopes (normal)
alpha1.N := (l1)/sqrt(1-l1^2)
alpha2.N := (l2)/sqrt(1-l2^2)
alpha3.N := (l3)/sqrt(1-l3^2)
alpha4.N := (l4)/sqrt(1-l4^2)
alpha5.N := (l5)/sqrt(1-l5^2)
# convert thresholds to intercepts (normal)
beta1.N := (-th1)/sqrt(1-l1^2)
beta2.N := (-th2)/sqrt(1-l2^2)
beta3.N := (-th3)/sqrt(1-l3^2)
beta4.N := (-th4)/sqrt(1-l4^2)
beta5.N := (-th5)/sqrt(1-l5^2)
# convert intercepts to locations (normal)
loc1 := -beta1.N/alpha1.N
loc2 := -beta2.N/alpha2.N
loc3 := -beta3.N/alpha3.N
loc4 := -beta4.N/alpha4.N
loc5 := -beta5.N/alpha5.N
# convert loadings to slopes (logistic)
alpha1.L := (l1)/sqrt(1-l1^2)*1.7
alpha2.L := (l2)/sqrt(1-l2^2)*1.7
alpha3.L := (l3)/sqrt(1-l3^2)*1.7
alpha4.L := (l4)/sqrt(1-l4^2)*1.7
alpha5.L := (l5)/sqrt(1-l5^2)*1.7
# convert thresholds to locations (logistic)
loc1.L := th1/l1
loc2.L := th2/l2
loc3.L := th3/l3
loc4.L := th4/l4
loc5.L := th5/l5
# convert locations to intercepts (logistic)
beta1.L := (-alpha1.L)*loc1.L
beta2.L := (-alpha2.L)*loc2.L
beta3.L := (-alpha3.L)*loc3.L
beta4.L := (-alpha4.L)*loc4.L
beta5.L := (-alpha5.L)*loc5.L
'

twoP.fit <- cfa(twoP.model, data=data.frame(lsat6),  std.lv=TRUE, ordered=c("Q1","Q2","Q3","Q4", "Q5"))
summary(twoP.fit, standardized=TRUE)
