Chapter 8: Sample Size Planning

*Warning: Depending on your computer, the following simulations could take multiple hours to converge

Example: Mean difference between two groups

# traditional power analysis
library(pwr)
pwr.t.test(d = 0.49, sig.level = 0.10, power = 0.80)
t.sampSize <- pwr.t.test(d = 0.49, sig.level = 0.10, power = 0.80)

# Monte Carlo power analysis

# convert effect size to correlation
library(compute.es)
des(d=0.49, n.1=100,n.2=100)

# specify data generation model
act.model <- '
# regression
act ~ 0.24*class
# error variance
act ~~ 0.9424*act
'
# specify analysis model
act2.model <- '
act ~ class
'
# simulate and analyze data
library(simsem)
act.power <- sim(nRep=1000, generate=act.model, model=act2.model, n =104, lavaanfun = "sem", multicore=TRUE, seed=0)
summaryParam(act.power,alpha = 0.10,detail=TRUE)

# search sample sizes
act.n <- sim(model=act2.model, n =rep(seq(50,120,10), 500), generate=act.model, lavaanfun = "sem", multicore=TRUE)

# power curve with line added at y= 0.80
plotPower(act.n, alpha=0.1, powerParam="act~class")
abline(h=.8, lwd=2, lty=2)

# find sample size needed for power = .0.8
act.pwr.n <- getPower(act.n, alpha = 0.10)
findPower(act.pwr.n, iv="N", power=0.8)
Example: Latent curve model

# specify data generation model
lcm.pop.model <- '
# latent variable model
i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
s =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
# latent variable means
i ~ 0.00*1
s ~ 0.20*1
# regressions, with parameter of interest labeled
i ~ 0.50*x  
s ~ a*x + 0.20*x 
# mean and variance of x
x ~ 0.50*1
x ~~ 0.25*x
# manifest (residual) variances
y1 ~~ 0.5*y1
y2 ~~ 0.5*y2
y3 ~~ 0.5*y3
y4 ~~ 0.5*y4
# latent variable residual variances/covariances
i~~0.25*i
s~~0.09*s
i~~0.0*s
'

# specify analysis model
lcm.model <- '
# latent variable model
i =~ 1*y1 + 1*y2 + 1*y3 + 1*y4
s =~ 0*y1 + 1*y2 + 2*y3 + 3*y4
# regressions
i ~ x
s ~ a*x
'

# show model's fixed and default values
lcm.pop.fit <- growth(lcm.pop.model, do.fit=FALSE, fixed.x=FALSE)
summary(lcm.pop.fit, std=TRUE, rsquare=TRUE)
fitted(lcm.pop.fit)

# run simulations for multiple sample sizes
lcm.n <- sim(nRep=NULL, model=lcm.model, n =rep(seq(100,200,10), 500), generate=lcm.pop.model,
lavaanfun = "growth", multicore=TRUE)

# find the sample size needed for power = .0.8
lcm.pwr.n <- getPower(lcm.n)
findPower(lcm.pwr.n, "N", 0.8)

# power analysis with n=149
lcm1.power <- sim(nRep=10000, model=lcm.model, n =149, generate=lcm.pop.model, lavaanfun = "growth", multicore=TRUE, seed=998877)
summaryParam(lcm1.power,alpha = 0.05,detail=TRUE)
Example: Latent curve model with attrition

# specify missing data
lcm.pcnt.missing <- '
y1 ~ p(0.12)  
y2 ~ p(0.18) + 1*x
y3 ~ p(0.27) + 1*x
y4 ~ p(0.50) + 1*x
' 
missing.model <- miss(logit=lcm.pcnt.missing)

# plot missing data
plotLogitMiss(lcm.pcnt.missing, x1lim=c(0,1))

# run simulations for multiple sample sizes
missing.model <- miss(logit=lcm.pcnt.missing, m=10, package="mice")
lcm.missing.n <- sim(nRep=NULL, model=lcm.model, n=rep(seq(200,300,10), 100), 
generate=lcm.pop.model, lavaanfun = "growth", multicore=TRUE, miss=missing.model)

# find the sample size needed for power = .0.8
lcm.missing.pwr.n <- getPower(lcm.missing.n)
findPower(lcm.missing.pwr.n, "N", 0.8)

# power analysis with n=273
lcm1.missing <- sim(nRep=10000, model=lcm.model, n =273, generate=lcm.pop.model, lavaanfun = "growth", multicore=TRUE, miss=missing.model, seed=4335)
summaryParam(lcm1.missing,alpha = 0.05,detail=TRUE)
