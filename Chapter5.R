### Chapter 5: Models with Multiple Time Periods
## ===============

# Example: Latent curve model

# Import data

# Download LCM.zip file from UCLA web page. The nypa95_listwise.sav file is in the SPSS folder of the the chapter02 folder.

library(foreign)

crime.data <- read.spss("nypa95_listwise.sav", to.data.frame=TRUE)
crime.data <- na.omit(crime.data)

# make state x centered poverty interaction variable
crime.data$STATEPOV <- crime.data$PA * crime.data$CPV12590

# dataset 1
time <- c("JANFEB95", "MARAPR95", "MAYJUN95","JLYAUG95")
crime.cov <- cov(crime.data[time])
crime.mean <- colMeans(crime.data[time])
names(crime.mean) <- colnames(crime.cov) <- rownames(crime.cov) <- 
  c("Time1", "Time2", "Time3", "Time4")

# dataset 2
crime.vars <- c("JANFEB95", "MARAPR95", "MAYJUN95","JLYAUG95", "PA", "CPV12590", "STATEPOV")
crime2.cov <- cov(crime.data[crime.vars])
crime2.mean <- colMeans(crime.data[crime.vars])
names(crime2.mean) <- colnames(crime2.cov) <- rownames(crime2.cov) <- 
  c("Time1", "Time2", "Time3", "Time4", "State", "Poverty", "StPov")

# Basic latent curve model specification

library(lavaan)
# mean latent intercept and constrained residual variances
crime.model1 <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
  i~~0*i
  # residual variances
  Time1~~r*Time1
  Time2~~r*Time2
  Time3~~r*Time3
  Time4~~r*Time4
'
crime.fit1 <- growth(crime.model1, sample.cov=crime.cov, sample.mean=crime.mean, sample.nobs=952)

# mean latent intercept that is allowed to vary, and constrained residual variances
crime.model2 <- '
# intercept
i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
# residual variances
Time1~~r*Time1
Time2~~r*Time2
Time3~~r*Time3
Time4~~r*Time4
'
crime.fit2 <- growth(crime.model2, sample.cov=crime.cov, 
                     sample.mean=crime.mean, sample.nobs=952)

# mean latent intercept that is allowed to vary, mean latent slope, and constrained residual variances
crime.model3 <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
  # slope
  s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4
  s~0*1
  s~~0*i

  # residual variances
  Time1~~r*Time1
  Time2~~r*Time2
  Time3~~r*Time3
  Time4~~r*Time4
'
crime.fit3 <- growth(crime.model3, sample.cov=crime.cov, 
                     sample.mean=crime.mean, sample.nobs=952)

# mean latent intercept that is allowed to vary, mean latent slope that is allowed to vary, and constrained residual variances
crime.model4 <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4
  
  # slope
  s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4
  
  # residual variances
  Time1~~r*Time1
  Time2~~r*Time2
  Time3~~r*Time3
  Time4~~r*Time4
'
crime.fit4 <- growth(crime.model4, sample.cov=crime.cov, 
                     sample.mean=crime.mean, sample.nobs=952)

# unconstrained model
crime.model5 <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4

  # slope
  s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4
'
crime.fit5 <- growth(crime.model5, sample.cov=crime.cov, 
                     sample.mean=crime.mean, sample.nobs=952)

# Latent curve models with covariates

# State is a predictor variable
crime.model6 <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4

  # slope
  s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4

  # regression
  i + s ~ State 
'
crime.fit6 <- growth(crime.model6, sample.cov=crime2.cov, 
                     sample.mean=crime2.mean, sample.nobs=952)

# State and poverty are predictors
crime.model7 <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4

  # slope
  s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4

  # regression
  s + i ~ State + Poverty
'
crime.fit7 <- growth(crime.model7, sample.cov=crime2.cov, 
                     sample.mean=crime2.mean, sample.nobs=952)

# State, poverty, and their interaction are predictors
crime.model8 <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4

  # slope
  s =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4

  # regression
  s + i ~ State + Poverty + StPov
'
crime.fit8 <- growth(crime.model8, sample.cov=crime2.cov, 
                     sample.mean=crime2.mean, sample.nobs=952)

# Alternative specification of latent intercept and slope

# intercept coded to be the last data collection period
model <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4

  # slope
  s =~ -3*Time1 + -2*Time2 + -1*Time3 + 0*Time4
'

# intercept coded to be the third data collection period
model <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4

  # slope
  s =~ -2*Time1 + -1*Time2 + 0*Time3 + 1*Time4
'

# intercept coded to be the middle of data collection
model <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4

  # slope
  s =~ -1.5*Time1 + -0.5*Time2 + 0.5*Time3 +
  1.5*Time4
'

# using two units as the time between data collection periods
model <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4

  # slope
  s =~ 0*Time1 + 2*Time2 + 4*Time3 + 6*Time4
'
# model with a follow-up time period distant (four units) from the end of regular data collection   
model <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4

  # slope
  s =~ 0*Time1 + 1*Time2 + 2*Time3 + 6*Time4
'

# quadratic growth--second latent slope's loadings are the square of the first latent slope's loadings
model <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4

  # slope 1
  s1 =~ 0*Time1 + 1*Time2 + 2*Time3 + 3*Time4

  # slope 2
  s2 =~ 0*Time1 + 1*Time2 + 4*Time3 + 9*Time4
'

# piecewise linear slopes
model <- '
  # intercept
  i =~ 1*Time1 + 1*Time2 + 1*Time3 + 1*Time4 + 1*Time5 + 1*Time6 

  # slope 1
  s1 =~ -3*Time1 + -2*Time2 + -1*Time3 + 0*Time4 + 0*Time5 + 0*Time6

  # slope 2
  s2 =~ 0*Time1 + 0*Time2 + 0*Time3 + 0*Time4 + 1*Time5 + 2*Time6
'

