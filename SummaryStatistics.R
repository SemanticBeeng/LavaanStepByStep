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
