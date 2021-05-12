#Chapter 2 R script

#reading in data
afdp <- read.table("R script and data files/afdp.dat", header=FALSE)
varnames <-c("id","coa","age","gen","stress","emotion","negaff","peer")
names(afdp)<-varnames

#loading lavaan package
library(lavaan)

#fitting path analysis model
pathmodel.1 <- '
              #regressions
              stress ~ coa + gen + age
              emotion ~ coa + gen + age
              negaff ~ stress + emotion
              peer ~ negaff

              #covariances
              stress ~~ emotion         
              '
#double tilda is for covariances
# relationships between exogenous variables are implied so you don't need to specify it in the model


fit <- sem(pathmodel.1, data=afdp, meanstructure=TRUE)
summary(fit, standardized=TRUE, rsquare=TRUE)
# R sq to tell you how much variance is explained

summary(fit, std.nox=TRUE, rsquare=TRUE)

# the next are model implied covariance matrix
fitted(fit)

resid(fit, type="raw") 
# this gives you the raw residuals  covariance residuals 

resid(fit, type="normalized")
# this is the standardized





