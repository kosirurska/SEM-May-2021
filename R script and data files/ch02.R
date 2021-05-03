#Chapter 2 R script

#reading in data
afdp <- read.table("afdp.dat", header=FALSE)
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
fit <- sem(pathmodel.1, data=afdp, meanstructure=TRUE)
summary(fit, standardized=TRUE, rsquare=TRUE)
summary(fit, std.nox=TRUE, rsquare=TRUE)

fitted(fit)
resid(fit, type="raw")
resid(fit, type="normalized")
