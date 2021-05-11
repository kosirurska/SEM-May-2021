#Chapter 1 R script


#reading in data
afdp <- read.table("R Script and data files/afdp.dat", header=FALSE)
varnames <-c("id","coa","age","gen","stress","emotion","negaff","peer")
names(afdp)<-varnames
View(afdp)


#loading lavaan package
library(lavaan)


#one predictor regression of peer on coa
model.1 <- 'peer ~ coa'

fit <- sem(model.1, data=afdp, meanstructure=TRUE)
summary(fit)
summary(fit, standardized=TRUE)
summary(fit, std.nox=TRUE)
summary(fit, rsquare=TRUE)


#multiple regression of peer on coa, gen, and age
model.2 <- 'peer ~ coa + gen + age'

fit <- sem(model.2, data=afdp, meanstructure=TRUE)
summary(fit, std.nox=TRUE, rsquare=TRUE)


#multiple regression of peer on coa, gen, age, emotion, stress, negaff
model.3 <- 'peer ~ coa + gen + age + emotion + stress + negaff'

fit <- sem(model.3, data=afdp, meanstructure=TRUE)
summary(fit, standardized=TRUE, rsquare=TRUE)
summary(fit, std.nox=TRUE, rsquare=TRUE)

?sem
