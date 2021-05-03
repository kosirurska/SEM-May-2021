#Chapter 3 R script

#reading in data
afdp <- read.table("afdp.dat", header=FALSE)
varnames <-c("id","coa","age","gen","stress","emotion","negaff","peer")
names(afdp)<-varnames

#loading lavaan package
library(lavaan)

#re-fitting initial path analysis model from Chapter 2
pathmodel.1 <-'
              #regressions
              stress ~ coa + gen + age
              emotion ~ coa + gen + age
              negaff ~ stress + emotion
              peer ~ negaff

              #covariances
              stress ~~ emotion              
              '
fit.1 <- sem(pathmodel.1, data=afdp, meanstructure=TRUE)
summary(fit.1, fit.measures=TRUE)

#fitting modified model adding direct effects of coa on negaff and peer
pathmodel.2 <- '
               #regressions
               stress ~ coa + gen + age
               emotion ~ coa + gen + age
               negaff ~ stress + emotion + coa
               peer ~ negaff + coa

               #covariances
               stress ~~ emotion              
               '
fit.2 <- sem(pathmodel.2, data=afdp, meanstructure=TRUE)
summary(fit.2, fit.measures=TRUE)

#likelihood ratio (chi-square difference) test between models
lavTestLRT(fit.2, fit.1)

#examining modification indices for initial model
modindices(fit.1, sort.=TRUE, minimum.value=10)

#fitting modified model with direct effect of age on peer
pathmodel.3 <-'
              #regressions
              stress ~ coa + gen + age
              emotion ~ coa + gen + age
              negaff ~ stress + emotion
              peer ~ negaff + age

              #covariances
              stress ~~ emotion              
              '
fit.3 <- sem(pathmodel.3, data=afdp, meanstructure=TRUE)
summary(fit.3, fit.measures=TRUE)
lavTestLRT(fit.3, fit.1)
modindices(fit.3, sort.=TRUE, minimum.value=10)

#fitting modified model with direct effect of age and coa on peer
pathmodel.4 <-'
              #regressions
              stress ~ coa + gen + age
              emotion ~ coa + gen + age
              negaff ~ stress + emotion
              peer ~ negaff + age + coa

              #covariances
              stress ~~ emotion              
              '
fit.4 <- sem(pathmodel.4, data=afdp, meanstructure=TRUE)
summary(fit.4, fit.measures=TRUE)
modindices(fit.4, sort.=TRUE, minimum.value=10)

#fitting modified model that also adds direct effect of age on negaff
pathmodel.5 <-'
              #regressions
              stress ~ coa + gen + age
              emotion ~ coa + gen + age
              negaff ~ stress + emotion + age
              peer ~ negaff + age + coa

              #covariances
              stress ~~ emotion              
              '
fit.5 <- sem(pathmodel.5, data=afdp, meanstructure=TRUE)
summary(fit.5, fit.measures=TRUE)

summary(fit.5, standardized=TRUE, rsquare=TRUE)
summary(fit.5, std.nox=TRUE, rsquare=TRUE)

#testing direct and indirect effects
effects.5 <-'
            #regressions
            stress ~ c_s*coa + gen + age
            emotion ~ c_e*coa + gen + age
            negaff ~ s_n*stress + e_n*emotion + age
            peer ~ n_p*negaff + age + c_p*coa 

            #covariances
            stress ~~ emotion         

            #direct effect
            dir := c_p

            #specific indirect effects
            ind_s := c_s*s_n*n_p
            ind_e := c_e*e_n*n_p

            #total indirect effect
            tot_ind := c_s*s_n*n_p + c_e*e_n*n_p

            #total effect
            tot := c_s*s_n*n_p + c_e*e_n*n_p + c_p
            '
set.seed(62973)
fit.5b <- sem(effects.5, data=afdp, meanstructure=TRUE, se="bootstrap", bootstrap=1000)
parameterEstimates(fit.5b, boot.ci.type = "perc")
parameterEstimates(fit.5b, boot.ci.type = "bca.simple")