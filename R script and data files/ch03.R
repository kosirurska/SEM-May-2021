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
# fit measures = TRUE gives all the model fit indices in the output
# Loglikelihood user model (H0)     -1158.938 if multiply by 2 you get the chisq.
# RMSEA CI gives you also some idea of the fit


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

# this model above was re-specified solely based on the theory
# formal test to see which model fit is better-->
#likelihood ratio (chi-square difference) test between models
lavTestLRT(fit.1, fit.2) # the order does not matter
# the model fit is better but still bad

## NEXT STEP THEN IS TO SPEAK TO THE DATA - data driven, careful,
# biggest bank for the buck are Modification indices

#examining modification indices for initial model
modindices(fit.1, sort.=TRUE, minimum.value=10)

# lhs op    rhs --left hand side , op = operator, right hand side
# eg the first one does not make sense AGE regressed on age does not make sense
# the next one is reverted and will be put in the model #3

# lhs op    rhs     mi   epc sepc.lv sepc.all sepc.nox
# 63    age  ~   peer 53.942 1.114   1.114    0.413    0.413
# 47   peer  ~    age 42.540 0.129   0.129    0.348    0.241 <--------!!!!!
# 42 negaff  ~    age 11.889 0.119   0.119    0.179    0.124
# 62    age  ~ negaff 10.877 0.303   0.303    0.201    0.201

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
# we vastly improved the model fit by 1 degree of freedom
# but other model fit indices are still bad

lavTestLRT(fit.3, fit.1)

# inspect the modindices again
modindices(fit.3, sort.=TRUE, minimum.value=10)

# lhs op    rhs     mi   epc sepc.lv sepc.all sepc.nox
# 44   peer  ~ stress 13.381 0.149   0.149    0.192    0.192
# 63    age  ~   peer 12.858 1.977   1.977    0.721    0.721
# 43 negaff  ~    age 11.889 0.119   0.119    0.179    0.124
# 46   peer  ~    coa 11.853 0.185   0.185    0.175    0.350 <---- next based on theory
# 62    age  ~ negaff 10.859 0.303   0.303    0.200    0.200


#EPC - expected parameter change, values of raw coefficients if we added those to the model
# usually more concerned about the magnitude 

# re-specify AGAIN #4
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

#Inspect the fit measures again, T (test stat is better), CFI, TLI looking better ---TLI is looking better
summary(fit.4, fit.measures=TRUE)

# we inspect again (round 3) - do only 1 at the time
modindices(fit.4, sort.=TRUE, minimum.value=10)

# Burn one more degree of freedom and you expect a change of 11 in T

# 
# lhs op    rhs     mi   epc sepc.lv sepc.all sepc.nox
# 44 negaff  ~    age 11.889 0.119   0.119    0.179    0.124
# 62    age  ~ negaff 10.875 0.303   0.303    0.201    0.201

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

# the fit is looking reasonable and we have already used 3 Modification indices

summary(fit.5, standardized=TRUE, rsquare=TRUE) #we want it standardize to interpret SD changes

summary(fit.5, std.nox=TRUE, rsquare=TRUE) # partially std coefficients 


# NEXT STEP IS TESTING FOR THE MEDIATION
# the model is rewritten so that includes the labels
# lavaan labels them by *c_s (multiplier - regression coefficient, just to supply a label for that path)
# you can label them whatever you want


#testing direct and indirect effects
# := means define ---we are defining effects, new ones, in terms of other parametes estimates already in your model
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
            ind_s := c_s*s_n*n_p #<- indirect effect via stress --was most of the interest
            ind_e := c_e*e_n*n_p

            #total indirect effect
            tot_ind := c_s*s_n*n_p + c_e*e_n*n_p

            #total effect
            tot := c_s*s_n*n_p + c_e*e_n*n_p + c_p
            '

# For replicability set seed
set.seed(62973)

# bootstrapping is a random sampling --hence the seed, if you dont specify you wont get the same results
# if you dont specify it uses internal clock on the computer

fit.5b <- sem(effects.5, 
              data=afdp, 
              meanstructure=TRUE, 
              se="bootstrap", #
              bootstrap=1000) # number of bootstraps 

parameterEstimates(fit.5b, boot.ci.type = "perc") # percentile based

# the output gives the % bootstrap for every parameter in the model, hence 35 lines
# if the upper and lower limit include 0 then it is not significant 
# I am only interested in the effects defined := (they are at the very bottom)
# lhs op                         rhs   label    est    se      z pvalue ci.lower ci.upper
#  -- || --
# 31     dir :=                         c_p     dir  0.185 0.052  3.575  0.000    0.082    0.287
# 32   ind_s :=                 c_s*s_n*n_p   ind_s  0.015 0.008  1.995  0.046    0.004    0.032
# 33   ind_e :=                 c_e*e_n*n_p   ind_e  0.009 0.005  1.661  0.097    0.000    0.021
# 34 tot_ind :=     c_s*s_n*n_p+c_e*e_n*n_p tot_ind  0.024 0.010  2.351  0.019    0.008    0.048
# 35     tot := c_s*s_n*n_p+c_e*e_n*n_p+c_p     tot  0.209 0.052  4.017  0.000    0.104    0.312

## the z and SE are ignored because it assumes a normal sampling distribution whereas the bootstraped CI is asymmetric and more reliable

# actual coefficient - the units are og predictor and ultimate outcome - raw unit change in y per x per this pathway (mediators get the )
# can also standardize them and interpret them as any others just thru a particular pathway
#PME proportion mediated effect - total ind /total effect - how much (%) of the effect is running through the mediated
# careful when speaking of the proportions
# IDE - indirect to the direct ration (relative balance between the 2)
# these can be unstable in denominator is small!
# these can be used in enhancing interpretations

parameterEstimates(fit.5b, boot.ci.type = "bca.simple") # biased corrected - argued
# similar results in terms of bCI - but bias adjusted for the model
# percentile is more intuitive for the reader 
