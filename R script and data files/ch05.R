#Chapter 5 R script

#loading lavaan package

library(lavaan)


#defining variable names

names <- c("ptgi","ptgi1", "ptgi2", "ptgi3", "ptgi4", "ptgi5", 
           "marital", "fa", "fr", "si", "child", "child18",
           "age", "gender", "depres", "comt", "con", "cha", 
           "es", "lo", "pro", "th", "diord", "time", "problem", 
           "em", "ind", "ru", "av", "hy", "relipart", "rb")


#entering data and computing covariance matrix from SDs and correlation matrix
# how to use summary stats 
mip.mns <- c(59.18, 19.84, 10.53, 12.65, 9.80, 6.35,
             9.53, 24.96, 19.89, 18.24, 2.68, .45,
             52.04, .11, 9.37, 10.36, 10.35, 9.93,
             30.33, 80.47, 2.67, 1.78, .47, 3.91, 70.58, 
             38.87, 24.34, 12.26, 11.80, 9.17, .81, 2.74)

mip.sds <- c(24.24, 9.00, 6.89, 5.22, 3.73, 3.05, 
             2.59, 3.87, 6.07, 7.48, 1.07, .81, 
            11.04, .318, 6.57, 2.65, 3.27, 2.45, 
            5.92, 18.12, .74, 1.14, .50, 8.05, 11.94,
            11.41, 6.83, 7.51, 5.51, 5.94, 1.24, .92)


mip.cor <- read.table("R script and data files/mip.dat", header=FALSE, row.names=names, col.names=names)
mip.cor <- data.matrix(mip.cor)

# transform the correlation matrix into the covariance matrix
mip.cov <- cor2cov(mip.cor,sds=mip.sds)


#Fitting initial SEM
# first specify the latent factors
mod.1 <-'#specifying measurement model portion
         ER =~ fa + fr + si
         IR =~ comt + con + cha + es + lo
         ERF =~ pro + th + time
         CPP =~ em + ind + ru + av + hy + rb
         PTG =~ ptgi1 + ptgi2 + ptgi3 + ptgi4 + ptgi5

         #specifying structural model portion
         ERF ~ ER + IR
         CPP ~ ERF
         PTG ~ ER + IR + CPP
        '

# lavaan assumes that you have raw data so sample.cov tells the pckg that you only have the covariance
fit.1 <- sem(mod.1, 
             sample.cov=mip.cov, 
             sample.mean=mip.mns, # optional if you don't have any mean structure, usualy in more advanced models
             sample.nobs=132, # tell it the sample size because it doesnt have the raw data
             meanstructure=TRUE, 
             std.lv=TRUE) # standardize the zetas of latent variables in the model BUT we'll look at fully stdzed output anyway

summary(fit.1, 
        fit.measures=TRUE, 
        estimates=FALSE) # first look at the fit and then look at p values etc

# the model fit is bad, inspect the mod. indices
modindices(fit.1, sort.=TRUE, minimum.value=10)
# lots of potential covariances (op ~~) -->
# =~ suggest crossloading

# lhs op   rhs     mi     epc sepc.lv sepc.all sepc.nox
# 368   ru ~~    hy 32.238  34.381  34.381    4.461    4.461
# 233 comt ~~   cha 27.844   2.684   2.684    0.546    0.546
# 348   em ~~   ind 21.340 -26.655 -26.655   -0.410   -0.410
# 183   fa ~~   ind 14.250  -7.574  -7.574   -0.333   -0.333
# 250  con ~~   cha 12.343  -2.215  -2.215   -0.381   -0.381
# 119   IR =~ ptgi5 11.877  -0.730  -0.730   -0.241   -0.241
# 256  con ~~    em 10.695  -8.717  -8.717   -0.304   -0.304
# 160  PTG =~   cha 10.321   0.590   0.688    0.282    0.282

# so maybe just need some correlation between indicators on the same measures
# items coming from the same scale/item bank type thing

# Another way to look at model misfit-->
#Fitting CFA model with all factors correlated to localize misfit 
# (p.83 in the pdf notes on R)
# everything covaries with everything else, find out if you do that does the model fit?
# saturated model of latent variables, if it still doesnt fit then it's the structural model that doesnt fit

mod.2 <-'#specifying measurement model
         ER =~ fa + fr + si
         IR =~ comt + con + cha + es + lo
         ERF =~ pro + th + time
         CPP =~ em + ind + ru + av + hy + rb
         PTG =~ ptgi1 + ptgi2 + ptgi3 + ptgi4 + ptgi5
        '
         
fit.2 <- sem(mod.2, 
             sample.cov=mip.cov, 
             sample.mean=mip.mns, 
             sample.nobs=132, 
             meanstructure=TRUE, 
             std.lv=TRUE)

# inspect the model fit, ignore the estimates
summary(fit.2, fit.measures=TRUE, estimates=FALSE)

# the model still fits poorly!
# formally test this via likelihood ration test (significant difference in the fit? -- not significant)
lavTestLRT(fit.2, fit.1)

# measurement model seems to be leading the misfit
# introduce the correlated residualts
# allow items on the same subscale to correlate -- see the bottom part of the model spec.-->
# see on page 86 to see what was done visually

#fitting revised SEM with correlated uniquenesses
mod.3 <-'#specifying measurement model portion
         ER =~ fa + fr + si
         IR =~ comt + con + cha + es + lo
         ERF =~ pro + th + time
         CPP =~ em + ind + ru + av + hy + rb
         PTG =~ ptgi1 + ptgi2 + ptgi3 + ptgi4 + ptgi5

         #specifying structural model portion
         ERF ~ ER + IR
         CPP ~ ERF
         PTG ~ ER + IR + CPP

         #correlating uniquenesses of same-scale items
         comt ~~ con + cha
         con ~~ cha
         em ~~ ind
         ru ~~ av + hy
         av ~~ hy
        '

fit.3 <- sem(mod.3, 
             sample.cov=mip.cov, 
             sample.mean=mip.mns, 
             sample.nobs=132, 
             meanstructure=TRUE, 
             std.lv=TRUE)

summary(fit.3, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

lavTestLRT(fit.3, fit.1)

#computing indirect effects of ERF, ER and IR on PTG for revised SEM
mod.3b <-'#specifying measurement model portion
          ER =~ fa + fr + si
          IR =~ comt + con + cha + es + lo
          ERF =~ pro + th + time
          CPP =~ em + ind + ru + av + hy + rb
          PTG =~ ptgi1 + ptgi2 + ptgi3 + ptgi4 + ptgi5

          #specifying structural model portion
          ERF ~ a1*ER + a2*IR
          CPP ~ b*ERF
          PTG ~  d1*ER + d2*IR + c*CPP

          #correlating uniquenesses of same-scale items
          comt ~~ con + cha
          con ~~ cha
          em ~~ ind
          ru ~~ av + hy
          av ~~ hy

          #effects of ERF
          dir_ERF := 0             
          ind_ERF := b*c          
          tot_ERF := 0 + b*c

          #effects of ER
          dir_ER := d1
          ind_ER := a1*b*c
          tot_ER := d1 + a1*b*c

          #effects of IR
          dir_IR := d2
          ind_IR := a2*b*c
          tot_IR := d2 + a2*b*c
         '

fit.3b <- sem(mod.3b, 
              sample.cov=mip.cov, 
              sample.mean=mip.mns, 
              sample.nobs=132, 
              meanstructure=TRUE, 
              std.lv=TRUE)

summary(fit.3b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
 
## semtools pckg described in the appendix shows how to 