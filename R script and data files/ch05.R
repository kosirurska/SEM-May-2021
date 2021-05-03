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


mip.cor <- read.table("mip.dat", header=FALSE, row.names=names, col.names=names)
mip.cor <- data.matrix(mip.cor)
mip.cov <- cor2cov(mip.cor,sds=mip.sds)


#Fitting initial SEM
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

fit.1 <- sem(mod.1, sample.cov=mip.cov, sample.mean=mip.mns, sample.nobs=132, meanstructure=TRUE, std.lv=TRUE)
summary(fit.1, fit.measures=TRUE, estimates=FALSE)

modindices(fit.1, sort.=TRUE, minimum.value=10)


#Fitting CFA model with all factors correlated to localize misfit
mod.2 <-'#specifying measurement model
         ER =~ fa + fr + si
         IR =~ comt + con + cha + es + lo
         ERF =~ pro + th + time
         CPP =~ em + ind + ru + av + hy + rb
         PTG =~ ptgi1 + ptgi2 + ptgi3 + ptgi4 + ptgi5
        '
         
fit.2 <- sem(mod.2, sample.cov=mip.cov, sample.mean=mip.mns, sample.nobs=132, meanstructure=TRUE, std.lv=TRUE)
summary(fit.2, fit.measures=TRUE, estimates=FALSE)

lavTestLRT(fit.2, fit.1)


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

fit.3 <- sem(mod.3, sample.cov=mip.cov, sample.mean=mip.mns, sample.nobs=132, meanstructure=TRUE, std.lv=TRUE)
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

fit.3b <- sem(mod.3b, sample.cov=mip.cov, sample.mean=mip.mns, sample.nobs=132, meanstructure=TRUE, std.lv=TRUE)
summary(fit.3b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
 
