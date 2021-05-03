#Chapter 4 R script

#reading in data
hs <- read.table("hs.dat", header=FALSE)
names(hs) <- c("school", "female", "age", "month", 
               "visperc", "cubes", "lozenges", 
               "parcomp", "sencomp", "wordmean", 
               "addition", "countdot", "sccaps")

#rescaling indicator variables with widely discrepant scale from others
hs$addition <- hs$addition/4
hs$countdot <- hs$countdot/4
hs$sccaps <- hs$sccaps/4

#loading lavaan package
library(lavaan)


#CFA with standardized factors
cfa.1a <-'#factor loadings
          visual =~ visperc + cubes + lozenges
          verbal =~ parcomp + sencomp + wordmean
          speed =~ addition + countdot + sccaps
         '

fit.1a <- cfa(cfa.1a, data=hs, meanstructure=TRUE, std.lv=TRUE)
summary(fit.1a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

modindices(fit.1a, sort.=TRUE, minimum.value=10)


#CFA with scaling indicators
cfa.1b <-'#factor loadings
          visual =~ visperc + cubes + lozenges
          verbal =~ parcomp + sencomp + wordmean
          speed =~ addition + countdot + sccaps
     
          #means/intercepts
          visual ~ 1
          verbal ~ 1
          speed ~ 1
          visperc ~ 0*1
          parcomp ~ 0*1
          addition ~ 0*1
         '

fit.1b <- cfa(cfa.1b, data=hs, meanstructure=TRUE)
summary(fit.1b, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)


#fitting modified model with cross-loading of sccaps on visual
cfa.2 <- '#factor loadings
          visual =~ visperc + cubes + lozenges + sccaps
          verbal =~ parcomp + sencomp + wordmean
          speed =~ addition + countdot + sccaps
         '
fit.2 <- cfa(cfa.2, data=hs, meanstructure=TRUE, std.lv=TRUE)
summary(fit.2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

lavTestLRT(fit.2, fit.1a)

modindices(fit.2, sort.=TRUE, minimum.value=10)


#fitting alternative modified model with residual covariance between addition and countdot
cfa.3 <- '#factor loadings
          visual =~ visperc + cubes + lozenges 
          verbal =~ parcomp + sencomp + wordmean
          speed =~ addition + countdot + sccaps

          #covariances
          addition ~~ countdot
         '
fit.3 <- cfa(cfa.3, data=hs, meanstructure=TRUE, std.lv=TRUE)
summary(fit.3, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

lavTestLRT(fit.3, fit.1a)

modindices(fit.3, sort.=TRUE, minimum.value=10)
