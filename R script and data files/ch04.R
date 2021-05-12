#Chapter 4 R script

#reading in data
hs <- read.table("R script and data files/hs.dat", header=FALSE)

names(hs) <- c("school", "female", "age", "month", 
               "visperc", "cubes", "lozenges", 
               "parcomp", "sencomp", "wordmean", 
               "addition", "countdot", "sccaps")

#rescaling indicator variables with widely discrepant scale from others
hs$addition <- hs$addition/4
hs$countdot <- hs$countdot/4
hs$sccaps <- hs$sccaps/4

# divided by 4 because that's what the authors did originally
# makes it easier to optimize the 

#loading lavaan package
library(lavaan)

# Specify the model
#CFA with standardized factors (this just means that the factors have mean of 0 and variance 1)

# latent factors are defined by =~ <- factor loading, on the left you type the factor,
# the indicators go on the right combined with +
# so the example has 3 factors (visual, verbal, speed) with no cross loadings, disturbance
# lavaan automatically assumes correlation between all latent factors! no need to specify visual ~~ verbal

cfa.1a <-'#factor loadings
          visual =~ visperc + cubes + lozenges
          verbal =~ parcomp + sencomp + wordmean
          speed =~ addition + countdot + sccaps
         '

# cfa function - can look at the defaults in the functio if want to know more
# default will not do meanstructure -- to see the means and intercepts even tho saturated in this case
# std.lv - 
# default of lavaan: set mean of factor to 0 and estimate variance, and fixes lamba to 1 
# so invoke std.lv TRUE --- will standardize the variance to 1 and estimate lambda instead

fit.1a <- cfa(cfa.1a, 
              data=hs, 
              meanstructure=TRUE, 
              std.lv=TRUE)

summary(fit.1a, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)
# the model was not a good fit
# output is organized by the factors - estimates are loadings
# column std.all are the fully standardized and useful for the interpretation
# covariances - std are correlations
# intercepts - means, of factors are fixed to zero (intercept has .)
# variances - residual variances for items have . before name, total variance does not have a dot (.visual)
# R square is communality h2

modindices(fit.1a, sort.=TRUE, minimum.value=10)


#CFA with scaling indicators
# scaling by indicators requires a bit more work ---see in the equation below
# lavaan automatically sets first lambda to 1, so only need to code 
# the fit will be exactly the same as the one above
# estimates have changed as the first lambda is fixed to 1, so all the estimates are sclaed differently
# but the fully standardized solution is all the same


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

### 
## Examine MIs
# lhs op      rhs     mi     epc sepc.lv sepc.all sepc.nox
# 42   visual =~   sccaps 36.411   4.672   4.672    0.515    0.515
# 88 addition ~~ countdot 34.145  15.423  15.423    0.859    0.859
# 40   visual =~ addition 18.631  -2.182  -2.182   -0.349   -0.349
# 90 countdot ~~   sccaps 14.946 -19.039 -19.039   -0.805   -0.805
# > 
# sepc all - standardized values you would find
# then add the scaps-->

#fitting modified model with cross-loading of sccaps on visual
cfa.2 <- '#factor loadings
          visual =~ visperc + cubes + lozenges + sccaps
          verbal =~ parcomp + sencomp + wordmean
          speed =~ addition + countdot + sccaps
         '
fit.2 <- cfa(cfa.2, data=hs, meanstructure=TRUE, std.lv=TRUE)
summary(fit.2, fit.measures=TRUE, standardized=TRUE, rsquare=TRUE)

lavTestLRT(fit.2, fit.1a) # <- inspect the model fit, compare previous fit to the new one

modindices(fit.2, sort.=TRUE, minimum.value=10)

## ALTERNATIVE
# take out the cross loading but ass the covar.--> (2nd suggestion in the MI table)
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
