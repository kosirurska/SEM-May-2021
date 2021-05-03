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

#loading packages
library(lavaan)
library(semPlot)

#CFA with standardized factors
cfa.1a <-'#factor loadings
          visual =~ visperc + cubes + lozenges
          verbal =~ parcomp + sencomp + wordmean
          speed =~ addition + countdot + sccaps
         '
fit.1a <- cfa(cfa.1a, data=hs, meanstructure=TRUE, std.lv=TRUE)

#generating path diagrams
semPaths(fit.1a)
semPaths(fit.1a, whatLabels="est", intercepts="FALSE", curve=1.75)
semPaths(fit.1a, whatLabels="std", intercepts="FALSE", curve=1.75)
