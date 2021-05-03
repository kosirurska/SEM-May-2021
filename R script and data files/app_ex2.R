#reading in data, assigning variable names
classes <- rep("numeric",12)
anti <- read.table("anti.dat", header=FALSE, colClasses=classes, na.strings=".")
y <- paste("anti", 6:14, sep = "")
names(anti) <- c(c("id","gen","homecog"),y)

#loading packages
library(lavaan)
library(semPlot)

#fitting linear model with gen and homecog as predictors of intercepts and slopes
lcm <- 'I =~ 1*anti6 + 1*anti7 + 1*anti8 + 1*anti9 +
             1*anti10 + 1*anti11 + 1*anti12 + 1*anti13 +
             1*anti14 
        S =~ 0*anti6 + 1*anti7 + 2*anti8 + 3*anti9 +
             4*anti10 + 5*anti11 + 6*anti12 + 7*anti13 +
             8*anti14 
        I ~ gen + homecog
        S ~ gen + homecog
       '
fit.lcm <- growth(lcm, data=anti, missing="ML")
summary(fit.lcm)

#generating path diagram
semPaths(fit.lcm, whatLabels="est", intercepts="FALSE")
