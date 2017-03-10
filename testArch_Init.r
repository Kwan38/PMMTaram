## Test the convergence, computionnal time and cumulative RSS 
## In Arch model depending on the init point

.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

# List of GSL algo
algoGSL.list <- c("gsl_ConjugateFR","gsl_ConjugatePR", "gsl_BFGS","gsl_BFGS2","gsl_Steepest",
                  "gsl_SimplexNM","gsl_SimplexNM2","gsl_SimplexNM2Rand")
#Number of simulation
simuNb <- 10
# Residual set
r <- residualsSet('Normal')
# Vector of parameters and initPoints
paramArch <- c(0.5)
paramVectInit <- c(0.05,0.1,0.25,0.45,0.5,0.55,0.7,0.9)
# Mid result
res.errCumTmp <- 0
# Results
resListByMod <- list(distInitPoint = paramVectInit - paramArch, ErrSqCum = c(), compTime = c())
res <- list(ConjugateFR = resListByMod, ConjugatePR = resListByMod, BFGS = resListByMod,
            BFGS2 = resListByMod, Steepest = resListByMod, SimplexNM = resListByMod,
            SimplexNM2 = resListByMod, SimplexNM2Rand = resListByMod)

#Begin

print(paste("Test of model Arch with ", 1, " lags"))
# Creation of model Arch with 1 lags
var <- varSet(Arch = list(ConstVar = 0.1, Arch = paramArch))
mod <- modelSet(condMean = NULL, condVar = var, condRes = r)
for (n in 1:length(paramVectInit)) {
  print(paste("With init point ", paramVectInit[n], " against the true : ", paramArch))
  #Set the init point
  varInit <- varSet(Arch = list(ConstVar = 0.1, Arch = paramVectInit[n])) 
  modInit <- modelSet(condMean = NULL, condVar = varInit, condRes = r)
  # Initialize
  for (a in 1:length(algoGSL.list)) {
    res[[a]]$ErrSqCum[n] <- 0
    res[[a]]$compTime[n] <- 0
  }
  # Fit and compare by simu
  for (s in 1:simuNb) {
    print(paste("with simu : ", s))
    # Simulation
    arch.sim <- RegArchSim(1000,mod)
    # By algo
    for (a in 1:length(algoGSL.list)) {
      print(paste("and algo : ", algoGSL.list[a]))
      GSLAlgoParam <- setGSLParam(Algo = algoGSL.list[a])
      # Fit with this algo
      res.fit <- RegArchFit(model = mod, Yt = arch.sim$Yt, 
                            initPoint = modInit, AlgoParam = GSLAlgoParam)
      res.errCumTmp <- 0
      for (k in 2:2) {
        res.errCumTmp = res.errCumTmp + ((summary(res.fit))$coef[k] - paramArch[k-1])^2
      }
      res[[a]]$ErrSqCum[n] <- res[[a]]$ErrSqCum[n] + res.errCumTmp 
      res[[a]]$compTime[n] <- res[[a]]$compTime[n] + res.fit$GSLResult$ComputeTime
    }
  }
  # Mean for each result
  for (a in 1:length(algoGSL.list)) {
    res[[a]]$ErrSqCum[n] = res[[a]]$ErrSqCum[n] / simuNb
    res[[a]]$compTime[n] = res[[a]]$compTime[n] / simuNb
  }
}

#Display graph
colors <- c("black","lightblue", "darkblue", "lightgreen", "darkgreen", "red", "pink", "cyan")
# For computational time
plot(res$ConjugateFR$distInitPoint, res$ConjugateFR$compTime,
     main = "Time in function of init point distance by algo",
     xlab = paste("Init point distance to ", paramArch), ylab = "Time (s)",
     type = 'l', lwd = 2, ylim = c(0,1.5), col = colors[1])
for (i in 2:length(algoGSL.list)) {
  lines(res[[i]]$distInitPoint, res[[i]]$compTime, type = 'l', lwd = 2, col = colors[i])
}
legend("topleft", legend = names(res), col = colors, pch = 8)

# For error squared #Steepest has NaN
plot(res$ConjugateFR$distInitPoint, res$ConjugateFR$ErrSqCum,
     main = "Mean of cumulative error squared \nin function of init point by algo",
     xlab = paste("Init point distance to ", paramArch), ylab = "Mean of Cum. Error Squared",
     type = 'l', lwd = 2, ylim = c(0,0.008), col = colors[1])
for (i in 2:length(algoGSL.list)) {
  lines(res[[i]]$distInitPoint, res[[i]]$ErrSqCum, type = 'l', lwd = 2, col = colors[i])
}
legend("topright", legend = names(res), col = colors, pch = 8)


