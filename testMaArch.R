.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

# Créer le modèle
m1 <- meanSet(Ma = c(0.3,0.8),Const = 0.1)
v1 <- varSet(Arch = list(ConstVar = 0.4, Arch = c(0.5)))
r1 <- residualsSet('Normal')

mod1 <- modelSet(condMean = m1, condVar = v1, condRes = r1)

# Faire une simu
ZZ1 <- RegArchSim(nSimul = 1000, model=mod1)


#Les différents algos sont : 
# "gsl_ConjugateFR",
#"gsl_ConjugatePR", 
#"gsl_BFGS",
#"gsl_BFGS2",
#"gsl_Steepest",
#"gsl_SimplexNM",
#"gsl_SimplexNM2",
#"gsl_SimplexNM2Rand".


GSLAlgoParam <- setGSLParam(Algo = "gsl_ConjugatePR")
# Fitter le modèle
Res1 <- RegArchFit(model=mod1, Yt=ZZ1$Yt,initPoint = mod1, AlgoParam = GSLAlgoParam)

# Résumé sur le fit
summary(Res1)