.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

# Créer le modèle
m1 <- meanSet(Ar = c(0.3,0.8),Const = 0.1)
m2 <- meanSet(VarInMean=1.0, nMa=3)
m3 <- meanSet(Ar = c(0.1,0.2), Ma = c(0.1))
v1 <- varSet(ConstVar = 0.1)
r1 <- residualsSet('Normal')

mod1 <- modelSet(condMean = m3, condVar = v1, condRes = r1)

# Faire une simu
ZZ1 <- RegArchSim(nSimul = 1000, model=mod1)

# Fitter le modèle
Res1 <- RegArchFit(model=mod1, Yt=ZZ1$Yt)

# Résumé sur le fit
summary(Res1)

