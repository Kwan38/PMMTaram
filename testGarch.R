.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

# Créer le modèle
m1 <- NULL
v1 <- varSet(Arch = list(ConstVar = 2, GArch = c(0.7)))
r1 <- residualsSet('Normal')

mod1 <- modelSet(condMean = m1, condVar = v1, condRes = r1)

# Faire une simu
ZZ1 <- RegArchSim(nSimul = 1000, model=mod1)

# Fitter le modèle
Res1 <- RegArchFit(model=mod1, Yt=ZZ1$Yt)

# Résumé sur le fit
summary(Res1)
Res1