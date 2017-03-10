###### Commandes de bases pour utiliser la librairie RRegArch ########
# Ajouter la librairie
.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

r <- residualsSet("Normal")

v <- varSet(Figarch=list(ConstVar=0.5, Arch=c(0.6), Garch=c(0.3), FracD=1))
mod <- modelSet(condMean = NULL,condVar = v, condRes = r)


vInitPoint<- varSet(Figarch=list(ConstVar=0.2, Arch=c(0.3), Garch=c(0.1), FracD=1))

modInitPoint <- modelSet(condMean = NULL,condVar = vInitPoint, condRes = r)

# !!!! ERROR : Session R aborted every time we try a simulation
ZZ1 <- RegArchSim(nSimul = 1000, model=mod)

Res1 <- RegArchFit(model=mod, Yt=ZZ1$Yt,initPoint = modInitPoint)
summary(Res1)

