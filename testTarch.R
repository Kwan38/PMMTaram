###### Commandes de bases pour utiliser la librairie RRegArch ########
# Ajouter la librairie
.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)


v3 <- varSet(Tarch=list(ConstVar = 0.8, ArchPlus=c(0.4), ArchMinus=c(0.3)))
mod3 <- modelSet(condMean = NULL,condVar = v3, condRes = r1)

v3InitPoint<- varSet(Tarch=list(ConstVar = 0.5, ArchPlus=c(0.3), ArchMinus=c(0.1)))
mod3InitPoint <- modelSet(condMean = NULL,condVar = v3InitPoint, condRes = r1)


ZZ1 <- RegArchSim(nSimul = 1000, model=mod3)

Res1 <- RegArchFit(model=mod3, Yt=ZZ1$Yt,initPoint = mod3InitPoint)
summary(Res1)

