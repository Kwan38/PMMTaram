###### Commandes de bases pour utiliser la librairie RRegArch ########
# Ajouter la librairie
.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

r <- residualsSet("Normal")

v <- varSet(Tarch=list(ConstVar = 0.8, ArchPlus=c(0.4), ArchMinus=c(0.3)))
mod <- modelSet(condMean = NULL,condVar = v, condRes = r)

vInitPoint<- varSet(Tarch=list(ConstVar = 0.5, ArchPlus=c(0.3), ArchMinus=c(0.1)))
modInitPoint <- modelSet(condMean = NULL,condVar = vInitPoint, condRes = r)


ZZ1 <- RegArchSim(nSimul = 1000, model=mod)

# !!!! ERROR : CreateOneRealCondVar: unknown conditional var type !!!!
Res1 <- RegArchFit(model=mod, Yt=ZZ1$Yt,initPoint = modInitPoint)


summary(Res1)

