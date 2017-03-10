###### Commandes de bases pour utiliser la librairie RRegArch ########
# Ajouter la librairie
.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

r <- residualsSet("Normal")

v <- varSet(Aparch=list(ConstVar=0.2, Arch=c(0.1), Gamma=c(0.4), Garch=c(0.4), Delta=2))
mod <- modelSet(condMean = NULL,condVar = v, condRes = r)


vInitPoint<- varSet(Aparch=list(ConstVar=0.1, Arch=c(0.2), Gamma=c(0.4), Garch=c(0.6), Delta=2))
modInitPoint <- modelSet(condMean = NULL,condVar = vInitPoint, condRes = r)


ZZ1 <- RegArchSim(nSimul = 1000, model=mod)

# !!!! ERROR : division by zero !!!!
Res1 <- RegArchFit(model=mod, Yt=ZZ1$Yt,initPoint = modInitPoint)
summary(Res1)

