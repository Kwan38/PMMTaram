###### Commandes de bases pour utiliser la librairie RRegArch ########
# Ajouter la librairie
.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

r <- residualsSet("Normal")

v <- varSet(Ugarch=list(ExistConstBool=TRUE, Beta=c(0.5), ConstVar=0.3, Arch=c(0.7), Garch=c(0.8)))
mod <- modelSet(condMean = NULL,condVar = v, condRes = r)


vInitPoint<- varSet(Ugarch=list(ExistConstBool=FALSE, Beta=c(0.3), ConstVar=0.1, Arch=c(0.5), Garch=c(0.6)))
modInitPoint <- modelSet(condMean = NULL,condVar = vInitPoint, condRes = r)

# !!!! ERROR : bad index !!!!
ZZ1 <- RegArchSim(nSimul = 1000, model=mod)

Res1 <- RegArchFit(model=mod, Yt=ZZ1$Yt,initPoint = modInitPoint)
summary(Res1)

