###### Commandes de bases pour utiliser la librairie RRegArch ########
# Ajouter la librairie
.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

r <- residualsSet("Normal")

# !!!! ERROR : could not find "egarchSet" !!!! 
v <- varSet(Egarch=list(ConstVar=0, Arch=c(0.1), Garch=c(0.6), Teta=0.2, Gamma=0.1))
mod <- modelSet(condMean = NULL,condVar = v, condRes = r)

vInitPoint<- varSet(Egarch=list(ConstVar=0, Arch=c(0.2), Garch=c(0.5), Teta=0.2, Gamma=0.2))
modInitPoint <- modelSet(condMean = NULL,condVar = vInitPoint, condRes = r)


ZZ1 <- RegArchSim(nSimul = 1000, model=mod)

Res1 <- RegArchFit(model=mod, Yt=ZZ1$Yt,initPoint = modInitPoint)
summary(Res1)

