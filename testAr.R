###### Commandes de bases pour utiliser la librairie RRegArch ########
# Ajouter la librairie
.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

m1 <- meanSet(Ar = 0.5) #Must be < 1
v1 <- varSet(ConstVar = 0.5)

r1 <- residualsSet("Normal")

mod1 <- modelSet(condMean = m1, condVar = v1, condRes = r1)

# Faire une simu
ZZ1 <- RegArchSim(nSimul = 1000, model=mod1)
plot(ZZ1$Yt, type = 'l')

# Fitter le modèle
Res1 <- RegArchFit(model=mod1, Yt=ZZ1$Yt)