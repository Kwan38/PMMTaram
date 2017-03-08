.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

#Nombre de Simu 
nbSimu = 100
N = 3 #Car c'est le modèle qu'on prend

ListAlgosGSL = c('gsl_ConjugateFR',"gsl_ConjugatePR", "gsl_BFGS","gsl_BFGS2","gsl_Steepest","gsl_SimplexNM","gsl_SimplexNM2",
                 "gsl_SimplexNM2Rand")
#TODO ListModelsNLOpt


#Utiliser ça pour savoi le taux de convergence !!!!!!!!!
#Res1$GSLResult$Convergence

#Stockage Results

M <- matrix(nrow = length(ListAlgosGSL),ncol = N,0)

computingTime <- vector(length = length(ListAlgosGSL))

ratioConv <- vector(length = length(ListAlgosGSL))
#Les modèles à tester sont : 
# Arch / Garch / Tarch / Egarch / Aparch / Figarch / Ugarch / 


# Créer le modèle
m1 <- NULL
v1 <- varSet(Arch = list(ConstVar = 0.4, Arch = c(0.5)))

#m2 <- meanSet(Ma = c(0.2,-0.5),Const = 0.3)
v2 <- varSet(Arch = list(ConstVar = 0.1, Arch = c(0.8)))
r1 <- residualsSet('Normal')
mod1 <- modelSet(condMean = m1,condVar = v1, condRes = r1)

mod2 <- modelSet(condMean = m1,condVar = v2, condRes = r1)



Param <- vector(length = N); 
Param[1] = 0.1
Param[2] = 0.4
Param[3] = 0.5

for(j in 1:nbSimu){
  
  # Faire une simu
  ZZ1 <- RegArchSim(nSimul = 1000, model=mod1)
  
  for(i in 1:length(ListAlgosGSL)){
    print(ListAlgosGSL[i])
    GSLAlgoParam <- setGSLParam(Algo = ListAlgosGSL[i])
    # Fitter le modèle
    Res1 <- RegArchFit(model=mod1, Yt=ZZ1$Yt,initPoint = mod2, AlgoParam = GSLAlgoParam)
    summary(Res1)
    for(k in 1:N){
      M[i,k] =  M[i,k] + (summary(Res1)$coef[k] - Param[k])^2
      
      
    }##On récupère tout ce qu'il y a à récupérer pour chaque modèle et on fait la moyenne
    computingTime[i] = computingTime[i] + Res1$GSLResult$ComputeTime
    if(Res1$GSLResult$Convergence == TRUE){
      
      ratioConv[i] = ratioConv[i] + 1
      
    }
  }
  
}
#Moyenne pour chaque algo

for(i in 1:length(ListAlgosGSL)){
  for(k in 1:N){
    M[i,k] = M[i,k]/nbSimu
  }
  computingTime[i] = computingTime[i]/nbSimu
  ratioConv[i] = ratioConv[i] / nbSimu
}
print("Matrice d'erreur quadratiques des modèles")
M
print("Vecteur de temps d'execution moyen des modèles")
computingTime





# Résumé sur le fit
#summary(Res1)

