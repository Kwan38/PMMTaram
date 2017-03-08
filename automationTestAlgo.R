.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

#Nombre de Simu 
nbSimu = 10
N = 5 #Car c'est le modèle qu'on prend

ListAlgosGSL = c('gsl_ConjugateFR',"gsl_ConjugatePR", "gsl_BFGS","gsl_BFGS2","gsl_Steepest","gsl_SimplexNM","gsl_SimplexNM2",
                  "gsl_SimplexNM2Rand")


#ListAlgosGSL = c('gsl_ConjugateFR')

#TODO ListModelsNLOpt


#Stockage Results

M <- matrix(nrow = length(ListAlgosGSL),ncol = N,0)

computingTime <- vector(length = length(ListAlgosGSL))

ratioConv <- vector(length = length(ListAlgosGSL))

#Les modèles à tester sont : 
# Arch / Garch / Tarch / Egarch / Aparch / Figarch / Ugarch / 


# Créer le modèle
m2 <- meanSet(Ma = c(0.7,0.1),Const = 0.8)
v2 <- varSet(Arch = list(ConstVar = 0.2, Arch = c(0.1)))
r2 <- residualsSet('Normal')
mod2 <- modelSet(condMean = m2, condVar = v2, condRes = r2)




m1 <- meanSet(Ma = c(0.3,0.8),Const = 0.1)
v1 <- varSet(Arch = list(ConstVar = 0.4, Arch = c(0.5)))
r1 <- residualsSet('Normal')
mod1 <- modelSet(condMean = m1, condVar = v1, condRes = r1)

Param <- vector(length = N); 
Param[1] = 0.3
Param[2] = 0.8
Param[3] = 0.1
Param[4] = 0.4
Param[5] = 0.5

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

      
    }
    if(Res1$GSLResult$Convergence == TRUE){
      
      ratioConv[i] = ratioConv[i] + 1
      
    }
    
    ##On récupère tout ce qu'il y a à récupérer pour chaque modèle et on fait la moyenne
    computingTime[i] = computingTime[i] + Res1$GSLResult$ComputeTime
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
print("Matrice d'erreur quadratiques moyenne des modèles")
print(M)
print("Vecteur de temps d'execution moyen des modèles")
print(computingTime)





# Résumé sur le fit
#summary(Res1)