.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

#Nombre de Simu 
nbSimu = 10
NbModels = 3

ListAlgosGSL = c('gsl_ConjugateFR',"gsl_ConjugatePR", "gsl_BFGS","gsl_BFGS2","gsl_Steepest","gsl_SimplexNM","gsl_SimplexNM2",
                 "gsl_SimplexNM2Rand")
#TODO ListModelsNLOpt


#Les modèles à tester sont : 
# Arch / Garch / Tarch / Egarch / Aparch / Figarch / Ugarch / 

#ListModels
#ARCH
v1 <- varSet(Arch = list(ConstVar = 0.4, Arch = c(0.5)))
r1 <- residualsSet('Normal')
mod1 <- modelSet(condMean = NULL,condVar = v1, condRes = r1)

v1InitPoint <- varSet(Arch = list(ConstVar = 0.7, Arch = c(0.8)))
mod1InitPoint <- modelSet(condMean = NULL,condVar = v1InitPoint, condRes = r1)

NArch = 2

#GARCH
v2 <- varSet(Garch = list(ConstVar = 0.6, Garch = c(0.5), Arch = c(0.4)))
mod2 <- modelSet(condMean = NULL,condVar = v2, condRes = r1)

v2InitPoint<- varSet(Garch = list(ConstVar = 0.2, Garch = c(0.3), Arch = c(0.6)))
mod2InitPoint <- modelSet(condMean = NULL,condVar = v2InitPoint, condRes = r1)

NGarch = 3

#EGARCH : error on egarchset function [RESOLVED]
v3 <- varSet(Egarch=list(ConstVar=0.2, Arch=c(0.4), Garch=c(0.6), Teta=0.2, Gamma=0.1))
mod3 <- modelSet(condMean = NULL,condVar = v3, condRes = r1)

v3InitPoint <- varSet(Egarch=list(ConstVar=0.3, Arch=c(0.2), Garch=c(0.8), Teta=0.1, Gamma=0.3))
mod3InitPoint <- modelSet(condMean = NULL,condVar = v3InitPoint, condRes = r1)

NEgarch = 5

ListModels = list(mod1,mod2,mod3)
ListModelsInitPoint = list(mod1InitPoint,mod2InitPoint,mod3InitPoint)

#Stockage des parametres pour les différents modeles poour l'erreur quadratique
NbParam = c(NArch,NGarch,NEgarch)

NbParamMax = max(NbParam)

Param <- matrix(nrow = NbModels, ncol = NbParamMax)

#Model 1 : ARCH
Param[1,1] = 0.4
Param[1,2] = 0.5
Param[1,3] = 0.0 
#Model 2 : GARCH
Param[2,1] = 0.6
Param[2,2] = 0.5
Param[2,3] = 0.4
#Model 3 : EGARCH
Param[3,1] = 0.2
Param[3,2] = 0.4
Param[3,3] = 0.6
Param[3,4] = 0.2
Param[3,5] = 0.1
  

#Liste des matrices de résultats d'erreur quadratique pour chaque modele sur chacun des algo
m1 <- matrix(nrow = length(ListAlgosGSL),ncol = NArch,0)
m2 <- matrix(nrow = length(ListAlgosGSL),ncol = NGarch,0)
m3 <- matrix(nrow = length(ListAlgosGSL),ncol = NEgarch,0)
ListMatrixError = list(m1,m2,m3)


#Matrice de résulats des temps d'exécution pour chaque modele sur chacun des algo
computingTime <- matrix(nrow = NbModels, ncol = length(ListAlgosGSL), 0)

#Matrice de résulats des ratio de convergence pour chaque modele sur chacun des algo
ratioConv <- matrix(nrow = NbModels, ncol = length(ListAlgosGSL), 0)

modelNames = c("ARCH", "GARCH", "EGARCH")
#Pour chacun des modèles (3)
for (m in 1:NbModels) {
    
  print("BOUCLE SUR LE MODELE : " )
  print(modelNames[m])
  
  #Stockage Results
  M <- matrix(nrow = length(ListAlgosGSL),ncol = NbParam[m],0)

  #Pour chaque simu
  for(j in 1:nbSimu){
    
    # Faire une simu
    ZZ1 <- RegArchSim(nSimul = 1000, model=ListModels[[m]])
    
    #On teste tous les algo
    for(i in 1:length(ListAlgosGSL)){
      print(ListAlgosGSL[i])
      GSLAlgoParam <- setGSLParam(Algo = ListAlgosGSL[i])
      # Fitter le modèle
      Res1 <- RegArchFit(model=ListModels[[m]], Yt=ZZ1$Yt,initPoint = ListModelsInitPoint[[m]], AlgoParam = GSLAlgoParam)
      summary(Res1)
      for(k in 1:NbParam[m]){
        M[i,k] =  M[i,k] + (summary(Res1)$coef[k] - Param[m,k])^2
        
        
      }
      ##On récupère tout ce qu'il y a à récupérer pour chaque modèle et on fait la moyenne
      computingTime[m,i] = computingTime[m,i] + Res1$GSLResult$ComputeTime
      print(Res1$GSLResult$ComputeTime)
      print(paste("Simulation ",j))
      if(Res1$GSLResult$Convergence == TRUE){
        
        ratioConv[m,i] = ratioConv[m,i] + 1
        
      }
    }
    
  }
  
  #Moyenne pour chaque algo
  for(i in 1:length(ListAlgosGSL)){
    for(k in 1:NbParam[m]){
      M[i,k] = M[i,k]/nbSimu
    }
    computingTime[m,i] = computingTime[m,i]/nbSimu
    ratioConv[m,i] = ratioConv[m,i] / nbSimu
  }
  
  #On stocke la matrice d'erreur quad pour chacun des modèles
  ListMatrixError[[m]] = M
}
