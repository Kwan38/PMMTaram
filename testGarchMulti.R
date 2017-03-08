.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

#Nombre de Simu 
nbSimu = 3
N = 2
  
ListAlgosGSL = c('gsl_ConjugateFR',"gsl_ConjugatePR", "gsl_BFGS","gsl_BFGS2","gsl_Steepest","gsl_SimplexNM","gsl_SimplexNM2",
                 "gsl_SimplexNM2Rand")


ListResultat = list(computingTime = c(), quadraticError = c())

ListResult = list(gsl_ConjugateFR = ListResultat , gsl_ConjugatePR = ListResultat, gsl_BFGS = ListResultat)




#ListAlgosGSL = c('gsl_ConjugateFR')

#TODO ListModelsNLOpt


#Stockage Results


#On test pour un vecteur de taille 


Models <- c()
InitModels <- c()

Params <- c(0.1,0.5)
ParamInit <-c(0.3,0.7)


computingTime <- vector(length = length(ListAlgosGSL))

tmp =1
mod = c()
initMod = c()

for(z in 1:N){
  
  for(i in 1:2){
    
    ListResult[[i]]$computingTime[z] = 0
    ListResult[[i]]$quadraticError[z] = 0
    
  }
  
}


for(z in 1:N){

  m11 <- NULL
  v11 <- varSet(Garch = list(ConstVar = 0.1, Garch = Params[1:z], Arch = c(0)))
  r11 <- residualsSet('Normal')
  mod <- modelSet(condMean = m11, condVar = v11, condRes = r11)
  
  
  v12 <- varSet(Garch = list(ConstVar = 0.1,Arch = c(0),Garch = ParamInit[1:z]))
  r12 <- residualsSet('Normal')
  initMod <- modelSet(condMean = m11, condVar = v12, condRes = r12)
  
  
  M <- matrix(nrow = length(ListAlgosGSL),ncol = 2+z,0)
  
  
  for(j in 1:nbSimu){
    
    # Faire une simu
    ZZ1 <- RegArchSim(nSimul = 1000, model=mod)
    
    for(i in 1:length(ListAlgosGSL)){
      print(ListAlgosGSL[i])
      GSLAlgoParam <- setGSLParam(Algo = ListAlgosGSL[i])
      # Fitter le modèle
      Res1 <- RegArchFit(model=mod, Yt=ZZ1$Yt,initPoint = initMod, AlgoParam = GSLAlgoParam)
      summary(Res1)
      for(k in 2:(z+1)){
        ListResult[[i]]$quadraticError[z] = ListResult[[i]]$quadraticError[z] + (summary(Res1)$coef[k] - (Params[k-1]))^2
      }
      
      ##On récupère tout ce qu'il y a à récupérer pour chaque modèle et on fait la moyenne
      ListResult[[i]]$computingTime[z] = ListResult[[i]]$computingTime[z] + Res1$GSLResult$ComputeTime
    }
    
  }
}  
  
#Moyenne pour chaque algo
for(z in 1:N){
  
  
  for(i in 1:length(ListAlgosGSL)){
    for(k in 1:N){
      (ListResult[[i]]$quadraticError)[k] = (ListResult[[i]]$quadraticError)[k]/nbSimu
    }
    (ListResult[[i]]$quadraticError)[z] = (ListResult[[i]]$quadraticError)[z]/nbSimu
  } 
  
  
}
print("Matrice d'erreur quadratiques des modèles")
print(M)
print("Vecteur de temps d'execution moyen des modèles")
print(computingTime)





# Résumé sur le fit
#summary(Res1)