.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

#Nombre de Simu 
nbSimu = 10
N = 5 #Car c'est le modèle qu'on prend

ListModelsGSL = c('gsl_ConjugateFR',"gsl_ConjugatePR", "gsl_BFGS","gsl_BFGS2","gsl_Steepest","gsl_SimplexNM","gsl_SimplexNM2",
                  "gsl_SimplexNM2Rand")
#TODO ListModelsNLOpt


#Stockage Results

M <- matrix(nrow = length(ListModelsGSL),ncol = N,0)


# Créer le modèle
m1 <- meanSet(Ma = c(0.3,0.8),Const = 0.1)
v1 <- varSet(Arch = list(ConstVar = 0.4, Arch = c(0.5)))
r1 <- residualsSet('Normal')
mod1 <- modelSet(condMean = m1, condVar = v1, condRes = r1)

for(j in 1:nbSimu){
  
  # Faire une simu
  ZZ1 <- RegArchSim(nSimul = 1000, model=mod1)
  
  
  
  
  

  for(i in 1:length(ListModelsGSL)){
    print(ListModelsGSL[i])
    GSLAlgoParam <- setGSLParam(Algo = ListModelsGSL[i])
    # Fitter le modèle
    Res1 <- RegArchFit(model=mod1, Yt=ZZ1$Yt,initPoint = mod1, AlgoParam = GSLAlgoParam)
    summary(Res1)
    for(k in 1:N){
      M[i,k] =  M[i,k] + summary(Res1)$coef[k]
      
    }##On récupère tout ce qu'il y a à récupérer pour chaque modèle et on fait la moyenne
    
  }
  
}
#Moyenne pour chaque algo

for(i in 1:length(ListModelsGSL)){
  for(k in 1:N){
    M[i,k] = M[i,k]/nbSimu
  }
}

M





# Résumé sur le fit
#summary(Res1)