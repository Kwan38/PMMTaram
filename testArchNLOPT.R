.libPaths("/user/1/taram/Public/RLib/")
library(RRegArch)

#Nombre de Simu 
nbSimu = 10
N = 2 #Car c'est le modèle qu'on prend

#Computing Time = 0 pour tous les NLOPT sauf : 

# 'nlopt_ld_lbfgs_nocedal', 'nlopt_ld_lbfgs', 'nlopt_ln_praxis', 
#'nlopt_ld_var1', 'nlopt_ld_var2', 'nlopt_ld_tnewton', 'nlopt_ld_tnewton_restart',
#'nlopt_ld_tnewton_precond', 'nlopt_ln_neldermead','nlopt_ld_auglag_eq'
# 'nlopt_g_mlsl_lds'


#Division by zero 

# 'nlopt_gn_direct','nlopt_gn_direct_l', 'nlopt_gn_direct_l_rand', 'nlopt_gn_direct_noscal', 'nlopt_gn_direct_l_noscal', 
# 'nlopt_gn_direct_l_rand_noscal', 'nlopt_gn_orig_direct',

# Tres Bon 

#nlopt_ld_lbfgs_nocedal','nlopt_g_mlsl_lds','nlopt_ld_tnewton', 'nlopt_ld_tnewton_restart','nlopt_ld_tnewton_precond'

#NA
#'nlopt_ln_cobyla',

#Long 

#'nlopt_ln_newuoa_bound',


#Tres Long 

#'nlopt_ld_tnewton_precond_restart', 'nlopt_gn_crs2_lm','nlopt_gn_mlsl', 'nlopt_gd_mlsl', 'nlopt_gn_mlsl_lds','nlopt_gd_mlsl_lds', 
#'nlopt_ld_mma',  'nlopt_ln_newuoa', 'nlopt_ln_sbplx', 'nlopt_ln_auglag', 'nlopt_ld_auglag', 'nlopt_ln_auglag_eq',
#''nlopt_ln_bobyqa',  'nlopt_ld_slsqp',  'nlopt_ld_ccsaq',


ListAlgosNLOPT = c('nlopt_gn_orig_direct_l', 'nlopt_gd_stogo', 'nlopt_gd_stogo_rand', 
                   'nlopt_ld_lbfgs_nocedal', 'nlopt_ld_lbfgs', 'nlopt_ln_praxis', 
                   'nlopt_ld_var1', 'nlopt_ld_var2', 'nlopt_ld_tnewton', 'nlopt_ld_tnewton_restart',
                   'nlopt_ld_tnewton_precond', 'nlopt_ln_neldermead','nlopt_ld_auglag_eq',
                   'nlopt_gn_isres', 'nlopt_auglag', 'nlopt_auglag_eq', 'nlopt_g_mlsl','nlopt_g_mlsl_lds','nlopt_gn_esch', 'nlopt_num_algorithms')

#TODO ListModelsNLOpt


#Stockage Results

M <- matrix(nrow = length(ListAlgosNLOPT),ncol = N,0)

computingTime <- vector(length = length(ListAlgosNLOPT))

ratioConv <- vector(length = length(ListAlgosNLOPT))

#Les modèles à tester sont : 
# Arch / Garch / Tarch / Egarch / Aparch / Figarch / Ugarch / 


# Créer le modèle
m2 <- NULL
v2 <- varSet(Arch = list(ConstVar = 0.2, Arch = c(0.1)))
r2 <- residualsSet('Normal')
mod2 <- modelSet(condMean = m2, condVar = v2, condRes = r2)




m1 <- NULL
v1 <- varSet(Arch = list(ConstVar = 0.4, Arch = c(0.5)))
r1 <- residualsSet('Normal')
mod1 <- modelSet(condMean = m1, condVar = v1, condRes = r1)

Param <- vector(length = N); 

Param[1] = 0.4
Param[2] = 0.5

for(j in 1:nbSimu){
  
  # Faire une simu
  ZZ1 <- RegArchSim(nSimul = 1000, model=mod1)
  
  for(i in 1:length(ListAlgosNLOPT)){
    print(ListAlgosNLOPT[i])
    NLOPTAlgoParam <- setNLOPTParam(Algo = ListAlgosNLOPT[i])
    # Fitter le modèle
    Res1 <- RegArchFit(model=mod1, Yt=ZZ1$Yt,initPoint = mod2, AlgoParam = NLOPTAlgoParam)
    for(k in 1:N){
      M[i,k] =  M[i,k] + (summary(Res1)$coef[k] - Param[k])^2
      
      
    }
    if(Res1$NLOPTResult$Convergence == TRUE){
      
      ratioConv[i] = ratioConv[i] + 1
      
    }
    
    ##On récupère tout ce qu'il y a à récupérer pour chaque modèle et on fait la moyenne
    computingTime[i] = computingTime[i] + Res1$NLOPTResult$ComputeTime
  }
  
}
#Moyenne pour chaque algo

for(i in 1:length(ListAlgosNLOPT)){
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


#[,1]        [,2]
#[1,] 0.040000000 0.160000000
#[2,] 0.040000000 0.160000000
#[3,] 0.040000000 0.160000000
#[4,] 0.001026148 0.002184268
#[5,] 0.040000000 0.160000000
#[6,] 0.001027281 0.002213418
#[7,] 0.001027281 0.002213418
#[8,] 0.001026379 0.002185192
#[9,] 0.001026379 0.002185199
#[10,] 0.001026382 0.002185192
#[11,] 0.001026378 0.002185196
#[12,] 0.040000000 0.160000000
#[13,] 0.040000000 0.160000000
#[14,] 0.040000000 0.160000000
#[15,] 0.040000000 0.160000000
#[16,] 0.040000000 0.160000000
#[17,] 0.040000000 0.160000000
#[18,] 0.001028533 0.002192487
#[19,] 0.040000000 0.160000000
#[20,] 0.040000000 0.160000000

# 0.000 0.000 0.000 0.050 0.062 0.047 0.047 0.039 0.060 0.035 0.059 0.048 0.010 0.000 0.000 0.000 0.000 0.078 0.000 0.000

# Résumé sur le fit
#summary(Res1)