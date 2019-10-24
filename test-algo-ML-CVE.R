### Test des algo de ML pour le projet ML-CVE
### Jeremy L'Hour
### 24/10/2019

setwd('//Abra/current/CRT_PRJ_CVE_Machlearning/Hétérogéneité/')
rm(list=ls())

libraries = c('dplyr',
              'fastDummies',
              'DescTools',
              'Hmisc',
              'haven',
              'AER')

.libPaths(c("L:/R-3.4.3/library","W:/R/win-library/3.4"))
lapply(libraries, require, character.only = TRUE) 

# Opening df
data = as.data.frame(read_dta("data/old data/data_CVEOPP.dta"))

####################
####################
## Traitement etc ##
####################
####################

# proba
data[,"prob_OPP"] = data[,"pOPP_XcEMP"]
data[is.na(data[,"prob_OPP"]),"prob_OPP"] = 0
data[,"prob_CVE"] = data[,"pCVE_XcEMP"]
data[is.na(data[,"prob_CVE"]),"prob_CVE"] = 0
data[,"prob_CLA"] = data[,"pCLA_XcEMP"]


# assignation
data[,"Treatment_CVE"] = data[,"CVE"]
data[,"Treatment_OPP"] = data[,"OPP"]

# take-up
data[,"CVE_Ent"] = 1-is.na(data[,"date_acceptationCVE"])
data[,"OPP_Ent"] = 1-is.na(data[,"date_acceptationOPP"])

# autres variables importantes
data[,"FluxInd"] = data[,"FI"]
data[,"FluxNonInd"] = data[,"FNI"]
data[,"Stock"] = data[,"STOCK"]
data[,"ALE"] = data[,"ale"]

###########################
###########################
## Variables de controle ##
###########################
###########################

data[,'nivfor'] = car::recode(data$nivfor,"c('AGR','AU1','CN1','CPS','DCO','DE1','DEC','DES','DIG','DO','ICN','IEP','IGE','MA','MIA')='NV1'")
data[,'nivfor'] = car::recode(data$nivfor,"c('AU2','CA2','CN2','DCF','DE2','FP2','IUP','LIC','MAI','MS')='NV2'")
data[,'nivfor'] = car::recode(data$nivfor,"c('AU3','BTS','CA3','CN3','DE3','DEF','DEU','DPC','DUT','FP3','IPC')='NV3'")
data[,'nivfor'] = car::recode(data$nivfor,"c('A','AU4','B','BEI','BM','BP','BT','C','CA4','CDR','D','E','ES','ESE','F','FP4','G','H','L','PRO','S','SMS','STI','STL','STT')='NV4'")
data[,'nivfor'] = car::recode(data$nivfor,"c('AU5','BEP','CA5','CAP','FP5')='NV5'")
data[,'nivfor'] = as.factor(data[,'nivfor'])
data[,"dpt"] = as.numeric(substr(data[,"depcom"],1,2))
data[,"exper"] = as.numeric(data[,"exper"])


data[,"IdF"] = as.numeric(data[,"nregion"]=="116") # Ile de France
data[,"North"] = as.numeric(data[,"nregion"]=="311") # Nord
data[,"Otherregion"] = as.numeric(1 - data[,"IdF"] - data[,"North"])
data[,"Region"] = ifelse(data$nregion == '116', 'Idf', ifelse(data$nregion == '311', 'North', 'OtherRegion'))

data[,"EconLayoff"] = as.numeric(data[,"motins"]=="1")
data[,"PersLayoff"] = as.numeric(data[,"motins"]=="2")
data[,"EndCDD"] = as.numeric(data[,"motins"]=="4")
data[,"EndInterim"] = as.numeric(data[,"motins"]=="5")
data[,"OtherEnd"] = as.numeric(1 - data[,"EconLayoff"] - data[,"PersLayoff"] - data[,"EndCDD"] - data[,"EndInterim"])
data[,"RegistrationReason"] = ifelse(data$motins == '1', 'EconLayoff',
                                     ifelse(data$motins == '2', 'PersLayoff', 
                                            ifelse(data$motins == '4', 'EndCDD',
                                                   ifelse(data$motins == '5', 'EndInterim', 'OtherEnd'))))

data[,"Exper.cont"] = as.numeric(data[,"exper"])
data[,"exper.cont2"] = data[,"Exper.cont"]^2

data[,"exper0"] = as.numeric(data[,"exper"] == 0)
data[,"exper1_5"] = as.numeric((data[,"exper"] < 6 & data[,"exper"] > 0))
data[,"experM5"] = as.numeric(1 - data[,"exper0"] - data[,"exper1_5"])

data[,"rsqstat2"] = as.numeric(data[,"rsqstat"] == "RS2")
data[,"rsqstat3"] = as.numeric(data[,"rsqstat"] == "RS3")
data[,"Orsqstat"] = as.numeric(1 - data[,"rsqstat2"] - data[,"rsqstat3"])
data[,"Rsqstat"] = ifelse(data$rsqstat == 'RS2', 'rsqstat2',
                          ifelse(data$rsqstat == 'RS3', 'rsqstat3', 'Orsqstat'))

data[,"tempcomp"] =  as.numeric(data[,"temps"] == 1) # Search full-time position
data[,"Otemp"] = as.numeric(1 - data[,"tempcomp"]) # Does not search full-time position
data[,"TimeContract"] = ifelse(data$tempcomp == 1, 'FullTime', 'OFullTime')

data[,"LivesInZUS"] = as.numeric(data[,"zus"] == "ZU") # Zone Urbaine Sensible

data[,"Wage"] = data$salaire
data = fastDummies::dummy_cols(data,remove_first_dummy = F,select_columns = c('salaire'))

data[,"ce1"] = as.numeric(data[,"cemploi"] == "CE1") # Signification peu claire mais importante
data[,"ce2"] = as.numeric(data[,"cemploi"] == "CE2")
data[,"cemiss"] = as.numeric(data[,"cemploi"] == "")
data[,"CEmploi"] = ifelse(data$cemploi == 'CE1', 'CE1',
                          ifelse(data$cemploi == 'CE2', 'CE2', 'CEmiss'))

data[,"primo"] = as.numeric(data[,"ndem"] == 1) # First unemployment spell
data[,"SecSpell"] = as.numeric(data[,"ndem"] == 2) # Second unemployment spell
data[,"ThirdSpell"] = as.numeric(data[,"ndem"] == 3) # Third Unemployment spell
data[,"MoreSpell"] = as.numeric(data[,"ndem"] > 3) # 4 or more
data[,"NbSpells"] = data$ndem

data[,"ndem2"] = data[,"ndem"]^2

data$Qualif = ifelse(data$CS == 3, 'Cadre', 
                     ifelse(data$CS == 4, 'Techn',
                            ifelse(data$CS == 51, 'EmployQ',
                                   ifelse(data$CS == 56, 'EmployNQ',
                                          ifelse(data$CS == 61, 'OuvrQ',
                                                 ifelse(data$CS == 66 | data$CS == 99, 'OuvrNQ', NA))))))
data = fastDummies::dummy_cols(data,remove_first_dummy = F,select_columns = c('Qualif'))

data$Nationality = ifelse(((1-(data[,"nation"]<"31"))*(data[,"nation"]<="49")),'African',
                          ifelse((1-(data[,"nation"]<"90"))*(data[,"nation"]<="98") + (data[,"nation"] %in% c("24","27")), 'EasternEurope',
                                 ifelse(data[,"nation"] %in% c("02","03","14","19","21","22","25","26"), 'SouthEuropeTurkey',
                                        ifelse(data[,"etranger"] == 1, 'French', NA))))
data = fastDummies::dummy_cols(data,remove_first_dummy = F,select_columns = c('Nationality'))

data[,"nochild"] = as.numeric(data[,"nenf"] == 0)
data[,"onechild"] = as.numeric(data[,"nenf"] == 1)
data[,"twoormorechild"] = as.numeric(data[,"nenf"] > 1)
data[,"NbChildren"] = data[,"nenf"]

data[,"woman"] = as.numeric(data[,"sexe"] == 2)
data$Gender = ifelse(data$sexe == 2, 'Woman', 'Man')

data$Quarter = ifelse(data[,"mois_saisie_occ"] %in% 1:3, 'Q1',
                      ifelse(data[,"mois_saisie_occ"] %in% 4:6, 'Q2',
                             ifelse(data[,"mois_saisie_occ"] %in% 7:9, 'Q3',
                                    ifelse(data[,"mois_saisie_occ"] %in% 10:12, 'Q4', NA))))
data = fastDummies::dummy_cols(data,remove_first_dummy = F,select_columns = c('Quarter'))



data$Education = ifelse(data$nivetude1 == 1, 'College Education',
                        ifelse(data$nivetude2 == 1, 'High School',
                               ifelse(data$nivetude3 == 1, 'Vocational',
                                      ifelse(data$nivetude4 == 1, 'Other', NA))))

data$married = ifelse(data$sitmat == "M", 1, 0)

# Select control variables
list.controls  =  c("woman",
                    "married",
                    "onechild","twoormorechild",
                    "Nationality_French",
                    "IdF",
                    "nivetude1","nivetude3","nivetude4",
                    "Qualif_Cadre","Qualif_Techn","Qualif_EmployQ","Qualif_EmployNQ",
                    "agegr2635","agegr3645","agegr4655",
                    "ce1","ce2",
                    "EconLayoff","PersLayoff","EndCDD","EndInterim",
                    "rsqstat2","rsqstat3",
                    "tempcomp",
                    "LivesInZUS",
                    "salaire_A","salaire_B","salaire_C","salaire_D",
                    "primo","SecSpell",
                    "exper0","exper1_5")


# Outcomes
outcome_names = c("EMPLOI_3MOIS",
                  "EMPLOI_6MOIS",
                  "EMPLOI_12MOIS")



## Weight = empirical assignment probabilities by strata 

for (var in c('p1','p2','p3','p4')){
  data[[var]] = round(data[[var]],2)
}

# Indexes for rounded theoritical assignment probabilities
data$indx_FI_CVE = data %>% group_by(p2) %>% group_indices
data$indx_FI_CVEOPP = data %>% group_by(.dots=c("p1","p2")) %>% group_indices
data$indx_FI_OPP = data %>% group_by(p1) %>% group_indices
data$indx_FNI = data %>% group_by(p3) %>% group_indices
data$indx_ST = data %>% group_by(p4) %>% group_indices
# Indicators for each constant theoritical assignment probability (rounded)
data$assignment_index = ifelse(data$FluxInd == 1 & data$p2 > 0 & data$p1 ==0, paste0(data$indx_FI_CVE, '_FI_CVE'),
                               ifelse(data$FluxInd == 1 & data$p1 > 0 & data$p2 > 0, paste0(data$indx_FI_CVEOPP, '_FI_CVEOPP'),
                                      ifelse(data$FluxInd == 1 & data$p1 > 0 & data$p2 == 0, paste0(data$indx_FI_OPP, '_FI_OPP'),
                                             ifelse(data$Stock == 1, paste0(data$indx_ST, '_ST'),
                                                    ifelse(data$FluxNonInd,  paste0(data$indx_FNI, '_FNI'), NA)))))

# Empirical proba = in a strata: nb of treated / total nb of individuals in the strata
data = data %>%
  dplyr::group_by(assignment_index) %>%
  dplyr::mutate(probs_CVE = sum(Treatment_CVE)/n(),
                probs_OPP = sum(Treatment_OPP)/n()) 

# Deleting obs with prob = 0 or 1:
data$indic_delete_CVE = ifelse((data$p2 > 0 & data$p1 ==0) & (data$probs_CVE == 1 | data$probs_CVE == 0),1,0)
data$indic_delete_OPP = ifelse((data$p1 > 0 & data$p2 == 0) & (data$probs_OPP == 1 | data$probs_OPP == 0),1,0)
data$indic_delete_CVEOPP = ifelse((data$p1 > 0 & data$p2 > 0) & (data$probs_OPP == 1 | data$probs_OPP == 0),1,0)
data = data[data$indic_delete_CVE == 0 & data$indic_delete_CVEOPP == 0 & data$indic_delete_OPP == 0,]

# Final df
data = na.omit(data[data$FluxInd == 1,c('Treatment_CVE', 'Treatment_OPP',
                                        'CVE_Ent','OPP_Ent',
                                        'probs_CVE','probs_OPP',
                                        'Stock','FluxInd','FluxNonInd',
                                        'ALE','POIDSEMP_Z','POIDS_PZ_3MOIS',
                                        'POIDS_PZ_6MOIS', 'POIDS_PZ_12MOIS',
                                        outcome_names, 
                                        list.controls)])

data$Treatment_CLA = ifelse(data$Treatment_CVE == 0 & data$Treatment_OPP == 0, 1, 0)

################## SUB SAMPLING FOR ANALYSIS OF THE PUBLIC PROGRAM

# Subset to FI 
data_sub = data %>% dplyr::filter((Treatment_CVE == 1 | Treatment_CLA ==1) & probs_CVE > 0 & probs_OPP > 0)
data_sub[,"pi"] = data_sub[,"probs_CVE"] / (1-data_sub[,"probs_OPP"]) # proba randomisation (a ajuster)

#--------------------------------------------------------------------------------------------------------------------
#                                                     GENERIC ML
#--------------------------------------------------------------------------------------------------------------------


vec.pac = c("foreign","quantreg","gbm","glmnet",
            "doParallel","sandwich",
            "matrixStats","xtable","lfe","caret",
            "foreach", "multcomp","AER")

lapply(vec.pac, require, character.only = TRUE) 

ptm = proc.time()

####################################### Inputs  ######################################

sim     = 100     # number of repetitions
K       = 2       # number of folds
p       = 5       # number of groups 
thres   = 0.2     # quantile for most/least affected group
alpha   = 0.05    # significance level

#  dimension of these three vectors should match. If dimension is greater than 1 the program runs heterogeneity estimation separately for each outcome variable
names = c("Take_Up","Within 3 months","Within 6 months","Within 12 months")    # vector of labels for outcome variables
Y     = c("CVE_Ent","EMPLOI_3MOIS","EMPLOI_6MOIS","EMPLOI_12MOIS")     # vector of outcome variables
D     = rep("Treatment_CVE", length(Y))  # vector of treatment variables, en vrai Z pour nous
D.signal = c("CVE_Ent") # entree dans le traitement

### Puisque l'on a ajouté le CLAN avec des groupes formés sur le LATE, il faut nécessairement mettre l'entrée dans le traitement en première outcome.

# specify cluster, fixed effect and partition
cluster      = 0       # if no cluster       use    cluster      = "0"
fixed_effect = "ALE"       # if no fixed_effect  use    fixed_effect = "0"
partition    = "Treatment_CVE"       # if no partition     use    partition    = "0"

# create a vector of control variables
controls       = list.controls
affected       = c(list.controls, "CVE_Ent", "OPP_Ent","Stock","FluxInd","FluxNonInd")      # characteristics for most/least affected analysis (CLAN)
names_affected = c(list.controls, "CVE_Ent", "OPP_Ent","Stock","FluxInd","FluxNonInd")      # characteristics for most/least affected analysis

# generate formula for x, xl is for linear models
X = ""
for(i in 1:length(controls)){
  X = paste(X, controls[i], "+", sep = "")
}
X  = substr(X, 1, nchar(X)-1)
X_ind = ""
for(i in 1:length(list.controls)){
  X_ind = paste(X_ind, list.controls[i], "+", sep = "")
}
X_ind  = substr(X_ind, 1, nchar(X_ind)-1)


######################################################################################################

if(fixed_effect=="0" & cluster=="0"){
  data_sub = data_sub[,c(Y, D, controls,affected,"pi")]
}

if(fixed_effect=="0" & cluster!="0"){
  data_sub = data_sub[,c(Y, D,controls, cluster,affected, "pi")]
}

if(fixed_effect!="0" & cluster=="0"){
  data_sub = data_sub[,c(Y, D, controls, fixed_effect,affected, "pi")]
}

if(fixed_effect!="0" & cluster!="0"){
  data_sub = data_sub[,c(Y, D, controls, cluster, fixed_effect, affected,"pi")]
}

####################################### ML Inputs  #######################################

# svmPoly    : Support Vector Machines with Polynomial Kernel , package: kernlab, tuning parameters: degree (Polynomial Degree), scale (Scale), C (Cost)
# svmLinear  : Support Vector Machines with Linear Kernel , package: kernlab , C (Cost)
# svmLinear2 : Support Vector Machines with Linear Kernel , package: e1071 , cost (Cost)
# gbm        : Stochastic Gradient Boosting , package: gbm  , n.trees (# Boosting Iterations), interaction.depth (Max Tree Depth), shrinkage (Shrinkage), n.minobsinnode (Min. Terminal Node Size)
# glmnet     : Regularized Generalized Linear Models, package: glmnet , alpha (Mixing Percentage), lambda (Regularization Parameter)
# blackboost : Boosted Tree , package : mboost , mstop (#Trees), maxdepth (Max Tree Depth)
# nnet       : Neural Network , package : nnet  , size (#Hidden Units) , decay (Weight Decay)
# pcaNNet    : Neural Networks with Feature Extraction, package:nnet , size (#Hidden Units) , decay (Weight Decay)
# rpart      : CART, package:rpart, cp (Complexity Parameter)
# rf         : Random Forest,  package:randomForest, mtry (#Randomly Selected Predictors)


# Model names. For a list of available model names in caret package see: http://topepo.github.io/caret/available-models.html
# some available models given above

methods      = c("glmnet","svmLinear2","svmLinear3","xgbTree","pcaNNet","rf")
method_names = c("Elastic Net","SVM Simple","SVM-L2","Boosting","Neural Net","Random Forest")

# Tester SVM classif et regression??

# A list of arguments for models used in the estimation
args         = list(svmLinear2=list(type='eps-regression'), 
                    svmLinear=list(type='nu-svr'), 
                    svmPoly=list(type='nu-svr'), 
                    gbm=list(verbose=FALSE), 
                    xgbTree=list(verbose=FALSE),
                    glmnet = list(intercept = TRUE, family="binomial"),
                    rf=list(ntree=1000), 
                    gamboost=list(baselearner='btree'), avNNet=list(verbose = 0, linout = TRUE, trace = FALSE), 
                    pcaNNet=list(linout = TRUE, trace = FALSE, MaxNWts=100000, maxit=10000), 
                    nnet=list(linout = TRUE, trace = FALSE, MaxNWts=100000, maxit=10000))


methodML   = rep("cv",length(methods))
tune       = rep(20,length(methods))
proces     = rep("range",length(methods)) # pre-processing method
select     = rep("best",length(methods)) # optimality criteria for choosing tuning parameter in cross validation. available options: best, oneSE, tolerance
cv         = rep(5,length(methods))# the number of folds in cross-validation
rep        = rep(2,length(methods)) # number of iteration in repeated cross-validations


# If there is a parameter of the model that user doesn't want to choose with cross validation, it should be set using tune_param variable. Below mtry of random forest is set to 5 
# for glmnet we want to choose both tuning parameters using cross validation so it is set to NULL

tune_param       = list(0)
tune_param[[1]]  = 0
tune_param[[2]]  = 0
# tune_param[[3]]  = 0
# tune_param[[4]]  = data.frame(mtry=5)

set.seed(1211);

####################################### Test  #######################################

  
if(partition!="0"){
    ind = caret::createDataPartition(as.factor(data_sub[[partition]]), p = .5, list = FALSE)
    data_subuse_raw = as.data.frame(data_sub[ ind,])
    data_subout_raw = as.data.frame(data_sub[-ind,])
  }
  
if(partition=="0"){
    split             = runif(nrow(data_sub))
    cvgroup           = as.numeric(cut(split,quantile(split,probs = seq(0, 1, 1/K)),include.lowest = TRUE))  
    data_subuse_raw       = as.data.frame(data_sub[cvgroup == 1,])
    data_subout_raw       = as.data.frame(data_sub[cvgroup != 1,])  
}

### DEBUT DES SIMULATIONS

Results = array(dim=c(length(Y),length(methods),3))
  
for(i in 1:length(Y)){
  y      = Y[i]
  d      = D[i]
  
  data_subuse   = data.frame(data_subuse_raw[complete.cases(data_subuse_raw[, c(controls, affected, y, d, D.signal)]),])
  data_subout   = data.frame(data_subout_raw[complete.cases(data_subout_raw[, c(controls, affected, y, d, D.signal)]),])
  
  md_x          = data_subout[,'pi']
  ind           = c(md_x>0.01 & md_x<0.99) # only take people in the common support
  data_subout   = data_subout[ind, ]
  md_x          = c(md_x[ind])
  
  ind_u = which(data_subuse[,d]==1)         # treatment indicator
  
  for(l in 1:length(methods)){
    x = X
    if(tune_param[[l]]==0){ f = NULL}
    if(tune_param[[l]]!=0){ f = tune_param[[l]]}
    
    form           = as.formula(paste(y,"~",x,sep=""));
    
    ############ Estimate PROXY Scores using ML ############
    # From the auxiliary sample A, we obtain Machine Learning
    # estimates of the baseline and treatment effects, which we call
    # proxy scores
    
    fitControl   = trainControl(method = methodML[l], number = cv[l], repeats = rep[l], allowParallel = FALSE, verboseIter=TRUE, search="random", selectionFunction=select[l])
    arg          = c(list(form=form, data = data_subuse[ind_u,],  method = methods[l], tuneGrid = f, trControl = fitControl, preProcess=proces[l], tuneLength=tune[l]), args[[methods[l]]])
    fit.yz1      = suppressWarnings(do.call(caret::train, arg))
    my_z1x       = predict(fit.yz1, newdata=data_subout, type="raw")
    if(i == 1){
      my_z1x       = ifelse(my_z1x<0,0,my_z1x)
      my_z1x       = ifelse(my_z1x>1,1,my_z1x)
    }
    
    fitControl   = trainControl(method = methodML[l], number = cv[l], repeats = rep[l], allowParallel = FALSE, verboseIter=TRUE, search="random", selectionFunction=select[l])
    arg          = c(list(form=form, data = data_subuse[-ind_u,],  method = methods[l], tuneGrid = f, trControl = fitControl, preProcess=proces[l], tuneLength=tune[l]), args[[methods[l]]])
    fit.yz0      = suppressWarnings(do.call(caret::train, arg))
    my_z0x       = predict(fit.yz0, newdata=data_subout, type="raw")
    if(i == 1){
      my_z0x       = ifelse(my_z0x<0,0,my_z0x)
      my_z0x       = ifelse(my_z0x>1,1,my_z0x)        
    }
    
    # Save test error
    Results[i,l,1:2] = c(mean((data_subout[data_subout[,d]==1,y]-my_z1x)^2), 
                         mean((data_subout[data_subout[,d]==0,y]-my_z0x)^2))
    
    # Proxy scores
    B   = my_z0x # baseline
    S   = my_z1x - my_z0x # Individual TE
    
    
    ################################### Best Linear Prediction Regression  ################################### 
    
    Sd            = data_subout$S- mean(data_subout$S) # S - E(S)
    data_subout$S_ort = I((as.numeric(data_subout[,d])-md_x)*Sd) # Orthogonal centered S (S-E(S))*(D-p(Z))
    data_subout$d_ort = I((as.numeric(data_subout[,d])-md_x)) # Orthogonal D
    
    form1 = as.formula(paste(y, "~", "B+S+d_ort+S_ort| ", fixed_effect, "| 0 |", cluster, sep=""))
    reg = felm(form1, data=data_subout, weights=data_subout$weight)  
    
    # Save correlation with s_0(X)
    Results[i,l,3] <- abs(summary(reg)$coefficients['S_ort',1])*sqrt(var(data_subout$S))
    print(method_names[l])
    print(Results[i,l,])
  }  
}


  