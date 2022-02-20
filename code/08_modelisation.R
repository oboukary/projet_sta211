#-------------------------------------------------------------------------------
#     RANDOM FOREST ET FUZZY FOREST SUR LES BLOC INITIAUX
#-------------------------------------------------------------------------------
seed=123
set.seed(seed)
#-- Discrétisation des variables qualitatives
dmy <- dummyVars(" ~ .", data = data_afm, fullRank = T, levelsOnly = F)
data_model<- data.frame(predict(dmy, newdata = data_afm))
#-- Création d'une liste contenant les six blocs de variables du jeu de données
# initiales
groups <- c(rep(1,21), rep(2,44), rep(3,38), rep(4,33), rep(5,5), rep(6,27))
names(groups)<- names(data_model)
data_dummy <- data_model
data_dummy$Formldéhyde<- data_impute$Formldéhyde
#--- Échantillonnage du jeu de données en train et test
train_index<- createDataPartition(y, p=0.7, list = FALSE)
traindata <- data_dummy[train_index,]
testdata  <- data_dummy[-train_index,]
X_train<-traindata %>% dplyr::select(-c(Formldéhyde))
y_train<- traindata$Formldéhyde
X_test <- testdata %>% dplyr::select(-c(Formldéhyde))
y_test<- testdata$Formldéhyde
#------ Modélisation avec Fuzzy forest
mtry_factor   <- 1; 
min_ntree     <- 500;  
drop_fraction <- .5; 
ntree_factor  <- 1
nodesize      <- 1; 
final_ntree   <- 500
screen_params <- screen_control(drop_fraction = drop_fraction,
                                keep_fraction = .25, min_ntree = min_ntree,
                                ntree_factor = ntree_factor,
                                mtry_factor = mtry_factor)
select_params <- select_control(drop_fraction = drop_fraction,
                                number_selected = 5,
                                min_ntree = min_ntree,
                                ntree_factor = ntree_factor,
                                mtry_factor = mtry_factor)

ff_fit <- ff(X_train, y_train, 
             module_membership = groups,
             screen_params = screen_params, 
             select_params=select_params,
             final_ntree = final_ntree)
dev.off()
modplot(ff_fit)
varImp(ff_fit$final_rf)
final_rf <- ff_fit$final_rf
final_rf_mse <- tail(final_rf$mse, 1)
#------- RANDOM FOREST CLASSIQUE AVEC TUNING DES PARAMETRES
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3, 
                        search='grid')
#create tunegrid with 15 values from 1:15 for mtry to tunning model. Our train function will change number of entry variable at each split according to tunegrid. 
tunegrid <- expand.grid(.mtry = (1:15)) 

rf_gridsearch <- train(Formldéhyde ~ ., 
                       data = data_dummy,
                       method = 'rf',
                       metric = 'Rmse',
                       tuneGrid = tunegrid)
print(rf_gridsearch)
varImpPlot(rf_gridsearch$finalModel)
#------------------------------------------------------------------------------
#  FUZZY FOREST SUR LES RESULTATS DE ClusterOfvar
#------------------------------------------------------------------------------
y <- data.clustofvar$Formldéhyde
#--- Échantillonnage du jeu de données en train et test
train_index<- createDataPartition(y, p=0.7, list = FALSE)
traindata <- data.clustofvar[train_index,]
testdata  <- data.clustofvar[-train_index,]
X_train<-traindata %>% dplyr::select(-c(Formldéhyde))
y_train<- traindata$Formldéhyde
X_test <- testdata %>% dplyr::select(-c(Formldéhyde))
y_test<- testdata$Formldéhyde


#------ Modélisation avec Fuzzy forest


mtry_factor   <- 1; 
min_ntree     <- 500;  
drop_fraction <- .5; 
ntree_factor  <- 1
nodesize      <- 1; 
final_ntree   <- 5000
screen_params <- screen_control(drop_fraction = drop_fraction,
                                keep_fraction = .25, min_ntree = min_ntree,
                                ntree_factor = ntree_factor,
                                mtry_factor = mtry_factor)
select_params <- select_control(drop_fraction = drop_fraction,
                                min_ntree = min_ntree,
                                ntree_factor = ntree_factor,
                                mtry_factor = mtry_factor)

ff_fit <- ff(X_train, y_train, module_membership = groups_cluster_of_vars,
             screen_params = screen_params, select_params=select_params,
             final_ntree = final_ntree)
dev.off()
modplot(ff_fit)
varImp(ff_fit$final_rf)
final_rf <- ff_fit$final_rf
final_rf_mse <- tail(final_rf$mse, 1)

