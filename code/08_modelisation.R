#-------------------------------------------------------------------------------
#            JEU DE TEST ET JEU D'APPRENTISSAGE
#-------------------------------------------------------------------------------
seed=123
set.seed(seed)
#-- Discrétisation des variables qualitatives
dmy <- dummyVars(" ~ .", data = data_afm, fullRank = T, levelsOnly = F)
data_model<- data.frame(predict(dmy, newdata = data_afm))
#-- Création d'une liste contenant les six blocs de vairables du jeu de données
groups <- c(rep(1,21), rep(2,44), rep(3,38), rep(4,33), rep(5,5), rep(6,27))
names(groups)<- names(data_afm)
data_dummy <- data_model
data_dummy$Formldéhyde<- data_impute$Formldéhyde
#--- Echantillonnage du jeu de données en train et test
train_index<- createDataPartition(y, p=0.7, list = FALSE)
traindata <- data_dummy[train_index,]
testdata  <- data_dummy[-train_index,]
X_train<-traindata %>% dplyr::select(-c(Formldéhyde))
y_train<- traindata$Formldéhyde
X_test <- testdata %>% dplyr::select(-c(Formldéhyde))
y_test<- testdata$Formldéhyde
#------------------------------------------------------------------------------
#  Modélisation avec Fuzzy forest
#-------------------------------------------------------------------------------
#21, 65, 98,  136, 141, 168
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

ff_fit <- ff(X_train, y_train, module_membership = module_membership,
             screen_params = screen_params, select_params=select_params,
             final_ntree = 500)
dev.off()
modplot(ff_fit)
varImp(ff_fit$final_rf)
final_rf <- ff_fit$final_rf
final_rf_mse <- tail(final_rf$mse, 1)

#-------------------------------------------------------------------------------
#            Estimation avec WGCNA
#-------------------------------------------------------------------------------
WGCNA_params <- WGCNA_control(p = 6, minModuleSize = 1, nThreads = 1)
wff_fit <- wff(X_train, y_train, WGCNA_params = WGCNA_params,
               screen_params = screen_params,
               select_params = select_params,
               final_ntree = final_ntree,
               num_processors = 1, nodesize = nodesize)

#-------------------------------------------------------------------------------
#  RANDOM FOREST CLASSIQUE AVEC TUNING DES PARAMETRES
#-------------------------------------------------------------------------------
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
# Dans cette partie, nous allons créer un nouveau modèle "customRF" basé sur 
# l'algorithme random Forest avec pour objectif de définir un certains nombre de
# paramètre à tuner au travers du package caret
#-------------------------------------------------------------------------------
customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree,
               maxnodes=param$maxnodes, nodesize = param$nodesize, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
custom <- train(Formldéhyde~., data=data_dummy, 
                method=customRF, 
                tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)