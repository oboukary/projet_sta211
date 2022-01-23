#-------------------------------------------------------------------------------
#            JEU DE TEST ET JEU D'APPRENTISSAGE
#-------------------------------------------------------------------------------
set.seed(123)
data_cluster <- data_cluster %>% 
  mutate(Formldéhyde = donnees_brutes$Formldéhyde)
dmy <- dummyVars(" ~ .", data = data_cluster, fullRank = T)
dat_transformed <- data.frame(predict(dmy, newdata = data_cluster))
y<- completeData$Formldéhyde
train_index<- createDataPartition(y, p=0.7, list = FALSE)
traindata <- completeData[train_index,]
testdata  <- completeData[-train_index,]
X_train<-traindata %>% dplyr::select(-c(Formldéhyde))
y_train<- traindata$Formldéhyde
X_test <- testdata %>% dplyr::select(-c(Formldéhyde))
y_test<- testdata$Formldéhyde


screen_c <- screen_control(keep_fraction = .25,
                           ntree_factor = 1,
                           min_ntree = 250)
select_c <- select_control(number_selected = 10,
                           ntree_factor = 1,
                           min_ntree = 250)

module_membership <- cluster.ind


ff_fit <- ff(X_train, y_train, module_membership = groups,
             screen_params = screen_c,
             select_params = select_c,
             final_ntree = 250)
