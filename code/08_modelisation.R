#-------------------------------------------------------------------------------
#            JEU DE TEST ET JEU D'APPRENTISSAGE
#-------------------------------------------------------------------------------
set.seed(123)
y <- completeData$Formldéhyde
X <- completeData %>% dplyr::select(-c(Formldéhyde))
train_index<- createDataPartition(y, p=0.7, list = FALSE)
X_train <- X[train_index,]
X_test  <- X[-train_index,]
y_train <- y[train_index,]
y_test  <- y[-train_index,]