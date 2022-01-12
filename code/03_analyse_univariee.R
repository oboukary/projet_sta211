#-------------------------------------------------------------------------------
#      Analyse univarié ménage
#-------------------------------------------------------------------------------
summary(data_menage)
data_menage %>% 
  prettyR::describe(num.desc=c("mean","median","min","max","var","sd"))