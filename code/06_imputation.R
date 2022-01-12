agg<- aggr(donnees_brutes, col=c('navyblue','red'))
imputed_Data <- mice(data = donnees, m=5, maxit = 5, method = 'pmm', seed = 500)
completeData <- complete(imputed_Data,2)