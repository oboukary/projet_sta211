data_cluster <- data_cluster %>% 
  mutate(Formldéhyde= donnees_brutes$Formldéhyde)

res.mice <- mice(data = data_cluster, m=10, maxit = 5, method = 'pmm', seed = 500)
completeData <- complete(res.mice,2)


