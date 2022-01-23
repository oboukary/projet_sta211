data_model <- data_afm %>% 
  mutate(Formldéhyde= donnees_brutes$Formldéhyde)

res.mice <- mice(data = data_model , m=10, maxit = 5, method = 'pmm', seed = 500)
data_impute <- complete(res.mice,2)


