data_impute <- data_afm %>% 
  mutate(Formldéhyde= donnees_brutes$Formldéhyde)

res.mice <- mice(data = data_impute , m=10, maxit = 5, method = 'pmm', seed = 500)
data_impute <- complete(res.mice,2)


