donnees_brutes <- read.table(file = "Donnees_CNL.csv",
                             header=TRUE, 
                             sep=";",
                             fileEncoding="latin1",
                             dec = ",") 

donnees<-donnees_brutes 
col_names<- names(donnees)
cat_cols <- col_names[ c(23:46, 80:116, 123:129)] # Numero des colonnes categorielles
num_cols <- col_names[-c(23:46, 80:116, 123:129)] # colonnes numériques
#------------------------------------------------------------------------------------
#  Transformation des variables categorielles en facteur et des variables numériques
#  en numérique
donnees <- donnees %>% 
  mutate_at(cat_cols, funs(as.character(.))) %>% 
  mutate_at(cat_cols, funs( as.factor(.))) %>%
  mutate_at(num_cols, funs(as.numeric(.)))
#----- Extraire la partie du nom des colonnes après le point ------------------
#names(donnees) <- sub(".*\\.", "", names(donnees))
#------------------------------- habitude---------------------------------------
data_habitude <- donnees %>% 
  dplyr::select(c(1:46)) %>% 
  dplyr::select(-c("TYPEHAB","ACTIVITE"))
quali_habitude <- data_habitude %>%  # données variables qualitatives habitude
  dplyr::select_if(~is.factor(.))
quanti_habitude <- data_habitude %>%  # données variables quantitatives habitude
  dplyr::select_if(~is.numeric(.))
#-------------------------------logement----------------------------------------
# - Les variables KVNT2e112  et KVNT2e12 n'ont qu'une seule modalités donc
#   on les supprime.
#   On supprime également TYPELOG qui une variable issue d'une ACM
data_logement <- donnees %>% 
  dplyr::select(c(47:117)) %>% 
  dplyr::select(-c("TYPELOG","KVNT2e112","KVNT2e12")) 
quali_logement <- data_logement %>%  # données variables qualitatives logement
  dplyr::select_if(~is.factor(.))
quanti_logement <- data_logement %>%  # données variables quantitatives logement
  dplyr::select_if(~is.numeric(.))
#-------------------------------ménage------------------------------------------
data_menage <- donnees %>%
  dplyr::select(c(118:129))%>% 
  dplyr::select(-c("TYPEMEN"))
quali_menage <- data_menage %>%    # données variables qualitatives ménage
  dplyr::select_if(~is.factor(.))
quanti_menage <- data_menage %>% 
  dplyr::select_if(~is.numeric(.)) # données variables quantitatives ménage
#-------------------------------------------------------------------------------
data_afm<- donnees %>% 
  select(-c("TYPEMEN","TYPEHAB", "ACTIVITE", "TYPELOG","Formldéhyde","KVNT2e12","KVNT2e112"))

donnees_impute<-donnees_brutes %>% 
  dplyr::select(-c("TYPEMEN","TYPEHAB", "ACTIVITE", "TYPELOG","KVNT2e12","KVNT2e112"))

#----- Selection des variables quantitatives du jeu de données ------*/
data_quanti <- data_afm %>% 
  dplyr::select_if(is.numeric)
#----- Selection des variables categorielles ----*/
data_quali <- data_afm %>% 
  dplyr::select_if(is.factor)
