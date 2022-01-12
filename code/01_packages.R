
liste_des_packages <- c("readxl", "readr","tidyverse", "reshape","cowplot" ,"cramer","vcd", 
                        "data.table", "DT","stringr","reshape2", "dplyr","gtsummary","plotly",
                        "qwraps2","tidyverse", "reshape","cowplot" ,"cramer","vcd",
                        "data.table", "DT","stringr","reshape2", "dplyr","gtsummary",
                        "FactoMineR","factoextra","caret","ade4","pretty","hrbrthemes",
                        "viridis", "forcats","faraway","corrplot","kableExtra","papeR",
                        "qwraps2", "prettyR","webshot", "tidyquant","GGally", "gridExtra",
                        "ggcorrplot","khroma", "patchwork","ggsci", "missMDA", "mice", "VIM",
                        "Amelia")
#Installer les package s'ils ne sont pas encore installés
instal <- liste_des_packages %in% installed.packages()
install.packages(liste_des_packages[!instal],repos='http://cran.us.r-project.org')

# Chargement des librairies
lapply(liste_des_packages, require, character.only=TRUE)