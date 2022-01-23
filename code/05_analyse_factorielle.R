#------------------------------------------------------------------------#
#  ACP, ACM, AFM SUR LES DONNEES MENAGES                                 #
#------------------------------------------------------------------------#
# ACP sur les variables quantitatives du bloc ménage
res.pca_menage<-PCA(quanti_meange)
summary(res.pca_menage)
resume(res.pca_menage)
# ACM sur les variables qualitatives du bloc ménage
res.mca_menage<-MCA(quali_menage)
summary(res.mca_menage)
resume(res.mca_menage)
# AFM sur les variables du bloc ménage
res.mfa_menage<-MFA(data_menage, 
                    group=c(5,6),
                    type=c(rep("s",1),rep("n",1)),
                    ncp=5,
                    name.group=c("Quali ménage","Quanti ménage"))
summary(res.mfa_menage)
resume(res.mfa_menage)
#-------------------------------------------------------------------#
#  ACP, ACM, AFM SUR LES DONNEES HABITUDES                          #
#-------------------------------------------------------------------#
# ACP sur les variables quantitatives du bloc habitude
res.pca_habitude<-PCA(quanti_habitude)
summary(res.pca_habitude)
# ACM sur les variables qualitatives du bloc habitude
res.mca_habitude<-MCA(quali_habitude)
summary(res.mca_habitude)
# AFM sur les variables du bloc habitude
res.mfa_habitude<-MFA(data_habitude, 
                    group=c(21, 23),
                    type=c(rep("s",1),rep("n",1)),
                    ncp=5,
                    name.group=c("Quali habitude","Quanti habitude"))
summary(res.mfa_habitude)
resume(res.mfa_habitude)
#---------------------------------------------------------------------#
#  ACP, ACM, AFM SUR LES DONNEES LOGEMENT                             #
#---------------------------------------------------------------------#
# ACP sur les variables quantitatives du bloc logement
res.pca_logement<-PCA(quanti_logement)
summary(res.pca_logement)
# ACM sur les variables qualitatives du bloc logement
res.mca_logement<-MCA(quali_logement)
summary(res.mca_logement)
# AFM sur les variables du bloc logement
res.mfa_logement<-MFA(data_logement, 
                    group=c(33, 35),
                    type=c(rep("s",1),rep("n",1)),
                    ncp=5,
                    name.group=c("Quali logement","Quanti logement"))
summary(res.mfa_logement)
resume(res.mfa_logement)


#---------------------------------------------------------------------#
#  ACP, ACM, AFM SUR LES DONNEES GLOBALES                             #
#---------------------------------------------------------------------#
res_afm_global<-MFA(data_afm,
                    group=c(21, 23,33, 35, 5, 6),
                    type =c("s","n","s","n","s","n"),
                    ncp=100,
                    name.group=c("Quali_habitude","Quanti_habitude",
                                 "Quali_logement","Quanti_logement",
                                 "Quali_menage","Quanti_menage"))

resume(res_afm_global)

#--------------------------------------------------------------------
#  Kmeans 
#-------------------------------------------------------------------


