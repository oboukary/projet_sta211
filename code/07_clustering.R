#------------ Classification hierarchique ascendante ----------------#
# Nous réalisons une classification hiérarchique ascendante sur le jeu de données.
# Le but étant de voir s'il existe une structure en groupe au sein des ménages et 
# de calculer les groupes.
# Comme nous avons un jeu de données mixtes, nous allons utiliser l'indice de 
# similarité de Gower. L’objectif de cet indice consiste à mesurer dans quelle 
# mesure deux individus sont semblables. L’indice de Gower varie entre 0 et 1.
# Dans le package cluster, la fonction daisy permet de calculer la distance de Gower
# définie par : D_g = 1- S_g avec D_g la distance de Gower et S_g l'indice de similarité de Gower.
# Avec la distance de Gower, deux individus sont identiques si la distance de Gower vaut 0 et ils sont
# totalement différents si D_g vaut 1.
res.daisy<-daisy(data_afm, metric="gower")       # 
res.hc<-hclust(res.daisy, method  = "ward.D2")   #
plot(res.hc, labels=FALSE)  
# Avec la fonction fviz_nbclust nous déterminons le nombre optimal de class
dev.off()
fviz_nbclust(data_afm, kmeans, method = "wss") +
geom_vline(xintercept = 4, linetype = 2) + 
  xlab("Nombre de k") +
  ylab("Total de la somme des carrés intra") +
  ggtitle("Nombre optimal de cluster")
ggsave("graphiques/optimal_indiv_clusters.png")
# Selon la règle du coude, le nombre optimal de classes à considérer est
# 4. 
#------------------------------------------------------
# Nous coupons l'arbre issue de la classification hiérarchique 
# ascendante précédente en 4 classes.
nbclusters <- 4
rect.hclust(res.hc, k=nbclusters, border="red")  #
set.seed(123)
#--------- Consolidation des clusters avec du kmeans
# Après la CAH, nous réalisons du Kmeans pour consolider les groupes.
res.km <-kmeans(res.daisy, centers=nbclusters)
cluster.ind<- res.km$cluster
data_cluster<- as.data.frame(cbind(data_afm, cluster.ind))

#------------------- Visualisation des clusters --------------------------------
dev.off()
fviz_cluster(res.km, data = res.daisy,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(plot.title = element_text(hjust = 0.5)),
             main = "Graphique des groupes issus du kmeans") +
   theme(legend.title = element_blank())
ggsave("graphiques/indiv_groups.png")

#----------------- clustering des variables --------------------------------
# Mise en oeuvre du cluster var
# La méthode de classification des variables avec ClusterOfVar permet de créer
# des groupes de variables liées entre elles au sein des groupes et hétérogènes à 
# l'extérieur des groupes. L'avantage principale de cette méthode est qu'elle prend
# en compte des données mixtes. Le critère d’homogénéité d’une classe est la somme
# des carrés des corrélations (pour les variables quantitatives) et des rapports 
# de corrélations (pour les variables qualitatives) à une variable synthétique
# (quantitative) résumant au mieux les variables de la classe.
#  La variable synthétique qui maximise ce critère est la première composante 
#principale de l'AFDM.
# Deux algorithmes de classifications sont implémentés dans ce package:
# 1. Un algorithme de classificatiion hiérachique ascendante à travers la fonction
# hclustvar 
# 2. Un algorithme itératif de partitionnement 
cluster_var_tree <- hclustvar(X.quanti=data_quanti, X.quali=data_quali)
stab <- stability(cluster_var_tree, B=10)
plot(stab)
png("graphiques/stability_var_clust.png")
abline(v=c(7,8), col=c("blue", "red"), lty=c(1,2), lwd=c(1, 3))
rect.hclust(cluster_var_tree, k=8,border=2:9)
part<-cutreevar(cluster_var_tree,8)
#-------------------------------------------------------------------------------
# Groupe 1
var_clust1<-as.data.frame(data_afm[,names(part$var$cluster1[,1])]) 
dmy <-dummyVars(" ~ .", data=var_clust1, fullRank = T, levelsOnly = F)
var_clust1 <- data.frame(predict(dmy, newdata = var_clust1))
#-------------------------------------------------------------------------------
# Groupe 2
var_clust2<-as.data.frame(data_afm[,names(part$var$cluster2[,1])]) 
dmy <-dummyVars(" ~ .", data=var_clust2, fullRank = T, levelsOnly = F)
var_clust2 <- data.frame(predict(dmy, newdata = var_clust2))
#-------------------------------------------------------------------------------
# Groupe 3
var_clust3<-as.data.frame(data_afm[,names(part$var$cluster3[,1])]) 
dmy <-dummyVars(" ~ .", data=var_clust3, fullRank = T, levelsOnly = F)
var_clust3 <- data.frame(predict(dmy, newdata = var_clust3))
#-------------------------------------------------------------------------------
# Groupe 4
var_clust4<-as.data.frame(data_afm[,names(part$var$cluster4[,1])]) 
dmy <-dummyVars(" ~ .", data=var_clust4, fullRank = T, levelsOnly = F)
var_clust4 <- data.frame(predict(dmy, newdata = var_clust4))
#-------------------------------------------------------------------------------
# Groupe 5
var_clust5<-as.data.frame(data_afm[,names(part$var$cluster5[,1])]) 
dmy <-dummyVars(" ~ .", data=var_clust5, fullRank = T, levelsOnly = F)
var_clust5 <- data.frame(predict(dmy, newdata = var_clust5))
#-------------------------------------------------------------------------------
# Groupe 6
var_clust6<-as.data.frame(data_afm[,names(part$var$cluster6[,1])]) 
dmy <-dummyVars(" ~ .", data=var_clust6, fullRank = T, levelsOnly = F)
var_clust6 <- data.frame(predict(dmy, newdata = var_clust6))
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Groupe 7
var_clust7<-as.data.frame(data_afm[,names(part$var$cluster7[,1])]) 
dmy <-dummyVars(" ~ .", data=var_clust7, fullRank = T, levelsOnly = F)
var_clust7 <- data.frame(predict(dmy, newdata = var_clust7))
#-------------------------------------------------------------------------------
# Groupe 8
var_clust8<-as.data.frame(data_afm[,names(part$var$cluster8[,1])]) 
dmy <-dummyVars(" ~ .", data=var_clust8, fullRank = T, levelsOnly = F)
var_clust8 <- data.frame(predict(dmy, newdata = var_clust8))
#-------------------------------------------------------------------------------
data.clustofvar<-as.data.frame(cbind(var_clust1,var_clust2,var_clust3,
                       var_clust4, var_clust5, var_clust6,
                       var_clust7, var_clust8))
groups_cluster_of_vars<-c(rep(1, length(var_clust1)),
                          rep(2, length(var_clust2)),
                          rep(3, length(var_clust3)),
                          rep(4, length(var_clust4)),
                          rep(5, length(var_clust5)),
                          rep(6, length(var_clust6)),
                          rep(7, length(var_clust7)),
                          rep(8, length(var_clust8)) )

names(groups_cluster_of_vars)<- c(names(var_clust1),
                                  names(var_clust2),
                                  names(var_clust3),
                                  names(var_clust4),
                                  names(var_clust5),
                                  names(var_clust6),
                                  names(var_clust7),
                                  names(var_clust8)
                                  )
data.clustofvar$Formldéhyde <- data_impute$Formldéhyde
#-------------------------------------------------------------------------------




# Une autre méthode d'estimation du nombre de clusters consiste à utiliser un 
# graphique de "stabilité". Il s'agit d'un graphe qui "évalue la stabilité des 
# partitions obtenues à partir d'une hiérarchie de p variables"




