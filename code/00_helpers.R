#-------------------------------------------------------------------------------
# fonction screeplot
#------------------------------------------------------------------------------

resume<- function(res){
  data<-res$eig
  eig.val <- get_eigenvalue(res)
  head(eig.val)
  eig_max<-max(data[,2])
  p <- fviz_eig(res, addlabels=TRUE,
                barfill="darkblue", barcolor ="darkblue",
                linecolor ="red",
                ylab = "Pourcentage de variances expliquées")  + 
    ylim(0, eig_max+1)
    theme_tq()
  p
  if(class(res)[1]=="PCA"){
    fviz_pca_var(res, col.var = "cos2",
                 gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                 repel = TRUE # Évite le chevauchement de texte
    )
    
    fviz_pca_ind (res, col.ind = "cos2",
                  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                  repel = TRUE # Évite le chevauchement de texte
    )
    
    fviz_pca_biplot(res, repel = TRUE,
                    col.var = "#2E9FDF", # Couleur des variables
                    col.ind = "#696969"  # Couleur des individues
                  
    )
  }
    else if(class(res)[1] == "MCA"){
      fviz_mca(res)
    }
   else if(class(res)[1] == "MFA"){
     fviz_mfa(res)
     fviz_screeplot(res) 
   }
    else if(class(res)[1] == "FAMD"){
      fviz_famd(res)
      
    }
    else if (class(res)[1]=="HMFA"){
      fviz_hmfa(res)
    }
}



