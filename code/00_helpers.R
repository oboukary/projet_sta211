#-------------------------------------------------------------------------------
# fonction screeplot
#------------------------------------------------------------------------------

graphes_factorielle<- function(res){
  data<-res$eig
  eig_max<-max(data[,2])
  p <- fviz_eig(res, addlabels=TRUE,
                barfill="darkblue", barcolor ="darkblue",
                linecolor ="red",
                ylab = "Pourcentage de variances expliquées")  + 
    ylim(0, eig_max+1)
    theme_tq()
  p
  if(class(res)[1]=="PCA"){
    fviz_pca(res)
  }
    else if(class(res)[1] == "MCA"){
      fviz_mca(res)
    }
   else if(class(res)[1] == "MFA"){
     fviz_mfa(res)
   }
    else if(class(res)[1] == "FAMD"){
      fviz_famd(res)
    }
    else if (class(res)[1]=="HMFA"){
      fviz_hmfa(res)
    }
}



