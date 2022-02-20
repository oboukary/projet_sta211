corr<-cor(quanti_menage)
melted<-melt(corr)
ggplot(data = melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(midpoint = 0.5, mid ="grey70", 
                       limits = c(-1, +1)) +
  labs(title = "", 
       x = "", y = "", fill = "Mesure de corrélation") +
  theme(plot.title = element_text(hjust = 0.5, colour = "blue"), 
        axis.title.x = element_text(face="bold", colour="darkgreen", size = 12),
        axis.title.y = element_text(face="bold", colour="darkgreen", size = 12),
        legend.title = element_text(face="bold", colour="brown", size = 10)) +
  geom_text(aes(x = Var1, y = Var2, label = round(value, 2)), color = "black", 
            fontface = "bold", size = 5)
ggsave("graphiques/corrplot.png")