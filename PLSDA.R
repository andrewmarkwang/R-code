library(mixOmics)
library(RVAideMemoire)

ASPGEN <- read.csv('C:/Users/think/Desktop/ASPGEN.txt',sep='\t')

X <- ASPGEN[,3:141]
X <- X[,which(colSums(X) > 0)]
Y <- c(rep('CKGEN',4),rep('ASPGEN',4))

PLSDA_ASPGEN <- plsda(X,Y,scale=F)
plotIndiv(PLSDA_ASPGEN)

PLSDA.VIP(PLSDA_ASPGEN)


ASPGEN <- read.csv('C:/Users/think/Desktop/PS.txt',sep='\t')

X <- ASPGEN[,3:141]
X <- X[,which(colSums(X) > 0)]
Y <- c(rep('CKGEN',6),rep('ASPGEN',5))

PLSDA_ASPGEN <- plsda(X,Y,scale=F)
plotIndiv(PLSDA_ASPGEN)

k<-PLSDA.VIP(PLSDA_ASPGEN)[[1]]
k <- k %>%
  mutate(Compound=rownames(k))

TNM<-'serif'
ggplot(k[c(1:15),],aes(x=VIP,y=factor(k[c(1:15),2],levels = k[c(15:1),2])))+
  geom_point(alpha=0.4,size=3)+
  labs(x="GeneRatio", y="Categories")+
  # add name to colour gradient legend
  scale_fill_continuous(name='pvalue')+

  # set the colour gradient 
  #scale_colour_gradient(low = "#132B43",high="#56B1F7",space="Lab",
  #                      na.value ="grey50",guide ="colourbar",aesthetics = "colour")+
  theme_bw()+
  theme(axis.title.x = element_text(family = TNM, face = "bold", size = 18),
        axis.title.y = element_text(family = TNM, face = "bold", size = 18),)+
  theme(plot.title = element_text(hjust = 0.5,family = TNM, face = "bold", size = 18), 
        legend.position="right", 
        legend.title = element_text(hjust = 0.5,family = TNM, face = "bold", size = 15),
        legend.text = element_text(hjust = 0.5,family = TNM, face = "bold", size = 12),
        axis.text.x=element_text(hjust = 0.5,family = TNM, face = "bold", size = 15),
        axis.text.y=element_text(family = TNM, face = "bold", size = 15),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white", size = 0.25),
        panel.border = element_rect(color="black", size=1.2, linetype="solid"),
        axis.ticks = element_line(colour = "black",size=1),
        text=element_text(hjust = 0.5,family = TNM, face = "bold", size = 15),
  )

