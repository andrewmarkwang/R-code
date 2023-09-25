###Venn plot###

A <- sample(1:1000, 400, replace = F )
B <- sample(1:1000, 600, replace = F )
C <- sample(1:1000, 350, replace = F )
D <- sample(1:1000, 550, replace = F )
E <- sample(1:1000, 375, replace = F )

library(VennDiagram)
p<-venn.diagram(
  x = list(
    'A(400)' = A,
    'B(600)' = B,
    'C(350)' = C,
    'D(550)' = D,
    'E(375)' = E
  ),
  filename = 'VN.png',
  col = "black",
  fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
  alpha = 0.5,
  cex = 0.8,
  cat.col = 'black',
  cat.cex = 0.8,
  cat.fontface = "bold",
  margin = 0.05,
  main = "Complex Venn Diagram",
  main.cex = 1.2
)

###Heatmap###
library(pheatmap)
a<-seq (from = 1,to = 20,by = 2)
b<-sample(1:10,10,replace = F )
c<-sample(10:20,10,replace=F)
df<-data.frame(cbind(a,b,c))
rownames(df)<-c('a','b','c','d','e','f','g','h','i','j')
colnames(df)<-c('s1','s2','s3')
pheatmap(df,fontsize_row  = 15,fontsize_col  = 15,cluster_rows = FALSE, cluster_cols = FALSE,fontfamily= "Times New Roman")
