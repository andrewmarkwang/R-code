
library(devtools)
library(Hmisc)
library(igraph)
library(tidyverse)
library(Rmisc) 
library(corrplot)
library(ggcorrplot)
library(RColorBrewer)
library(grDevices)
library(reshape2)
library(xlsx)

#tot中每一行为一个sample
tot <- read.xlsx('C:/Users/36461/Desktop/vic.xlsx',header = T,sheetIndex = 2,rowIndex = T)
#计算spearman相关性

cor <- rcorr(x=as.matrix(tot[,-1]),type=c("spearman") )
cor_r <- cor$r # 提取相关性矩阵
cor_p <- cor$P # 提取p值矩阵
cor_p[is.na(cor_p)] <- 1 # 将p值矩阵中的NA值替换为1

correlation <- as.matrix(cor_r)
n <- ncol(tot[,-1]) # 计算tot（除第一列外）的列数，本例中第一列为sample列
p <- c(cor_p)
bh <- sapply(as.numeric(p), p.adjust, method="BH") # 对p值进行Benjamini-Hochberg校正
padj <- matrix(c(bh),nrow=n,ncol=n)
padj[is.na(padj)] <- 1


#将对角线的p设为0
for(i in 1:n){
  for(j in 1:n){
    if(i==j){
      padj[i,j] <- 0
    }
  }
}

### 将p值大于0.05或r值小于0.5设为0
correlation[which(padj>0.05)] <- 0
correlation[which(abs(correlation)<0.6)] <- 0

network_adj <- as.matrix(correlation)

g <- graph_from_adjacency_matrix(network_adj, mode = 'undirected', weighted = TRUE, diag = FALSE)
E(g)$cov <- E(g)$weight
E(g)$weight <- abs(E(g)$weight)
V(g)$col=c(rep("blue",7),rep("red",(ncol(correlation)-7)))
deg=igraph::degree(g, mode = "all")
edge <- data.frame(as_edgelist(g))
#获得edge—list信息
edge_list <- data.frame( source = edge[[1]], target = edge[[2]], weight = E(g)$weight, cov = E(g)$cov)
#node_list <- data.frame( id = V(g)$name, label = V(g)$name, degree = degree(g))

group <- read.xlsx('C:/Users/36461/Desktop/vic.xlsx',header = F,sheetIndex = 4) #导入组别信息

#下述步骤用于删去抗性基因与抗性基因，抗生素与抗生素之间的相关性（即同组内的）
colnames(group) <- c('source','source_type')
edge_list2 <- left_join(edge_list,group)
colnames(group) <- c('target','target_type')
edge_list3 <- left_join(edge_list2,group)
edge_list4 <- edge_list3 %>%
  filter(source_type!=target_type)

#通过新构建的edge_list4获得degree信息
node_list1 <- edge_list4 %>%
  group_by(source) %>%
  dplyr::summarise(count=n())
colnames(node_list1) <- c('id','degree')
node_list2 <- edge_list4 %>%
  group_by(target) %>%
  dplyr::summarise(count=n())
colnames(node_list2) <- c('id','degree')
node_list <- rbind(node_list1,node_list2)

#write.graph(g, "./Corr/Network_tetARG-Fam.csv", format = 'graphml')
write.table(edge_list4, "C:/Users/36461/Desktop/vic_edge.csv", sep = ',', row.names = FALSE, quote = FALSE)
write.table(node_list, "C:/Users/36461/Desktop/vic_node.csv", sep = ',', row.names = FALSE, quote = FALSE)



