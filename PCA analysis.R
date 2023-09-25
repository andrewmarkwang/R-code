
########################################�����Աຯ��#################################################


#��R�Դ��ķ������ݼ�iris����Ϊ����data;
data<-iris
head(data)
#��ԭ���ݽ���z-score��һ����
dt<-as.matrix(scale(data[,1:4]))
head(dt)

#�������ϵ������
rm1<-cor(dt)
rm1

#������ֵ����������
rs1<-eigen(rm1)
rs1

#��ȡ����е�����ֵ���������ɷֵķ��
val <- rs1$values
#����ɱ�׼��(Standard deviation);
(Standard_deviation <- sqrt(val))

#���㷽����ʺ��ۻ������ʣ�
(Proportion_of_Variance <- val/sum(val))
(Cumulative_Proportion <- cumsum(Proportion_of_Variance))

#��ʯͼ����;
par(mar=c(6,6,2,2))
plot(rs1$values,type="b",
     cex=2,
     cex.lab=2,
     cex.axis=2,
     lty=2,
     lwd=2,
     xlab = "���ɷֱ��",
     ylab="����ֵ(���ɷַ���)")


#�������ɷ�
#��ȡ����е���������(Ҳ��ΪLoadings,�غɾ���)��
(U<-as.matrix(rs1$vectors))
#���о���˷������PC score��
PC <-dt %*% U
colnames(PC) <- c("PC1","PC2","PC3","PC4")
head(PC)

#�������ɷ�ɢ��ͼ
#��iris���ݼ��ĵ�5�����ݺϲ�������
df<-data.frame(PC,data$Species)
head(df)

#����ggplot2����
library(ggplot2)
#��ȡ���ɷֵķ�����ʣ�������������⣻
xlab<-paste0("PC1(",round(Proportion_of_Variance[1]*100,2),"%)")
ylab<-paste0("PC2(",round(Proportion_of_Variance[2]*100,2),"%)")
#����ɢ��ͼ������������Բ��
p1<-ggplot(data = df,aes(x=PC1,y=PC2,color=data.Species))+
  stat_ellipse(aes(fill=data.Species),type ="norm", geom ="polygon",alpha=0.2,color=NA)+
  geom_point()+labs(x=xlab,y=ylab,color="")
p1


#����scatterplot3d����
library(scatterplot3d)
color = c(rep('purple',50),rep('orange',50),rep('blue',50))
scatterplot3d(df[,1:3],color=color,
              pch = 16,angle=30,
              box=T,type="p",
              lty.hide=2,lty.grid = 2)
legend("topleft",c('Setosa','Versicolor','Virginica'),
       fill=c('purple','orange','blue'),box.col=NA)

##########################################ʹ���ֳɺ���#######################################
#scale. = TRUE��ʾ����ǰ�����ݽ��й�һ����
com1 <- prcomp(data[,1:4], center = TRUE,scale. = TRUE)
summary(com1)

#���ʹ��princomp()��������Ҫ������һ����princomp()�����������ݱ�׼����صĲ�����
#��Ĭ��ʹ��covariance matrix���õ��Ľ����ʹ������Ծ�����ϸ΢���죨���£���
#ԭ���Ǹ������ϵ����ʽ��֪����һ����������ϵ����������Э���
com2 <- princomp(dt,cor = T)
summary(com2)
com3 <- princomp(dt)
summary(com3)

#��ȡPC score��
df1<-com1$x
head(df1)
#��iris���ݼ��ĵ�5�����ݺϲ�������
df1<-data.frame(df1,iris$Species)
head(df1)

#��ȡ���ɷֵķ�����ʣ�������������⣻
summ<-summary(com1)
xlab<-paste0("PC1(",round(summ$importance[2,1]*100,2),"%)")
ylab<-paste0("PC2(",round(summ$importance[2,2]*100,2),"%)")
p2<-ggplot(data = df1,aes(x=PC1,y=PC2,color=iris.Species))+
  stat_ellipse(aes(fill=iris.Species),
               type = "norm", geom ="polygon",alpha=0.2,color=NA)+
  geom_point()+labs(x=xlab,y=ylab,color="")
p2+scale_fill_manual(values = c("purple","orange","blue"))+
  scale_colour_manual(values = c("purple","orange","blue"))