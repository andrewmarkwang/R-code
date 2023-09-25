library(readxl)
library(gplots)
library(car)
library(multcomp)
library(ggplot2)
library(tidyverse)
#用data frame的格式输入数据
raw_data<-read.csv('C:/Users/think/Desktop/ace.txt',col.names = c('conc','Treatment'),sep='',header = F)
# #medicine <- data.frame(
#   conc=c(0.4223,0.4346,0.4821,0.5,0.4976,0.4834),
#   Treatment=factor(c(rep('BUT',3),rep('CK',3)))
# )       



#各组样本大小
table(raw_data$Treatment)
#group names
name<-raw_data$Treatment
#number of groups
num_group<-length(unique(name))
#
a1<-c(1,2,3,13,14,15)
a2<-c(4:6,13:15)
a3<-c(7:9,13:15)
a4<-c(10:12,13:15)
b1<-c(16:18,28:30)
b2<-c(19:21,28:30)
b3<-c(22:24,28:30)
b4<-c(25:30)
lis<-list(a1,a2,a3,a4,b1,b2,b3,b4)

#

#各组的均值
#mu<-aggregate(raw_data$conc,by=list(raw_data$Treatment),FUN=mean)
mu<-raw_data %>%
  group_by(Treatment) %>%
  summarise(mu=mean(conc)) 
#各组的标准差
#sd<-aggregate(raw_data$conc,by=list(raw_data$Treatment),FUN=sd)
sd<-raw_data %>%
  group_by(Treatment) %>%
  summarise(mu=sd(conc))
#调用aov函数进行方差分析（检验组间差异）
#

for (i in 1:8) {
  r<-lis[[i]]
  sample_aov<-aov(conc ~ Treatment,data=raw_data[r,])
  print(summary(sample_aov))
}

x<-c(1:3,16:18)
raw_data.aov <- aov(conc ~ Treatment,data=raw_data[x,])
#summary提取方差分析的结果
summary(raw_data.aov)



anova.tab <- function(fm){
  tab <- summary(fm)
  k <- length(tab[[1]]-2)
  temp <- c(sum(tab[[1]][,1]),sum(tab[[1]][,2]),rep(NA,k))
  tab[[1]]["Total",] <- temp
}


#plot(raw_data$conc~raw_data$Treatment)
#绘制各组均值及其置信区间的图形

#plotmeans(raw_data$conc~raw_data$Treatment,xlab = "Treatment",ylab = "conc",main = "Mean Plot\nwith 95% CI")


# 如果p<0.05时,说明各组之间有显著性差异，具体哪些组之间有显著性差异，还要进行多重比较，TukeyHSD()函数提供了对各组均值差异的成
#对检验。 注意TukeyHSD()函数与本章使用的HH包存在兼容性问题：若载入HH包，TukeyHSD()函数将会失效。对于上例，使用detach("package::HH")将它从搜寻路
#径中删除，然后再调用TukeyHSD()
TukeyHSD(raw_data.aov)
#par()函数旋转轴标签，增大左边界面积，使标签摆放更美观。
par(las = 2)
par(mar = c(5, 8, 4, 2))
plot(TukeyHSD(raw_data.aov))

#多次重复使用t检验会增大犯第一类错误的概率，为了克服这一缺点，需要调整p-值。R软件调整p-值用的是p.adjust()函数，函数使用的不同参数代表不同的调整方法。
attach(raw_data)
p_adjust<-pairwise.t.test(conc, Treatment, p.adjust.method = "none")

plot(raw_data$conc~raw_data$Treatment)

###############

#used for testing Homogeneity Of Variance
bartlett.test(k~time, data = raw_data)

#Two way ANOVA
raw_data<-read.csv('C:/Users/think/Desktop/aa.txt',sep='',header = F)
colnames(raw_data)<-c('mol','time','k')
raw_data$mol<-as.character(raw_data$mol)
raw_data$time<-as.character(raw_data$time)
fit<-aov(k~mol*time,data=raw_data)
summary(fit)

plot(fit)


TT<-TukeyHSD(fit, "mol", conf.level = 0.95)
plot(TT)  #if the confidence interval does not cover 0, Signifacant!



###############柱形图

c_plot<-merge(mu,sd,by.x = 'Treatment',by.y = 'Treatment')
#c_plot<-rbind(c_plot,c_plot)
c_plot<-cbind(c_plot,c('14-day','7-day','14-day','7-day','14-day','7-day','14-day','7-day','14-day','7-day'))
colnames(c_plot)<-c('sample','mu','sd','group')

label <- c("*","***", "","", '',   "","*", "",'','')
TNM<-'Times New Roman'
cbPalette <- c("#33FFFF", "#33CC99")
p<-ggplot(c_plot,aes(x=factor(sample),y=mu,fill=group))+
  geom_col(position="dodge")+
  geom_errorbar(aes(ymin =mu-sd, ymax = mu+sd),width = 0.2, position = position_dodge(0.9))+
  geom_text(aes(y = mu +sd+0.21, label = label, group = group), 
            position = position_dodge(0.9), size = 5, fontface = "bold") +
  theme_bw()+
  ylim(0,0.6)+
  labs(x='Sample',y='Value')+
  theme(plot.title = element_text(hjust = 0.5,family = TNM, face = "bold", size = 12), 
                   legend.position="right", 
                   legend.title = element_blank(),
                   legend.text = element_text(hjust = 0.5,family = TNM, face = "bold", size = 15),
                   axis.text.x=element_text(hjust = 0.5,family = TNM, face = "bold", size = 12),
                   axis.text.y=element_text(hjust = 0.5,family = TNM, face = "bold", size = 12),
                   panel.grid.major = element_line(colour = "white"),
                   panel.grid.minor = element_line(colour = "white", size = 0.25),
                   panel.border = element_rect(color="black", size=1.2, linetype="solid"),
                   axis.ticks = element_line(colour = "black",size=1),
                   text=element_text(hjust = 0.5,family = TNM, face = "bold", size = 15)
  )+
  scale_fill_manual(values=cbPalette)
p



