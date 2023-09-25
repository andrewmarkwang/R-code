

#used for testing Homogeneity Of Variance

bartlett.test(k~Time, data = data_50)

#Two way ANOVA
raw_data<-read.csv('C:/Users/think/Desktop/aa.txt',sep='',header = T)
#colnames(raw_data)<-c('mol','time','k')
raw_data$Ratio<-as.character(raw_data$Ratio)
raw_data$Time<-as.character(raw_data$Time)

data_50<-raw_data %>%
  filter(Dp==50)

fit<-aov(k~Ratio*Time,data=data_50)
summary(fit)

plot(fit)



TT<-TukeyHSD(fit, "Ratio", conf.level = 0.95)
plot(TT)  #if the confidence interval does not cover 0, Signifacant!
