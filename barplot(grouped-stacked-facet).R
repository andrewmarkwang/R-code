# library
library(ggplot2)
library(viridis)
library(hrbrthemes)

#################Color颜色###############################

# 1: uniform color. Color is for the border, fill is for the inside
ggplot(mtcars, aes(x=as.factor(cyl) )) +
  geom_bar(color="blue", fill=rgb(0.1,0.4,0.5,0.7) )

# 2: Using Hue
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + 
  geom_bar( ) +
  scale_fill_hue(c = 40) +
  theme(legend.position="none")

# 3: Using RColorBrewer
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set2") +
  theme(legend.position="none")


# 4: Using greyscale:
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) + 
  geom_bar( ) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(legend.position="none")


# 5: Set manualy
ggplot(mtcars, aes(x=as.factor(cyl), fill=as.factor(cyl) )) +  
  geom_bar( ) +
  scale_fill_manual(values = c("red", "green", "blue") ) +
  theme(legend.position="none")

######################################

# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)


#input dataset must provide 3 columns: the numeric value (value), and 2 categorical variables for the group (specie) and the subgroup (condition) levels.
#in the aes() call, x is the group (specie), and the subgroup (condition) is given to the fill argument.
#in the geom_bar() call, position="dodge" must be specified to have the bars one beside each other.


# Grouped 分组
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")


# Stacked 堆叠
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity")


# Stacked + percent 堆叠百分比
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="fill", stat="identity")

# Grouped barchart customization
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Studying 4 species..") +
  theme_ipsum() +
  xlab("")

# Small multiple 分面图
ggplot(data, aes(fill=condition, y=value, x=condition)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Studying 4 species..") +
  facet_wrap(~specie) +
  theme_ipsum() +
  theme(legend.position="none") +
  xlab("")
