dat=read.csv('clean577.csv')

#Histogram
hist(dat$age)
hist(dat$day, main = "Histogram of Days", xlab = "Number of days", col=c("green","yellow"))

#normalization histogram
hist(rnorm(dat$age))
hist(rnorm(dat$age),main = "Histogram of age", xlab = "Number of age", col=c("green","yellow"))
#boxplot
boxplot(dat$age)
 
#boxplot
boxplot(dat$age, horizontal = T, main="Boxplot of Age",xlab='years', 
        col = c("green"))


##using ggplot2 package
install.packages("ggplot2")
library(ggplot2)        

datfr= read.csv('clean577.csv')
contVar=datfr[,2:8]

install.packages("GGally")
library(GGally)
ggcorr(contVar)

plot(ggpairs(datfr))



#ggplot2 install and use
install.packages("ggplot2")
library(ggplot2)

ggplot(dat,aes(x=dat$pdays)) + geom_histogram(bins = 100,color = 'red', fill = 'pink') + theme_minimal()
#ggplot- plots the dataframe in its x and y axis; No data shown just the area of graph
#geom_histogram: to plot histogram
#theme_minimal(): to set the theme
#all unctions work together hence + used for continuity



