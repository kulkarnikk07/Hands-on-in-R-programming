#Feature scaling

#Standardization: Xstd = (x- mean(x))/(std dev x)
#Normalisation: Xnorm = (x-min(x))/(max(x)-min(x))  

dat=read.csv('Data.csv')

install.packages("caTools")
library(caTools)
set.seed(123)

#conveting categorical into dummy variables
dat$Country = factor(dat$Country, 
                     levels = c('France', 'Spain','Germany'),
                     labels = c(1,2,3))
dat$Purchased = factor(dat$Purchased,
                       levels = c('No','Yes'),
                       labels = c(0,1))

splilt= sample.split(dat$Age, SplitRatio = 0.7)
train= subset(dat, splilt==T)
test= subset(dat, splilt==F)

#feature scaling
train[,2:3]= scale(train[,2:3])
test[,2:3]= scale(test[,2:3])
