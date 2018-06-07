#process on categorical variables

dat=read.csv("Data.csv")
#categories conversion in numerical factors
dat$Country = factor(dat$Country, 
                     levels = c('France', 'Spain','Germany'),
                     labels = c(1,2,3))
dat

dat$Purchased = factor(dat$Purchased,
                       levels = c('No','Yes'),
                       labels = c(0,1))
dat

#Dummy variable
#if there are k levels then no of dummy variables will be k-1
install.packages("dummies")
library(dummies)

dat = cbind(dat,dummy(dat$Country,sep='_')) 
dat

dat= cbind(dat, dummy(dat$Purchased,sep='_'))
dat

#to check the dummy variables are created
d= dummy.data.frame(dat) 
get.dummy(d,'Country')
