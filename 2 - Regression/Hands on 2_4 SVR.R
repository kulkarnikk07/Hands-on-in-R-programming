#SVR- Support Vector Regression
# https://www.youtube.com/watch?v=WpODWJBzK2w

dat = read.csv('Position_Salaries.csv')
dat= dat[,2:3]

library(caTools)
set.seed(123)
split= sample.split(dat$Salary,SplitRatio = 0.8)

Train= subset(dat, split==T)
Test= subset(dat, split==F)

#SVR package and function
install.packages("e1071")
library(e1071)
SVR_regr= svm(formula= Salary~., data=dat,
              type = "eps-regression") #C-classification or eps-regression and default kernal gaussian

#To predict the new result for the new value Level=6.5
y_pred = predict(SVR_regr, data.frame(Level=6.5))

y_pred
#Result: 177861.1

#Visualize the results in graph
library(ggplot2)
ggplot()+
  geom_point(aes(x= dat$Level, y= dat$Salary), col="green")+
  geom_line(aes(x= dat$Level, y= predict(SVR_regr, newdata = dat)),col="yellow")+
  theme_minimal()+
  ggtitle("SVR Predictions")+
  xlab("Job Levels")+
  ylab("Salary")
