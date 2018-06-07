#Decision Tree for Regression
#Non com=ntinuous Regression model

#CART - Classification and Regression Trees

dat=read.csv("Position_Salaries.csv")
dat= dat[,2:3]

library(caTools)
set.seed(213)
split= sample.split(dat$Level,SplitRatio = 0.8)
Train= subset(dat, split==T)
Test= subset(dat, split==F)

#Decision Tree- Regression Tree modeling
installed.packages("rpart")
library(rpart)

Tree_reg = rpart(formula = Salary~.,data=dat, 
                 control = rpart.control(minsplit = 1))
summary(Tree_reg)

#Results without rpart.cotrol
# Call:
#   rpart(formula = Salary ~ ., data = dat)
# n= 10 
# 
# CP nsplit rel error xerror xstd
# 1 0.01      0         1      0    0
# 
# Node number 1: 10 observations
# mean=249500, MSE=8.066225e+10

#Results with rpart.cotrol inclusion; PREFERRED

# Call:
#   rpart(formula = Salary ~ ., data = dat, control = rpart.control(minsplit = 1))
# n= 10 
# 
# CP nsplit  rel error   xerror      xstd
# 1 0.77638626      0 1.00000000 1.234568 0.7835133
# 2 0.15496716      1 0.22361374 1.201144 0.7880448
# 3 0.05217357      2 0.06864658 1.196398 0.7887511
# 4 0.01000000      3 0.01647301 1.204579 0.7875655
# 
# Variable importance
# Level 
# 100 
# 
# Node number 1: 10 observations,    complexity param=0.7763863
# mean=249500, MSE=8.066225e+10 
# left son=2 (8 obs) right son=3 (2 obs)
# Primary splits:
#   Level < 8.5 to the left,  improve=0.7763863, (0 missing)
# 
# Node number 2: 8 observations,    complexity param=0.05217357
# mean=124375, MSE=6.921484e+09 
# left son=4 (6 obs) right son=5 (2 obs)
# Primary splits:
#   Level < 6.5 to the left,  improve=0.7600316, (0 missing)
# 
# Node number 3: 2 observations,    complexity param=0.1549672
# mean=750000, MSE=6.25e+10 
# left son=6 (1 obs) right son=7 (1 obs)
# Primary splits:
#   Level < 9.5 to the left,  improve=1, (0 missing)
# 
# Node number 4: 6 observations
# mean=82500, MSE=1.38125e+09 
# 
# Node number 5: 2 observations
# mean=250000, MSE=2.5e+09 
# 
# Node number 6: 1 observations
# mean=500000, MSE=0 
# 
# Node number 7: 1 observations
# mean=1000000, MSE=0


#Predicting the test results for value Leel=6.5

y_pred= predict(Tree_reg, data.frame(Level=6.5))

y_pred
#249500

#Visualizing the results on scatterplot (For High resolution, every step is split for data tree)
library(ggplot2)
x_grid = seq(min(dat$Level), max(dat$Level),0.01) #0.1, 0.01 are resolution

ggplot()+
  geom_point(aes(x= dat$Level, y=dat$Salary), col = 'red')+
  geom_line(aes(x= x_grid, y= predict(Tree_reg, 
                                      newdata = data.frame(Level = x_grid))), 
            col="green")+
  xlab("Job Levels")+
  ylab("Salary")+
  ggtitle("Prediction of Regression Tree")+
  theme_minimal()

#Tree structure graph
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(Tree_reg, type = 4, digits = 2, fallen.leaves = F)
