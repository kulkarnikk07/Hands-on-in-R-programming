#Random Forest Regression
#Part of ensemble learning
# Step1: pick at random K data points from Training Set
# Step2: Build desicion tree based on these K data points
# Step3: Choose teh number Ntree of trees you want to build and repeat STEPS 1&2
# Step4: For a new data point, make each one of your Ntree trees predict the value of Y
# to for the data point in questionand assign the new data point the average across 
# all of the predicted Y values. 

dat= read.csv("Position_Salaries.csv")
dat = dat[,2:3]

library(caTools)
set.seed(123)
split = sample.split(dat$Salary, SplitRatio = 0.8)
Train = subset(dat, split == T)
Test = subset(dat, split == F)

#package 
install.packages("randomForest")
library(randomForest)

set.seed(1234)
rand_reg = randomForest(x= dat[1],
                        y= dat$Salary, ntree = 10)

#Predicting the new Level=6.5
y_pred= predict(rand_reg, newdata = data.frame(Level=6.5))

y_pred
#145900 for ntree=10 
#165013.3 for ntree=100
#158581.3 for ntree=500

#Visualizing in scatterplot in High Resolution

library(ggplot2)
x_grid = seq(min(x= dat$Level), max(dat$Level),0.01)

ggplot()+
  geom_point(aes(x=dat$Level, y=dat$Salary), col= "red")+
  geom_line(aes(x=x_grid, y= predict(rand_reg, newdata = data.frame(Level =x_grid))),
                col="Blue")+
  ggtitle("Random Forest")+
  xlab("Job Level")+
  ylab("Salary")+
  theme_minimal()

#Decision tree visulaization
install.packages("party")
library(party)
cforest(Salary ~ ., data=dat, controls=cforest_control(mtry=2, mincriterion=0))

library(randomForest)
MDSplot(rand_reg, dat$Salary)
