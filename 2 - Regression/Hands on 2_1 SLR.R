#Simple Linear Regression

# y= bo + b1x

# y= dependant variable
# x= independant variable
# b1= slope or coefficient
# bo= constant or intercept


#rading the data
dat= read.csv('Salary_Data.csv')

#splitting data into Training set and test set
library(caTools)
set.seed(123)
split= sample.split(dat$Salary, SplitRatio = 2/3)
Train= subset(dat, split ==T)
Test= subset(dat, split ==F)

#fitting SLR to training set
#formula takes care of feature scaling no need to do manually

#SLR using lm function
regressor = lm(formula = Salary~YearsExperience, data = Train)

#formula= y~x, data=train

#to see the results
summary(regressor)

# Call:
#   lm(formula = Salary ~ YearsExperience, data = Train)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -6653.9 -4973.4  -548.6  3284.1 10692.0 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      21871.0     3324.0    6.58 3.52e-06 ***
#   YearsExperience   9963.1      565.9   17.61 8.60e-13 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# *** means highly significant

# Residual standard error: 5848 on 18 degrees of freedom
# Multiple R-squared:  0.9451,	Adjusted R-squared:  0.9421 
# F-statistic: 309.9 on 1 and 18 DF,  p-value: 8.601e-13

#Predicting the test set results
y_pred= predict(regressor, newdata = Test)

#newdata is always the test set

y_pred


#visualizing the training the results
#using ggplot2
library(ggplot2)

ggplot()+
  geom_point(aes(x=Train$YearsExperience, y= Train$Salary), col="red")+
  
  geom_line(aes(x=Train$YearsExperience, y= predict(regressor, newdata= Train)), col="green")+
  
  ggtitle("Salary vs Years of Experience for Training Set")+
  xlab('Years of Experience')+
  ylab('Salary')+
  theme_minimal()

#ggplot to reserve area for plot
#geom_point to plot scatter plot
#geom_line to plot the line, y to be predicted values of y with predict function
#ggtitle to write title of the plot


#plotting scatter plot for test set data
ggplot()+
  geom_point(aes(x=Test$YearsExperience, y=Test$Salary), col="yellow")+
  geom_line(aes(x=Train$YearsExperience, y=predict(regressor, newdata = Train)),col="green")+
  ggtitle("Salary vs Years of Experience for Test Set")+
  ylab("Salary")+
  xlab('Years of Experience')+
  theme_dark()

#Important:geom_line will not change as we have trained data on the training line and checking test data for the trained line

#Optional:for plotting smooth scatterplot for fitted values
#scatter.smooth(x=Test$YearsExperience, y=Test$Salary, main= "Smooth scatterplot of Training set")
