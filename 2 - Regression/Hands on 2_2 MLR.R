#Multiple Linear Regression without Dummy Variables

# MLR: y=b0 + b1X1+ b2X2+ b3X3+....+bnXn
#y dependant variable, x1...xn dependant variable
 
# Assumption to be checked for linear regression
# 1. linearity
# 2. Homoscedasticity
# 3. Multivariate normality
# 4. Independence of error
# 5. Lack of multicollinear

#Import the dataset
dat=read.csv('50_Startups.csv')

str(dat)
#for State column create dummy variables to use this column in the regression model
#always omit one dummy variable as a reference

#factorising to the levels of category
dat$State= factor(dat$State,
                  levels =c("California","Florida","New York"),
                  labels = c(3,1,2))

# #creating dummies of all levels
# library(dummies)
# dat=cbind(dat,dummy(dat$State, sep = "_")) 
# 
# #Renaming the dummy variables
# colnames(dat)[c(6)] = "Dummy_CA"
# colnames(dat)[c(7)] = "Dummy_NY"
# colnames(dat)[c(8)] = "Dummy_FL"
# 
# #omitting the original column for this State
# dat=dat[,-4]
# 
# #omitting reference variable in this case Dummy_CA
# dat=dat[,-5]

#MLR building models types:
# 1. All in: Throwing all the variables in the model; Not preffered
# 2. backword Elimination: Fastest, robust and preffered
# 3. Forward Selection
# 4. Bidirectional Elimination
# 5. Score Comaprison
 
# 2,3,4 called Stepwise Regression

library(caTools)
set.seed(321)
split=sample.split(dat$Profit, SplitRatio = 0.8)
split
Train = subset(dat,split ==T)
Test =subset(dat, split ==F)

#no need to do feature scaling, taken care by the model of MLR

#Fitting MLR to training set

#regressor = lm(formula= Profit~R.D.Spend + Administration + Marketing.Spend + State, data=Train)

#Preffered: 
regressor = lm(formula = Profit ~.,data= Train)
#lm(formula = y~ .) . means all the independant variables; use it in MLR to write the independent variables

summary(regressor)

# Call:
#   lm(formula = Profit ~ ., data = Train)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -33187  -5022    255   7585  17733 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      4.900e+04  8.236e+03   5.950 1.00e-06 ***
#   R.D.Spend        8.039e-01  5.471e-02  14.696 2.72e-16 ***
#   Administration  -2.078e-02  6.301e-02  -0.330    0.744    
# Marketing.Spend  2.870e-02  2.125e-02   1.351    0.186    
# State1           1.131e+03  4.250e+03   0.266    0.792    
# State2          -8.901e+01  4.026e+03  -0.022    0.982    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 10430 on 34 degrees of freedom
# Multiple R-squared:  0.9346,	Adjusted R-squared:  0.9249 
# F-statistic:  97.1 on 5 and 34 DF,  p-value: < 2.2e-16

# IMPORTANT: Whether creating dummy variables for categorical variable the MLR model creates
# the categories and eliminate on category as reference to reduce redundancy

#Predicting the test set results
y_pred= predict(regressor, newdata = Test)

y_pred


