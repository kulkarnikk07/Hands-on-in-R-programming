#Ploynomial Linear Regression
#y= bo+b1x1+b2X1^2+ bnX1^n sometimes called exponential curve
#the coefficients are linear hence called linear regression

dat=read.csv('Position_Salaries.csv')
dat=dat[,2:3] #ignoring categorical data

library(caTools)
set.seed(123)
split= sample.split(dat$Level, SplitRatio = 0.8)
Train= subset(dat, split==T)
Test=subset(dat,split==F)

#fitting the linear regression model
lin_reg= lm(formula= Salary~Level, data = dat)
summary(lin_reg)

# Call:
#   lm(formula = Salary ~ Level, data = dat)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -170818 -129720  -40379   65856  386545 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -195333     124790  -1.565  0.15615   
# Level          80879      20112   4.021  0.00383 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 182700 on 8 degrees of freedom
# Multiple R-squared:  0.669,	Adjusted R-squared:  0.6277 
# F-statistic: 16.17 on 1 and 8 DF,  p-value: 0.003833

#visualization for Linear Regression

library(ggplot2)
ggplot() + 
  geom_point(aes(y=dat$Salary , x= dat$Level),col="red") +
  geom_line(aes(x=dat$Level , y= predict(lin_reg, newdata = dat)),col="blue")+
  ggtitle("Linear Regression Plot")+
  xlab("Level")+
  ylab("Salary")

#Prediction on Linear Regression for unlnown value level=6.5
y_predlin = predict(lin_reg, data.frame(Level=6.5))

#to see the result
y_predlin
#330378.8 


#fitting the polynomial regression model
dat=read.csv('Position_Salaries.csv')
dat=dat[,2:3] #ignoring categorical data

library(caTools)
set.seed(123)
split= sample.split(dat$Level, SplitRatio = 0.8)
Train= subset(dat, split==T)
Test=subset(dat,split==F)


dat$LevelSquared = dat$Level^2 #to create the exponential column in dataset
dat$LevelCube = dat$Level^3

ploy_reg= lm(formula= Salary~., data = dat)
summary(ploy_reg)

# Call:
#   lm(formula = Salary ~ ., data = dat)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -75695 -28148   7091  29256  49538 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -121333.3    97544.8  -1.244  0.25994   
# Level         180664.3    73114.5   2.471  0.04839 * 
#   LevelSquared  -48549.0    15081.0  -3.219  0.01816 * 
#   LevelCube       4120.0      904.3   4.556  0.00387 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 50260 on 6 degrees of freedom
# Multiple R-squared:  0.9812,	Adjusted R-squared:  0.9718 
# F-statistic: 104.4 on 3 and 6 DF,  p-value: 1.441e-05

#Prediction on Linear Regression for unlnown value level=6.5
y_predploy = predict(ploy_reg, data.frame(Level=6.5, 
                                          LevelSquared= 6.5^2,
                                          LevelCube= 6.5^3))

#to see the result
y_predploy
#133259.5

#This results cannot show the polynomial behavior; visualize the results to see the behavior


#visualization for Polynomial Regression
ggplot() + 
  geom_point(aes(y=dat$Salary , x= dat$Level),col="red") +
  geom_line(aes(x=dat$Level , y= predict(ploy_reg, newdata = dat)),col="blue")+
  ggtitle("Polynomial Regression Plot")+
  xlab("Level")+
  ylab("Salary") +
  theme_minimal()

#Polynomial Regression with 4th Degree
dat$Leveltimes4 = dat$Level^4 #to create the exponential column in dataset

poly_reg= lm(formula= Salary~., data = dat)
summary(poly_reg)

# Call:
#   lm(formula = Salary ~ ., data = dat)
# 
# Residuals:
#   1      2      3      4      5      6      7      8      9     10 
# -8357  18240   1358 -14633 -11725   6725  15997  10006 -28695  11084 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   184166.7    67768.0   2.718  0.04189 * 
#   Level        -211002.3    76382.2  -2.762  0.03972 * 
#   LevelSquared   94765.4    26454.2   3.582  0.01584 * 
#   LevelCube     -15463.3     3535.0  -4.374  0.00719 **
#   Leveltimes4      890.2      159.8   5.570  0.00257 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 20510 on 5 degrees of freedom
# Multiple R-squared:  0.9974,	Adjusted R-squared:  0.9953 
# F-statistic: 478.1 on 4 and 5 DF,  p-value: 1.213e-06


#visualization for Polynomial Regression
ggplot() + 
  geom_point(aes(y=dat$Salary , x= dat$Level),col="red") +
  geom_line(aes(x=dat$Level , y= predict(poly_reg, newdata = dat)),col="blue")+
  ggtitle("Polynomial Regression Plot")+
  xlab("Level")+
  ylab("Salary") +
  theme_minimal()

#Prediction on Linear Regression for unlnown value level=6.5
y_predpoly = predict(poly_reg, data.frame(Level=6.5, 
                                          LevelSquared= 6.5^2,
                                          LevelCube= 6.5^3,
                                          Leveltimes4=6.5^4))

#to see the result
y_predpoly
#158862.5

