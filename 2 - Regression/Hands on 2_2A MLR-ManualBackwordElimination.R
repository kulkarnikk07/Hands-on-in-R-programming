# MLR using manually backword elimination algorithm

dat=read.csv('50_Startups.csv')
str(dat)

dat$State= factor(dat$State, levels =c("California","Florida","New York"),
                  labels = c(3,1,2))
split=sample.split(dat, SplitRatio = 0.8)
Train= subset(dat, split==T )
Test= subset(dat, split==F)

#backword elimination model
regressor=lm(formula= Profit~ R.D.Spend + Administration + Marketing.Spend + State,
             data= dat) 
#write all independant variable to eliminatethem later
#dat taken to know each variables which are independantly significant
#refer description of Backword Elimination
#Eliminate variable P- value > SL (Significancs level here 0.05)

summary(regressor)

# Call:
#   lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + 
#        State, data = dat)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -33504  -4736     90   6672  17338 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      5.013e+04  6.885e+03   7.281 4.44e-09 ***
#   R.D.Spend        8.060e-01  4.641e-02  17.369  < 2e-16 ***
#   Administration  -2.700e-02  5.223e-02  -0.517    0.608    
# Marketing.Spend  2.698e-02  1.714e-02   1.574    0.123    
# State1           1.988e+02  3.371e+03   0.059    0.953    
# State2          -4.189e+01  3.256e+03  -0.013    0.990    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 9439 on 44 degrees of freedom
# Multiple R-squared:  0.9508,	Adjusted R-squared:  0.9452 
# F-statistic: 169.9 on 5 and 44 DF,  p-value: < 2.2e-16

#Remove variables with highest P value here state 1 and state 2 variables
#Remove state and run the command

regressor=lm(formula= Profit~ R.D.Spend + Administration + Marketing.Spend,
             data= dat) 
summary(regressor)

# Call:
#   lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, 
#      data = dat)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -33534  -4795     63   6606  17275 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      5.012e+04  6.572e+03   7.626 1.06e-09 ***
#   R.D.Spend        8.057e-01  4.515e-02  17.846  < 2e-16 ***
#   Administration  -2.682e-02  5.103e-02  -0.526    0.602    
# Marketing.Spend  2.723e-02  1.645e-02   1.655    0.105    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 9232 on 46 degrees of freedom
# Multiple R-squared:  0.9507,	Adjusted R-squared:  0.9475 
# F-statistic:   296 on 3 and 46 DF,  p-value: < 2.2e-16

#Remove variables with highest P value here Administration (p value = 0.062) variables
#Remove Administration and run the command

regressor=lm(formula= Profit~ R.D.Spend + Marketing.Spend,
             data= dat) 
summary(regressor)


# Call:
#   lm(formula = Profit ~ R.D.Spend + Marketing.Spend, data = dat)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -33645  -4632   -414   6484  17097 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     4.698e+04  2.690e+03  17.464   <2e-16 ***
#   R.D.Spend       7.966e-01  4.135e-02  19.266   <2e-16 ***
#   Marketing.Spend 2.991e-02  1.552e-02   1.927     0.06 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 9161 on 47 degrees of freedom
# Multiple R-squared:  0.9505,	Adjusted R-squared:  0.9483 
# F-statistic: 450.8 on 2 and 47 DF,  p-value: < 2.2e-16

#Remove variables with highest P value here Marketing.Spend variable
#Remove Marketing.Spend and run the command

regressor=lm(formula= Profit~ R.D.Spend,
             data= dat) 
summary(regressor)

# Call:
#   lm(formula = Profit ~ R.D.Spend, data = dat)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -34351  -4626   -375   6249  17188 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.903e+04  2.538e+03   19.32   <2e-16 ***
#   R.D.Spend   8.543e-01  2.931e-02   29.15   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 9416 on 48 degrees of freedom
# Multiple R-squared:  0.9465,	Adjusted R-squared:  0.9454 
# F-statistic: 849.8 on 1 and 48 DF,  p-value: < 2.2e-16


#This is the manually created backword Elimination model 


# Other useful functions
coefficients(regressor) # model coefficients
confint(regressor, level=0.95) # CIs for model parameters
fitted(regressor) # predicted values
residuals(regressor) # residuals
anova(regressor) # anova table
vcov(regressor) # covariance matrix for model parameters
influence(regressor) # regression diagnostics

# diagnostic plots
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
plot(regressor)

