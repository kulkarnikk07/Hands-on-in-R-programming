#MLR using automated backword elimination

dat= read.csv('50_Startups.csv')

library(caTools)
set.seed(123)
split= sample.split(dat$Profit,SplitRatio = 0.8)

Train= subset(dat, split==T)
Test= subset(dat, split==F)


#Automated function to perform backword elimination
backwardElimination <- function(x, sl) { #x is dataset and sl is significance level here sl=0.05
  numVars = length(x) #No. of variables in a dataset here 5 
  for (i in c(1:numVars)){  #for loop from 1 to 5
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"]) #2: no of variables, 1st is intercept hence not considered
    if (maxVar > sl){  #to eliminate the variable > sl
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL=0.05
dataset= dat
backwardElimination(Train, SL)

# Call:
#   lm(formula = Profit ~ ., data = x)
# 
# Residuals:
#   Min     1Q Median     3Q    Max 
# -34334  -4894   -340   6752  17147 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 4.902e+04  2.748e+03   17.84   <2e-16 ***
#   R.D.Spend   8.563e-01  3.357e-02   25.51   <2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 9836 on 38 degrees of freedom
# Multiple R-squared:  0.9448,	Adjusted R-squared:  0.9434 
# F-statistic: 650.8 on 1 and 38 DF,  p-value: < 2.2e-16

