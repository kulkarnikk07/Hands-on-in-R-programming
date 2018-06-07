dat = read.csv('Social_Network_Ads.csv')
dat = dat[,3:5]

library(caTools)
set.seed(123)
split = sample.split(dat$Purchased, SplitRatio = 0.75)
train = subset(dat, split== T)
test = subset(dat, split == F)

train[,-3] = scale(train[,-3])
test[,-3] = scale(test[,-3])

library(rpart)
classifier = rpart(formula = Purchased ~ ., data = train, method = "class") #method= class for classification
classifier
# n= 300 
# 
# node), split, n, deviance, yval
# * denotes terminal node
# 
# 1) root 300 68.836670 0.35666670  
# 2) Age< 0.4699183 219 30.082190 0.16438360  
# 4) EstimatedSalary< 0.5497465 184  7.652174 0.04347826  
# 8) Age< -0.1098301 119  0.000000 0.00000000 *
#   9) Age>=-0.1098301 65  7.015385 0.12307690  
# 18) EstimatedSalary< -0.1124778 30  0.000000 0.00000000 *
#   19) EstimatedSalary>=-0.1124778 35  6.171429 0.22857140 *
#   5) EstimatedSalary>=0.5497465 35  5.600000 0.80000000  
# 10) EstimatedSalary< 1.437729 20  4.550000 0.65000000  
# 20) EstimatedSalary>=1.091566 10  2.400000 0.40000000 *
#   21) EstimatedSalary< 1.091566 10  0.900000 0.90000000 *
#   11) EstimatedSalary>=1.437729 15  0.000000 1.00000000 *
#   3) Age>=0.4699183 81  8.765432 0.87654320  
# 6) Age< 0.8564173 16  3.750000 0.62500000 *
#   7) Age>=0.8564173 65  3.753846 0.93846150 *

y_out= predict(classifier, newdata = test[,-3], type = "class") #type= class for classification
y_out

#confusion matrix
cm= table(test[,3], y_out)
cm

# y_out
#   0  1
# 0 53 11
# 1  6 30

plot(classifier)
text(classifier)
#Visualizing Train Set
library(ElemStatLearn)
set = train
x1 = seq(min(set[,1])-1, max(set[,1])+1, by=0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, by=0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set,type = "class")
plot(set[,-3],
     main = 'Classification Tree for Training Data',
     xlab = 'Age', ylab = 'EstimatedSalary',
     xlim = range(x1), ylim =range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add= T)
points(grid_set, pch = '.',col = ifelse(y_grid == 1,'springgreen4', 'tomato'))
points(set, pch = 21,bg = ifelse(set[,3]==1,'green4', 'red3'))

#Visualizing Test Set
library(ElemStatLearn)
set = test
x1 = seq(min(set[,1])-1, max(set[,1])+1, by=0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, by=0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set,type = "class")
plot(set[,-3],
     main = 'Classification Tree for Test Data',
     xlab = 'Age', ylab = 'EstimatedSalary',
     xlim = range(x1), ylim =range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add= T)
points(grid_set, pch = '.',col = ifelse(y_grid == 1,'springgreen4', 'tomato'))
points(set, pch = 21,bg = ifelse(set[,3]==1,'green4', 'red3'))