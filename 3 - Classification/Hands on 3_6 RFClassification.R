#Random Forest Classification

#Ensemble Learning: When multiple ML algorithm put together to make on algorithm
#for RF there are many decision trees

# Step1: Pick at random K data points from the training set
# Step2: Build the decision treee associated to these K data points
# Step3: Choose the number Ntree of trees you want to build and repeat STEPS 1 & 2
# Step3: For a new data point, make each one of your Ntree trees predict the category to which
#       the data points belongs, and assign the new data point to category that wins majority votes  

dat =  read.csv('Social_Network_Ads.csv')
dat = dat[,3:5]

#IMPORTANT: Externaly inform classification by factorising the dependant variable
dat$Purchased = factor(dat$Purchased,
                       levels = c(0,1))

library(caTools)
set.seed(123)
split = sample.split(dat$Purchased, SplitRatio = 0.75)
train = subset(dat, split == T)
test = subset(dat, split == F)

train[,-3] = scale(train[,-3])
test[,-3] = scale(test[,-3])

library(randomForest)
classifier = randomForest(x= train[,-3],
                          y=train$Purchased,
                          ntree = 10)

classifier

# Call:
#   randomForest(x = train[, -3], y = train$Purchased, ntree = 10) 
# Type of random forest: classification
# Number of trees: 10
# No. of variables tried at each split: 1
# 
# OOB estimate of  error rate: 12.5%
# Confusion matrix:
#   0  1 class.error
# 0 173 16  0.08465608
# 1  21 86  0.19626168

y_pred = predict(classifier, newdata = test[,-3])
y_pred

# 2   4   5   9  12  18  19  20  22  29  32  34  35  38  45  46  48  52  66  69  74  75 
# 0   0   0   0   0   0   1   1   1   0   1   0   0   0   0   0   0   0   0   0   1   0 
# 82  84  85  86  87  89 103 104 107 108 109 117 124 126 127 131 134 139 148 154 156 159 
# 0   1   0   1   0   0   1   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
# 162 163 170 175 176 193 199 200 208 213 224 226 228 229 230 234 236 237 239 241 255 264 
# 0   0   0   0   0   0   0   0   1   1   1   0   1   0   0   1   0   0   1   1   1   0 
# 265 266 273 274 281 286 292 299 302 305 307 310 316 324 326 332 339 341 343 347 353 363 
# 1   1   1   1   1   1   1   1   0   0   1   0   0   1   0   1   0   1   0   1   1   1 
# 364 367 368 369 372 373 380 383 389 392 395 400 
# 1   1   1   0   1   0   1   1   1   1   0   1 
# Levels: 0 1

cm = table( test[,3],y_pred)
cm
# y_pred
#   0  1
# 0 54 10
# 1  5 31

#Visualizing Train Set
library(ElemStatLearn)
set = train
x1 = seq(min(set[,1])-1, max(set[,1])+1, by=0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, by=0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'Random Tree for Training Data',
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
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'Random Forest for Test Data',
     xlab = 'Age', ylab = 'EstimatedSalary',
     xlim = range(x1), ylim =range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add= T)
points(grid_set, pch = '.',col = ifelse(y_grid == 1,'springgreen4', 'tomato'))
points(set, pch = 21,bg = ifelse(set[,3]==1,'green4', 'red3'))



#Examining model by Measuring error
# False Positive and False Negative
# False positive Type error I: Predicted positive outcome but it's false
# False negative Type error II: Predicted negative outcome but it's false

#Confusion Matrix
#             y Predicted 
#             0   1
# y actual 0     FP
#          1  FN

#e.g.
#   0  1
# 0 54 10
# 1  5 31

#Accuracy Rate= correct / Total = (54+31)/100 = 85%
#Missclassification error = error/ total = (10+5)/100 = 15%





#Accuracy Paradox
#Confusion Matrix:
#   0     1
# 0 9700 150
# 1  50  100

#Accuracy = 9800/ 10000 = 98%

#convert

#   0  
# 0 9850 
# 1 150 

#Accuracy = 9850/10000 = 98.5%



#CAP curve
