#SVM (SUPPORT VECTOR MACHINE)
dat= read.csv('Social_Network_Ads.csv')
dat = dat[,3:5]

library(caTools)
set.seed(123)
split = sample.split(dat$Purchased, SplitRatio = 0.75)
Train = subset(dat, split==T)
Test = subset(dat, split==F)

Train[,1:2]= scale(Train[,1:2])
Test[,1:2]= scale(Test[,1:2])

library(e1071)
classifier = svm(formula= Purchased ~ .,
                 data = Train,
                 type=  'C-classification',
                 kernel = 'linear')

summary(classifier)

# Call:
#   svm(formula = Purchased ~ ., data = Train, type = "C-classification", kernel = "linear")
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  linear 
# cost:  1 
# gamma:  0.5 
# 
# Number of Support Vectors:  120
# 
# ( 60 60 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   0 1

#Predictons on Test Set

y_pred= predict(classifier, newdata = Test[,1:2])
y_pred

# 1   2   5   8   9  14  17  19  26  30  31  35  42  43  45  48  49  62  66  71  72  76 
# 0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
# 79  81  82  83  85 100 103 105 106 114 121 123 124 128 131 135 144 151 153 156 157 158 
# 0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   1   0 
# 166 172 173 190 196 197 202 206 219 220 221 223 224 229 230 231 234 235 249 257 260 262 
# 0   0   0   0   0   0   1   1   1   1   0   1   1   0   1   1   1   1   0   0   1   1 
# 266 269 278 281 288 294 295 298 299 305 309 318 319 325 334 335 338 341 347 354 358 359 
# 1   1   1   1   1   0   0   1   1   0   1   0   0   1   0   1   0   1   1   0   0   0 
# 360 363 365 368 378 379 385 389 396 397 399 400 
# 0   1   1   1   0   1   1   0   0   1   0   1 
# Levels: 0 1

#Classification Matrix
cm = table(Test[,3],y_pred)
cm

#   0  1
# 0 58 6
# 1  9 27

#Visualizing Train Set
library(ElemStatLearn)
set = Train
x1 = seq(min(set[,1])-1, max(set[,1])+1, by=0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, by=0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM for Training Data',
     xlab = 'Age', ylab = 'EstimatedSalary',
     xlim = range(x1), ylim =range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add= T)
points(grid_set, pch = '.',col = ifelse(y_grid == 1,'springgreen4', 'tomato'))
points(set, pch = 21,bg = ifelse(set[,3]==1,'green4', 'red3'))

#Visualizing Test Set
set = Test
x1 = seq(min(set[,1])-1, max(set[,1])+1, by=0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, by=0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM for Test Data',
     xlab = 'Age', ylab = 'EstimatedSalary',
     xlim = range(x1), ylim =range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add= T)
points(grid_set, pch = '.',col = ifelse(y_grid == 1,'springgreen4', 'tomato'))
points(set, pch = 21,bg = ifelse(set[,3]==1,'green4', 'red3'))