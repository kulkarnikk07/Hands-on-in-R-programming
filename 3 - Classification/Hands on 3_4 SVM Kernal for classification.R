# Different types of Kernals
# Gaussian RBF Kernal: looks like bell curve
# sigmoid Kernal: looks like s in two lines
# Polynomial Kernal

dat= read.csv('Social_Network_Ads.csv')
dat = dat[,3:5]

library(caTools)
set.seed(123)
split = sample.split(dat$Purchased, SplitRatio = 0.75)
Train = subset(dat, split==T)
Test = subset(dat, split==F)

Train[,1:2]= scale(Train[,1:2])
Test[,1:2]= scale(Test[,1:2])

#FITTING KERNAL to Train set 
#Kernlab is another useful package but e1071 is popular
library(e1071)
classifier = svm(formula= Purchased ~ .,
                 data = Train,
                 type=  'C-classification',
                 kernel = 'radial') #for gaussian kernals

summary(classifier)
# Call:
#   svm(formula = Purchased ~ ., data = Train, type = "C-classification", kernel = "radial")
# 
# 
# Parameters:
#   SVM-Type:  C-classification 
# SVM-Kernel:  radial 
# cost:  1 
# gamma:  0.5 
# 
# Number of Support Vectors:  77
# 
# ( 39 38 )
# 
# 
# Number of Classes:  2 
# 
# Levels: 
#   0 1

y_pred= predict(classifier, newdata = Test[,-3])
y_pred

# 2   4   5   9  12  18  19  20  22  29  32  34  35  38  45  46  48  52  66  69  74  75 
# 0   0   0   0   0   1   1   1   0   0   1   0   0   0   0   0   0   0   0   0   1   0 
# 82  84  85  86  87  89 103 104 107 108 109 117 124 126 127 131 134 139 148 154 156 159 
# 0   0   0   1   0   0   0   1   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
# 162 163 170 175 176 193 199 200 208 213 224 226 228 229 230 234 236 237 239 241 255 264 
# 0   0   0   0   0   0   0   0   1   1   1   0   1   0   0   1   1   0   1   1   1   0 
# 265 266 273 274 281 286 292 299 302 305 307 310 316 324 326 332 339 341 343 347 353 363 
# 1   1   1   1   1   1   1   0   1   0   1   0   0   1   0   1   0   1   0   1   1   0 
# 364 367 368 369 372 373 380 383 389 392 395 400 
# 0   1   1   0   1   0   1   1   1   1   0   1 
# Levels: 0 1

cm= table(Test[,3], y_pred)
cm
y_pred
#   0  1
# 0 58  6
# 1  4 32

#Visualizing Train Set
library(ElemStatLearn)
set = Train
x1 = seq(min(set[,1])-1, max(set[,1])+1, by=0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, by=0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM Kernal for Training Data',
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