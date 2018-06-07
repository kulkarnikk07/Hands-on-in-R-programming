#PCA

dat = read.csv('Wine.csv')
#Customer Segment is dependant variable, all other 13 variables are independant

#data preprocessing
library(caTools)
set.seed(123)
split = sample.split(dat$Customer_Segment, SplitRatio = 0.8)

Train = subset(dat, split ==T)
Test  = subset(dat, split == F)

Train[,-14] = scale(Train[,-14])
Test[,-14] = scale(Test[,-14])

#PCA applied to remove the dependant variable with high variance or correlation
#install.packages("caret")
library(caret)

#install.packages("e1071")
library(e1071)

pca = preProcess(x= Train[,-14],method = 'pca', pcaComp = 2) #thresh is threshold to 60% variance
#pcaComp gives how many components to create here 2 out of 13, that will explain most variant

training_set = predict(pca, Train)
training_set = training_set[c(2,3,1)] #arranging dependant variable to the end


test_set = predict(pca, Test)
test_set = test_set[c(2,3,1)]


#Logistic Regression
classifier = svm(formula= Customer_Segment~.,
                 data= training_set,
                 type = 'C-classification',
                 kernel = 'linear')

y_pred = predict(classifier, newdata = test_set[-3])                 

y_pred

# 4   5   8  11  16  20  21  24  31  32  50  59  65  67  68  69  87  88  89 104 106 107 
# 1   1   1   1   1   1   1   1   1   1   1   1   2   2   2   2   2   2   2   2   2   2 
# 111 114 118 126 132 134 137 138 139 145 151 167 173 174 
# 2   2   2   2   3   3   3   3   3   3   3   3   3   3 
# Levels: 1 2 3

cm = table(test_set[,3], y_pred)

cm

# y_pred
#    1  2  3
# 1 12  0  0
# 2  0 14  0
# 3  0  0 10

#visualization for Trainig set with 3 levels in dependant variable
library(ElemStatLearn)
set = training_set
x1 = seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)
grid_set = expand.grid(x1, x2)
colnames(grid_set) = c('PC1','PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM for training_set Data of PCA',
     xlab = 'pc1', ylab = 'pc2',
     xlim = range(x1), ylim =range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add= T)
points(grid_set, pch = '.',col = ifelse(y_grid==2, 'deepskyblue',
                                        ifelse(y_grid == 1,'springgreen4', 'tomato')))
points(set, pch = 21,bg = ifelse(set[,3]==2, 'blue3',
                                 ifelse(set[,3]==1,'green4', 'red3')))

#for test set for 3 levels of dependant variable
library(ElemStatLearn)
set = test_set
x1 = seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)
grid_set = expand.grid(x1, x2)
colnames(grid_set) = c('PC1','PC2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM for test_set Data of PCA',
     xlab = 'pc1', ylab = 'pc2',
     xlim = range(x1), ylim =range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add= T)
points(grid_set, pch = '.',col = ifelse(y_grid==2,'deepskyblue',
                                        ifelse( y_grid == 1,'springgreen4', 'tomato')))
points(set, pch = 21,bg = ifelse(set[,3]==2,'blue3',
                                 ifelse(set[,3]==1,'green4', 'red3')))