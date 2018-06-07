#k- nearest neighbors
# Step 1: Choose number of K
# Step 2: Take the K nearest neighbors of the new data point according to Euclian Distance
# Step 3: Among these K neighbors, count the number of data points in each category
# Step 4: Assign the dat apoint to the category where you counted teh most neighbors

#Euclidian dist = sqrt((X2-X1)^2 - (Y2-Y1)^2)

dat= read.csv('Social_Network_Ads.csv')
dat= dat[,3:5]

library(caTools)
set.seed(123)
split = sample.split(dat$Purchased, SplitRatio = 0.75)

train = subset(dat, split == T)
test = subset(dat, split == F)

train[,1:2] = scale(train[,1:2])
test[,1:2] =scale(test[,1:2])

#Fitting the kNN to training set
library(class)
y_pred = knn(train=train[,-3], test = test[,-3], 
             cl= train[,3] ,k=5)

y_pred
# 1] 0 0 0 0 0 1 1 1 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0
# [42] 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 1 0 0 1 0 0 0 1 1 0 1 1 1 1 1 1 1 0 0 0 1 0 0 1 0 1
# [83] 0 1 0 1 1 0 0 1 1 0 1 0 1 1 1 1 0 1
# Levels: 0 1

#confusion matrix
cm = table(test[,3], y_pred)
cm
# y_pred
#   0  1
# 0 59  5
# 1  6 30

#Visualization
library(ElemStatLearn)
set= train
X1 = seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
X2 = seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)
grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = knn(train=train[,-3], test = grid_set[,-3], cl = train[,3], k=5)
plot(set[,-3],
     main = 'k-NN plot for Purchase',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid),length(X1), length(X2)), add= T)
points(grid_set, pch = '.', col = ifelse(y_grid==1, 'springgreen3','tomato'))
points(set, pch = 21, bg = ifelse(set[,3]==1, 'green4','red3'))
     


