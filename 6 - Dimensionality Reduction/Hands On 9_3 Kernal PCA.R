#Kernal PCA
#Non linear data 

#Importing the dataset
dat = read.csv('Social_Network_Ads.csv')
dat = dat[,3:5]

#Splitting the dataset
library(caTools)
set.seed(123)
split = sample.split(dat$Purchased, SplitRatio = 0.75)
train = subset(dat, split == T)
test = subset(dat, split== F)

#Feature Scaling
train[,-3] = scale(train[,-3])
test[,-3] = scale(test[,-3])

#install.packages("kernlab")
library(kernlab)
KPCA = kpca(~ ., train[,-3], kernel = 'rbfdot', features = 2 )
#~ . means all the data,rbfdot is for gaussian kernel, features gives number of components

train_pca = as.data.frame(predict(KPCA, train))
train_pca$Purchased = train$Purchased

test_pca = as.data.frame(predict(KPCA, test))
test_pca$Purchased = test$Purchased

#Fitting logistic regression on Train_pca
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = train_pca)
#Classification on test set

prob_pred = predict(classifier, type = 'response', newdata = test_pca[,-3])
y_pred = ifelse(prob_pred > 0.5,1,0)

cm = table(test_pca[,3], y_pred)
cm

# y_pred
#   0  1
# 0 57  7
# 1 10 26

#Visualization of the training set
install.packages("ElemStatLearn")
library(ElemStatLearn)
set= train_pca
X1 = seq(min(set[,1])-1, max(set[,1]+1), 0.01) # min value-1 and max value +1 to make age axis
X2 = seq(min(set[,2])-1, max(set[,2]+1), 0.01) # min value-1 and max value +1 to make estimatedSalary axis
grid_Set= expand.grid(X1, X2)
colnames(grid_Set) = c('V1', 'V2')
prob_set = predict(classifier, type = 'response', newdata = grid_Set)
y_grid = ifelse(prob_set >0.5, 1, 0)
plot(set[,-3],
     main = "Logistic Regression for Training Set (Kernal PCA)", 
     xlab= 'V1', ylab= 'V2', 
     xlim= range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid),length(X1), length(X2)), add= T)
points(grid_Set, pch = '.', col = ifelse(y_grid==1, 'springgreen3','tomato'))
points(set, pch = 21, bg = ifelse(set[,3]==1, 'green','red'))



install.packages("ElemStatLearn")
library(ElemStatLearn)
set= test_pca
X1 = seq(min(set[,1])-1, max(set[,1]+1), 0.01) # min value-1 and max value +1 to make age axis
X2 = seq(min(set[,2])-1, max(set[,2]+1), 0.01) # min value-1 and max value +1 to make estimatedSalary axis
grid_Set= expand.grid(X1, X2)
colnames(grid_Set) = c('V1', 'V2')
prob_set = predict(classifier, type = 'response', newdata = grid_Set)
y_grid = ifelse(prob_set >0.5, 1, 0)
plot(set[,-3],
     main = "Logistic Regression for Training Set (Kernal PCA)", 
     xlab= 'V1', ylab= 'V2', 
     xlim= range(X1), ylim = range(X2))

contour(X1, X2, matrix(as.numeric(y_grid),length(X1), length(X2)), add= T)
points(grid_Set, pch = '.', col = ifelse(y_grid==1, 'springgreen3','tomato'))
points(set, pch = 21, bg = ifelse(set[,3]==1, 'green','red'))
