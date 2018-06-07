#Naive Bayes Classification

#Bayes Theorem: P(A|B) = (P(B|A) * P(A))/ P(B)
#e.g. P(Defect| Mach2) = (P(Mach2|Defect) * (P(Defect))/ P(Mach2)

#P(Walks | X) = (P(X|Walks)*P(Walks)/p(X))

##Plan of attack (Steps to execute)
#Prior Probability: P(Walks) = Number of Walkers/ Total Observation =10/30
#Marginal Likelihood: P(X)= Number of similar observations/ Total Observation =4/30
#Likelihood: P(X|Walks)= Number of similar observations among those walk/ Total number of walker = 3/10
#Posterior Likelihood: P(Walks|X) =((3/10 * 10/30) / (4/30) ) = 0.75

#P(Drives | X) = (P(X|Drives)*P(Drives)/p(X)) 
#Prior Probability: P(Drives)= Number of drivers/ Total Observation= 20/30
#Marginal Likelihood: P(X)= Number of similar observations/ Total Observation= 4/30
#Likelihood: P(X|Drives)= Number of similar observations among those drive/ Total number of drivers=1/20
#Posterior Likelihood: P(Drives|X) = ((1/20 * 20/30) / (4/30) ) = 0.25

#Then compare Walks v/s Drives
# WalksP(Walks|X): 0.75
# DrivesP(Drives|X): 0.25

#As 0.75 > 0.25, hence the new point Walks


#1) Naive: Bayes theorem assumes independance of variables
#2) We can drop calculations of P(X) because it can become 0 and the whole equation infinite
#also, its common denominatoer for both equations of walks and drives
#while comapring,Walks v/s Drives = (P(X|Walks)*P(Walks)/p(X)) v/s (P(X|Drives)*P(Drives)/p(X)) 
#P(X|Walks)*P(Walks) v/s P(X|Drives)*P(Drives)

#3) More than two classes??? similar to 2 classes



#Read dataset
dat = read.csv('Social_Network_Ads.csv')
dat = dat[,3:5]

#Encoding the tearget feature as factor
dat$Purchased = factor(dat$Purchased, levels = c(0,1))

library(caTools)
split = sample.split(dat$Purchased, SplitRatio = 0.75)
train = subset(dat, split == T)
test = subset(dat, split == F)

train[,-3] = scale(train[,-3])
test[,-3] = scale(test[,-3])


#Fitting of training set with Naive Bayes
#install.packages('e1071')
library(e1071)
classifier = naiveBayes(x=train[,-3], y=train[,3])

y_pred = predict(classifier, newdata = test[,-3])

cm = table(test[, 3],y_pred)
cm

# y_pred
#   0  1
# 0 57  7
# 1  6 30

#Visualization of training dataset
library(ElemStatLearn)
set = train
x1 = seq(min(set[,1])-1, max(set[,1])+1, 0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, 0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age','EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'Naive Bayes for Training Set',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1),length(x2)), add =T)
points(grid_set, pch = '.', col= ifelse(y_grid ==1, 'springgreen3','tomato'))
points(set, pch =21, bg = ifelse(set[,3]==1, 'green4','red3'))


#Visualization of test dataset
library(ElemStatLearn)
set = test
x1 = seq(min(set[,1])-1, max(set[,1])+1, 0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, 0.01)
grid_set = expand.grid(x1,x2)
colnames(grid_set) = c('Age','EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'Naive Bayes for Test Set',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(x1), ylim = range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1),length(x2)), add =T)
points(grid_set, pch = '.', col= ifelse(y_grid ==1, 'springgreen3','tomato'))
points(set, pch =21, bg = ifelse(set[,3]==1, 'green4','red3'))
       