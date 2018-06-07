#LDA (Linear Descriminant Analysis)
#to reduce the classes of dependant variable
#supervized learning; dimennsion variable reduction; feature extraction

#Get the dataset
dat = read.csv('Wine.csv')

#Splitting the dataset
split = sample.split(dat$Customer_Segment, SplitRatio = 0.8)
train = subset(dat, split == T)
test = subset(dat, split == F)

#Feature Scaling
train[,-14] = scale(train[,-14])
test[,-14] = scale(test[,-14])

#Applying LDA
library(MASS)

LDA = lda(formula= Customer_Segment ~ .,data = train) 

LDA
# Call:
#   lda(Customer_Segment ~ ., data = train)
# 
# Prior probabilities of groups:
#   1         2         3 
# 0.3309859 0.4014085 0.2676056 
# 
# Group means:
#   Alcohol Malic_Acid        Ash Ash_Alcanity   Magnesium Total_Phenols  Flavanoids
# 1  0.9161577 -0.2678128  0.3169366   -0.7677017  0.47784046    0.86554406  0.96085973
# 2 -0.8673918 -0.2900538 -0.4351173    0.2757096 -0.37867047   -0.06843199  0.03881285
# 3  0.1679453  0.7663227  0.2606754    0.5359613 -0.02300749   -0.96789335 -1.24665105
# Nonflavanoid_Phenols Proanthocyanins Color_Intensity        Hue      OD280    Proline
# 1          -0.54551405      0.57875336       0.2232125  0.4678127  0.7784722  1.1859333
# 2           0.01926071      0.06299869      -0.8392806  0.3902892  0.2271249 -0.7133071
# 3           0.64582368     -0.81032455       0.9828422 -1.1640443 -1.3035345 -0.3968516
# 
# Coefficients of linear discriminants:
#   LD1         LD2
# Alcohol              -0.36049436  0.57274240
# Malic_Acid            0.06833970  0.38451554
# Ash                  -0.23106642  0.54732004
# Ash_Alcanity          0.65863686 -0.47632919
# Magnesium            -0.04490611  0.03905734
# Total_Phenols         0.48992633 -0.05896633
# Flavanoids           -1.67593124 -0.51413527
# Nonflavanoid_Phenols -0.22867324 -0.12997256
# Proanthocyanins      -0.03251450 -0.11937429
# Color_Intensity       0.68161932  0.75202629
# Hue                  -0.20329547 -0.29185292
# OD280                -0.81167012 -0.05662932
# Proline              -0.94792758  0.80427205
# 
# Proportion of trace:
#   LD1    LD2 
# 0.7111 0.2889

training_set = as.data.frame(predict(LDA, train))
training_set = training_set[c(5,6,1)] #removing unnecessary columns 2,3,4

test_set = as.data.frame(predict(LDA, test))
test_set = test_set[c(5,6,1)] #removing unnecessary columns 2,3,4

#Fitting LDA to the training data
library(e1071)
classifier = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel= 'linear')

#Predict  the results
y_pred = predict(classifier, test_set[,-3])
y_pred

# 18  25  26  28  29  30  38  45  53  55  58  59  61  64  66  78  79  84  85  86  95  99 
# 1   1   1   1   1   1   1   1   1   1   1   1   2   2   2   2   2   2   2   2   2   2 
# 108 117 124 126 131 137 141 147 150 151 158 163 167 169 
# 2   2   2   2   3   3   3   3   3   3   3   3   3   3 
# Levels: 1 2 3

cm = table(test_set[,3], y_pred)
cm

# y_pred
#   1  2  3
# 1 12  0  0
# 2  0 13  0
# 3  0  1 10

#visualization for Trainig set
library(ElemStatLearn)
set = training_set
x1 = seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)
grid_set = expand.grid(x1, x2)
colnames(grid_set) = c('x.LD1','x.LD2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM for training_set Data of LDA',
     xlab = 'x.LD1', ylab = 'x.LD2',
     xlim = range(x1), ylim =range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add= T)
points(grid_set, pch = '.',col = ifelse(y_grid==2, 'deepskyblue',
                                        ifelse(y_grid == 1,'springgreen4', 'tomato')))
points(set, pch = 21,bg = ifelse(set[,3]==2, 'blue3',
                                 ifelse(set[,3]==1,'green4', 'red3')))

#visualization for Test set
library(ElemStatLearn)
set = test_set
x1 = seq(min(set[,1])-1, max(set[,1])+1, by = 0.01)
x2 = seq(min(set[,2])-1, max(set[,2])+1, by = 0.01)
grid_set = expand.grid(x1, x2)
colnames(grid_set) = c('x.LD1','x.LD2')
y_grid = predict(classifier, newdata = grid_set)
plot(set[,-3],
     main = 'SVM for test_set Data of LDA',
     xlab = 'x.LD1', ylab = 'x.LD2',
     xlim = range(x1), ylim =range(x2))
contour(x1,x2, matrix(as.numeric(y_grid), length(x1), length(x2)), add= T)
points(grid_set, pch = '.',col = ifelse(y_grid==2, 'deepskyblue',
                                        ifelse(y_grid == 1,'springgreen4', 'tomato')))
points(set, pch = 21,bg = ifelse(set[,3]==2, 'blue3',
                                 ifelse(set[,3]==1,'green4', 'red3')))
