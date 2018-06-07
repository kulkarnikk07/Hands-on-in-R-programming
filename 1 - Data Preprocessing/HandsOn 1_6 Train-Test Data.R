#sPLITTING INTO TRAINING AND TESTING SETS
install.packages("caTools")
library(caTools)
df=read.csv('Data.csv')
set.seed(100)
SplitData= sample.split(df$Country, SplitRatio = 0.6) #splitratio to select training; dataset$select onecolumn
SplitData #T indicates part of trainig set, F for Test Data

TrainSet= subset(df, SplitData==T)
TrainSet

TestSet= subset(df, SplitData==F)
TestSet
