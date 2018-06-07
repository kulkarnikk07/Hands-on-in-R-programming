#Data Preprocessing
#Importing the dataset
dataset=read.csv('Data.csv')

#Exploring data
is.na(dataset)
head(dataset, n=3)
tail(dataset, n=5)
summary(dataset)
str(dataset)

install.packages(pastecs)
library(pastecs)
stat.desc(dataset)
#To find NA
any(is.na(dataset)) #returns T/F by checking any missing values 

colSums(is.na(dataset)) #to count the number of missing values in columns

rowSums(is.na(dataset)) #to count the number of missing values in columns


#Handling Missing values
#using mean
dataset$Age= ifelse(is.na(dataset$Age),
                    ave(dataset, FUN = function(x) mean(x,na.rm = T)))
#OR method
dataset$Age= ifelse(is.na(dataset$Age),
                    mean(dataset$Age, na.rm = T),
                    dataset$Age)

dataset$Salary=ifelse(is.na(dataset$Salary),
                      mean(dataset$Salary, na.rm = T),
                      dataset$Salary)
#row deletion 
rowdelete= na.omit(dataset) #delete rows contains NA
rowdelete
str(rowdelete)

complete.cases(dataset) #shows NA by rows in T/F; F= missing value


dataset[,5] = c(NA) #add NA to 5th Column
dataset[11,] = c(NA) #add NA to 11th Row

colnames(dataset)[c(5)] = "VarCol" #to rename the column
dataset1= dataset[-5] #Column delete;- sign used to delete the column
dataset2= dataset[-11,] #Row delete;- sign used to delete the row

newDat= dataset[-5]
newDat= newDat[-11,]

any(is.na(newDat))
sum(is.na(newDat)) #to get total no of NA in the dataframe
sum(is.na(newDat$Age)) #total no of NAs in Age column
sum(is.na(newDat$Salary)) #total no of NAs in Salary column
colSums(is.na(newDat))

newDat[,5]=c(1,2,3,NA,4,5,6,7,NA,8) #new column in vector from
newDat
newDat=na.omit(newDat[-5]) #to omit the colum containing NA

nrow(newDat) #to count the no of rows in a dataset
ncol(newDat) #to count the no of columns in a dataset

dat1=dataset[,colSums(is.na(dataset))<1] #check if NA less than 1 put in new data frame; cleaning by column
dat1

dat2=dataset[rowSums(is.na(dataset))<1,] #check if NA less than 1 put in new data frame; cleaning by row
dat2


