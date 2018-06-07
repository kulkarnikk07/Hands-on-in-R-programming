#Duplicates removing

dat=read.csv('Dirty.csv') 

duplicated(dat) #T means duplicated row
which(duplicated(dat)) #gives incidents in row of duplicate values
distinct_dat_info= dat[which(duplicated(dat)),] #indicating duplicates from dataset
distinct_dat= dat[-which(duplicated(dat)),] #removing duplicates

# use unique
data_Clean=unique(dat)

#Use package dplyr to remove duplicates
install.packages("dplyr")
library(dplyr)
clean_Data= distinct(dat)

#pivot table package
install.packages("rpivotTable")
library(rpivotTable)

rpivotTable(dat)
