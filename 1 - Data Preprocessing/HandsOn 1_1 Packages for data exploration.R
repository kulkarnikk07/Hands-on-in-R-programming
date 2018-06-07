#Hmisc and Psych packages

dat=read.csv('Data.csv')

#instead of library function after installing, we can package-> search package-> tick on box the package to use it

install.packages("Hmisc") #to describe categorical data; frequency, misising values, proportion, distinct value, GMD(Generalise Minimum Distance)
library(Hmisc)
describe(dat)
describe(dat)
describe(dat$Country) #categorical
describe(dat$Age) #numerical
describe(dat$Purchased) #categorical

dat$Age = with(dat,impute(dat$Age,mean,imputed=T)) #calculate mean of the missing value on column and 'with' function replaces missing values with the mean

dat$Salary= impute(dat$Salary) #calculate median of the missing value on column; median is default
dat
dat$Purchased= impute(dat$Purchased, mode)
dat

#aregImpute function to prdict the value of missing data
dat= read.csv('Data.csv')
#variable= aregImpute(~linear regression formula; for continuous variable us I(), 
#                    data= datafile to be used)



#psych package data exploration

install.packages("psych") #to describe numerical data; * indicates categorical variable; descriptive statistics
library(psych)
describe(dat)
describe(dat$Country) ##throws an error as not work on categorical
describe(dat$Age)
describeBy(dat$Age, group = dat$Purchased)
describeBy(dat$Salary, group = dat$Country) #Numerical stat by grouping categorical ie categorical descriptive statistics

describeData(dat) #summary statistics of dataset; H1-H4= Top 4 records; T1-T4= Bottom 4 records


#MICE package for data explorationa and missing data handling

#MICE - Multivariated Imputation via Chained Equation

#By default, for continuous missing values variables= Linear regression;
#for categorical missing values variables= Logistic regression

#Methods used by package:
#Numerical variable: PMM (Predictive Mean Matching)
#Binary variable (With 2 levels): logreg (Logistic Regression)
#Factor Variables (>= 2 levels): polyreg (Bayesian Polytomous Regression)
#Factor Variables (ordered, >= 2 levels): Proportional odds model

dat= read.csv('database.csv')

install.packages("mice")
library(mice)
md.pattern(dat) #mising value pattern by row, columns and combinations of two or more columns

install.packages("VIM")
library(VIM)
mice_plot <- aggr(dat, col=c('navyblue','yellow'),
                    numbers=TRUE, sortVars=TRUE,
                    labels=names(dat), cex.axis=.7,
                    gap=3, ylab=c("Missing data","Pattern"))


install.packages("Amelia")
library(Amelia)

amelia_plot = missmap(dat,col=c('navyblue','yellow'), 
                      main ="Missing Value Imputation")
