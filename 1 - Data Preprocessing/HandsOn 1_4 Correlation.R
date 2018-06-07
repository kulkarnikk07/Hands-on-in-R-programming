#correlation for continuous variables

dat= read.csv('database.csv')

#plot scatterplots
plot(dat$X2010.Deposits, dat$X2011.Deposits)
plot(dat$X2010.Deposits, dat$X2012.Deposits)

#matrix plot of scatterplot
pairs(dat) #plotting scatterplots inmatrix form

#correlation
cor(dat$Branch.Number, dat$Main.Office)


###packages used for correlation

#hmisc -> rcorr()

###colored correlation package
#GGally package -> ggcorr()
# plot(ggpairs()) gives correlation with scatterplot


