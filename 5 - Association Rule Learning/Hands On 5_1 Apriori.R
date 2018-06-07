#Association Rule - Apriori 
#People who baught something also baught something
#e.g diapers & beer, Movie 1 and Movie 2 on netflix, Bread & milk
#Market Basket Optimisation use for recommendation system
#Support, Confidence, and Lift
#Support: Number of customers buy Item I1 / total no of customers
#Confidence: Number of customers buy I1 and I2/ total number of customers buy I2
#Lift: confidence/ support

# Step1: Set a minimum support and confidence
# Step2: Tae all the subsets in transactions having higher suuport than minimum support
# Step3: Take all the rules of these subsets having higher confidence than inimum confidence
# Step4: Sort the rules by decresing lift


#Data preprocessing
dat = read.csv('Market_Basket_Optimisation.csv', header = F)

#The package Arules cannot take csv file for apriori, it needs sparse matrix which contains lot of zeros
#Transform dataset into sparse matrix, all the products in columns here 120 products
#Put 0 and 1 for the product if customer buys the item (1= Buys, 0= Does not buy)
#Creating sparse matrix and reading the data
install.packages("arules")
library(arules)

sparseMat = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = T)
#as csv file is comma seperated- sep= ',' 
#To remove duplicated values use- rm.duplicates
# distribution of transactions with duplicates:
# 1 
# 5 means: 5 rows have 1 duplicate values 
# summary(sparseMat)
# transactions as itemMatrix in sparse format with
# 7501 rows (elements/itemsets/transactions) and
# 119 columns (items) and a density of 0.03288973 #3% non zero values
# 
# most frequent items:
#   mineral water          eggs     spaghetti  french fries     chocolate       (Other) 
# 1788          1348          1306          1282          1229         22405 
# 
# element (itemset/transaction) length distribution:
#   sizes
# 1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   18 
# 1754 1358 1044  816  667  493  391  324  259  139  102   67   40   22   17    4    1 
# 19   20 
# 2    1 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   3.000   3.914   5.000  20.000 
# 
# includes extended item information - examples:
#   labels
# 1           almonds
# 2 antioxydant juice
# 3         asparagus


itemFrequencyPlot(sparseMat, topN = 10) #To visualize the top 10 items

#Training Apriori on Training Set
rules = apriori(data = sparseMat, parameter = list(support = 0.003, confidence = 0.4))

#Support: consider products are purchased three times a day, for weekly data 3*7 =21; hence the support = 21/7501= 0.0028 ~ 0.003
#Confidence: default 0.8, we will change it as per need

# Apriori #for support = 0.003, confidence = 0.8
# 
# Parameter specification:
#   confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target
# 0.8    0.1    1 none FALSE            TRUE       5   0.003      1     10  rules
# ext
# FALSE
# 
# Algorithmic control:
#   filter tree heap memopt load sort verbose
# 0.1 TRUE TRUE  FALSE TRUE    2    TRUE
# 
# Absolute minimum support count: 22 
# 
# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[119 item(s), 7501 transaction(s)] done [0.00s].
# sorting and recoding items ... [115 item(s)] done [0.00s].
# creating transaction tree ... done [0.00s].
# checking subsets of size 1 2 3 4 5 done [0.00s].
# writing ... [0 rule(s)] done [0.00s].
# creating S4 object  ... done [0.00s].

#We got 0 rules hence change confidence from high 0.8 to 0.4


# Apriori (support = 0.003, confidence = 0.4))
# 
# Parameter specification:
#   confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target
# 0.4    0.1    1 none FALSE            TRUE       5   0.003      1     10  rules
# ext
# FALSE
# 
# Algorithmic control:
#   filter tree heap memopt load sort verbose
# 0.1 TRUE TRUE  FALSE TRUE    2    TRUE
# 
# Absolute minimum support count: 22 
# 
# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[119 item(s), 7501 transaction(s)] done [0.00s].
# sorting and recoding items ... [115 item(s)] done [0.00s].
# creating transaction tree ... done [0.00s].
# checking subsets of size 1 2 3 4 5 done [0.02s].
# writing ... [281 rule(s)] done [0.00s].
# creating S4 object  ... done [0.00s].

#We got 281 rules

#Visualizing the rules to sort rules descending by life
inspect(rules[1:10])  #first 10 rules without sorting

# lhs                       rhs             support     confidence lift     count
# [1]  {strong cheese}        => {spaghetti}     0.003732836 0.4827586  2.772720 28   
# [2]  {strong cheese}        => {mineral water} 0.003199573 0.4137931  1.735941 24   
# [3]  {green beans}          => {spaghetti}     0.003466205 0.4000000  2.297397 26   
# [4]  {cider}                => {eggs}          0.004266098 0.4050633  2.253991 32   
# [5]  {nonfat milk}          => {mineral water} 0.005065991 0.4871795  2.043811 38   
# [6]  {extra dark chocolate} => {mineral water} 0.005732569 0.4777778  2.004369 43   
# [7]  {tomato sauce}         => {spaghetti}     0.006265831 0.4433962  2.546642 47   
# [8]  {tomato sauce}         => {mineral water} 0.005732569 0.4056604  1.701822 43   
# [9]  {light cream}          => {mineral water} 0.007332356 0.4700855  1.972098 55   
# [10] {protein bar}          => {mineral water} 0.007732302 0.4172662  1.750511 58

inspect(sort(rules, by = 'lift')[1:10])  #first 10 rules sorting with lift

# lhs                                            rhs                 support    
# [1]  {mineral water,whole wheat pasta}           => {olive oil}         0.003866151
# [2]  {spaghetti,tomato sauce}                    => {ground beef}       0.003066258
# [3]  {french fries,herb & pepper}                => {ground beef}       0.003199573
# [4]  {cereals,spaghetti}                         => {ground beef}       0.003066258
# [5]  {frozen vegetables,mineral water,soup}      => {milk}              0.003066258
# [6]  {chocolate,herb & pepper}                   => {ground beef}       0.003999467
# [7]  {chocolate,mineral water,shrimp}            => {frozen vegetables} 0.003199573
# [8]  {frozen vegetables,mineral water,olive oil} => {milk}              0.003332889
# [9]  {cereals,ground beef}                       => {spaghetti}         0.003066258
# [10] {frozen vegetables,soup}                    => {milk}              0.003999467
# confidence lift     count
# [1]  0.4027778  6.115863 29   
# [2]  0.4893617  4.980600 23   
# [3]  0.4615385  4.697422 24   
# [4]  0.4600000  4.681764 23   
# [5]  0.6052632  4.670863 23   
# [6]  0.4411765  4.490183 30   
# [7]  0.4210526  4.417225 24   
# [8]  0.5102041  3.937285 25   
# [9]  0.6764706  3.885303 23   
# [10] 0.5000000  3.858539 30   

#Lets try for confidence 0.2
rules = apriori(data = sparseMat, parameter = list(support = 0.003, confidence = 0.2))


# Apriori # (support = 0.003, confidence = 0.2)
# 
# Parameter specification:
#   confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target
# 0.2    0.1    1 none FALSE            TRUE       5   0.003      1     10  rules
# ext
# FALSE
# 
# Algorithmic control:
#   filter tree heap memopt load sort verbose
# 0.1 TRUE TRUE  FALSE TRUE    2    TRUE
# 
# Absolute minimum support count: 22 
# 
# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[119 item(s), 7501 transaction(s)] done [0.00s].
# sorting and recoding items ... [115 item(s)] done [0.00s].
# creating transaction tree ... done [0.00s].
# checking subsets of size 1 2 3 4 5 done [0.00s].
# writing ... [1348 rule(s)] done [0.00s].
# creating S4 object  ... done [0.01s].
#We got 1348 rules

inspect(sort(rules, by = 'lift')[1:10])


# lhs                                       rhs             support     confidence
# [1]  {mineral water,whole wheat pasta}      => {olive oil}     0.003866151 0.4027778 
# [2]  {frozen vegetables,milk,mineral water} => {soup}          0.003066258 0.2771084 
# [3]  {fromage blanc}                        => {honey}         0.003332889 0.2450980 
# [4]  {spaghetti,tomato sauce}               => {ground beef}   0.003066258 0.4893617 
# [5]  {light cream}                          => {chicken}       0.004532729 0.2905983 
# [6]  {pasta}                                => {escalope}      0.005865885 0.3728814 
# [7]  {french fries,herb & pepper}           => {ground beef}   0.003199573 0.4615385 
# [8]  {cereals,spaghetti}                    => {ground beef}   0.003066258 0.4600000 
# [9]  {frozen vegetables,mineral water,soup} => {milk}          0.003066258 0.6052632 
# [10] {french fries,ground beef}             => {herb & pepper} 0.003199573 0.2307692 
# lift     count
# [1]  6.115863 29   
# [2]  5.484407 23   
# [3]  5.164271 25   
# [4]  4.980600 23   
# [5]  4.843951 34   
# [6]  4.700812 44   
# [7]  4.697422 24   
# [8]  4.681764 23   
# [9]  4.670863 23   
# [10] 4.665768 24  


#Support: consider products are purchased four times a day, for weekly data 4*7 =28; hence the support = 28/7501= 0.0038 ~ 0.004
#Confidence: default 0.8, we will change it as per need
rules = apriori(data = sparseMat, parameter = list(support = 0.004, confidence = 0.2))

# Apriori
# 
# Parameter specification:
#   confidence minval smax arem  aval originalSupport maxtime support minlen maxlen target
# 0.2    0.1    1 none FALSE            TRUE       5   0.004      1     10  rules
# ext
# FALSE
# 
# Algorithmic control:
#   filter tree heap memopt load sort verbose
# 0.1 TRUE TRUE  FALSE TRUE    2    TRUE
# 
# Absolute minimum support count: 30 
# 
# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[119 item(s), 7501 transaction(s)] done [0.00s].
# sorting and recoding items ... [114 item(s)] done [0.00s].
# creating transaction tree ... done [0.00s].
# checking subsets of size 1 2 3 4 done [0.00s].
# writing ... [811 rule(s)] done [0.00s].
# creating S4 object  ... done [0.00s]

#Here 811 rules are created 
 
inspect(sort(rules, by= 'lift')[1:10])

# lhs                                            rhs             support    
# [1]  {light cream}                               => {chicken}       0.004532729
# [2]  {pasta}                                     => {escalope}      0.005865885
# [3]  {pasta}                                     => {shrimp}        0.005065991
# [4]  {eggs,ground beef}                          => {herb & pepper} 0.004132782
# [5]  {whole wheat pasta}                         => {olive oil}     0.007998933
# [6]  {herb & pepper,spaghetti}                   => {ground beef}   0.006399147
# [7]  {herb & pepper,mineral water}               => {ground beef}   0.006665778
# [8]  {tomato sauce}                              => {ground beef}   0.005332622
# [9]  {mushroom cream sauce}                      => {escalope}      0.005732569
# [10] {frozen vegetables,mineral water,spaghetti} => {ground beef}   0.004399413
# confidence lift     count
# [1]  0.2905983  4.843951 34   
# [2]  0.3728814  4.700812 44   
# [3]  0.3220339  4.506672 38   
# [4]  0.2066667  4.178455 31   
# [5]  0.2714932  4.122410 60   
# [6]  0.3934426  4.004360 48   
# [7]  0.3906250  3.975683 50   
# [8]  0.3773585  3.840659 40   
# [9]  0.3006993  3.790833 43   
# [10] 0.3666667  3.731841 33 