#k-Means Clustering
# Step1: Choose teh number K of clusters
# Step2: Select at random K points, the centroid 
# Step3: Assign each point to the closest centroid -> that forms K clusters
# Step4: Compute and place the new centroid of each cluster
# Step5: Reassign each data point to the new closest centroid. 
#        if any reassignment took place, go to STEP4, otherwise go to FIN 

#Read dataset
dat = read.csv('Mall_Customers.csv')
x = dat[,4:5] #selecting only columns to be clustered

#Using the elbow method to find the optimal number of cluster
set.seed(6)
wcss = vector()
for (i in 1:10) wcss[i] = sum(kmeans(x,i)$withinss)

wcss

# 269981.28 183653.33 106348.37  73880.64  44448.46  37233.81  34659.08  35094.27
# [9]  32385.31  25292.58

#Visualizing the elbow method 
plot(1:10, wcss, type = 'b', main = 'Kmeans Elbow Method',
     xlab = '# of Clusters', ylab = 'wcss')

#optimal k=5 as the graph becomes stable from 5th cluster

#Applying k means with optimal k
set.seed(29)
km = kmeans(x, 5, iter.max = 300, nstart = 10)
km

# K-means clustering with 5 clusters of sizes 22, 81, 23, 39, 35
# 
# Cluster means:
#   Annual.Income..k.. Spending.Score..1.100.
# 1           25.72727               79.36364
# 2           55.29630               49.51852
# 3           26.30435               20.91304
# 4           86.53846               82.12821
# 5           88.20000               17.11429
# 
# Clustering vector:
#   [1] 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3 1 3
# [42] 1 3 2 3 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [83] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [124] 4 5 4 2 4 5 4 5 4 2 4 5 4 5 4 5 4 5 4 2 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4
# [165] 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4
# 
# Within cluster sum of squares by cluster:
#   [1]  3519.455  9875.111  5098.696 13444.051 12511.143
# (between_SS / total_SS =  83.5 %)
# 
# Available components:
#   
#   [1] "cluster"      "centers"      "totss"        "withinss"     "tot.withinss"
# [6] "betweenss"    "size"         "iter"         "ifault"      
# >

#Visualizing the clusters
plot(x$Annual.Income..k..,x$Spending.Score..1.100., type = 'p',
     main = 'Plot for clusters', xlab = 'Annual', ylab = 'Spending Score')

install.packages("cluster")
library(cluster)

clusplot(x,
         km$cluster,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = 'Cluster of Dataset',
         ylab = 'Spending Score',
         xlab = 'Annual Income')