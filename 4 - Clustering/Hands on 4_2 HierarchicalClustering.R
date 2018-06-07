#Hierarchical Clustering

# Two types
# 1. Agglomarative- bottom's up approach, combining multiple to 1 cluster
# 2. Divisive- One big cluster is divided into many clusters

# Agglomarative HC
# Step1: Make each data point a single point cluster -> that forms 
# Step2: Take the wo closest data points and make them one cluster ----> That forms N-1
# Step3: Take the two closest cluters and make them one cluster-----> That forms N-2
# Step4: Repear STEP 3 until there is only one cluster and then finish


#Distance between clusters:
# 1. Euclidian distancebet P1 and P2: squrt((X2-X1)^2 + (Y2-Y1)^2)
# 2. Closest points
# 3. Furthest points
# 4. Average distance  
# 5. Distance between centroids

#Step1 Reading the data
dat = read.csv('Mall_Customers.csv')
x = dat[,4:5]

#Step 2 Hierarchical Clustering using the dendogram to find optimal number of cluster
dendogram = hclust(dist(x, method = 'euclidean'),method = 'ward.D') 
#to find euclidian dist in each cluster
#ward.D gives the within cluster deviation minimum

plot(dendogram,
     main= 'Dendrogram',
     xlab = 'Customers',
     ylab = 'Euclidian Disatnce')

#Optimal solution is 5 clusters in Euc Dist 300 to 900

#Step 3 Fitting hierarchical clustering to data
hc = hclust(dist(x, method = 'euclidean'),method = 'ward.D') 
y_hc = cutree(hc, 5)

#Step 4 Visualizingthe clusters
install.packages("cluster")
library(cluster)

clusplot(x,
         y_hc,
         lines = 0,
         shade = T,
         color = T,
         labels = 2,
         plotchar = F,
         span = T,
         main = 'Cluster of Dataset using Hierarchical Clustering',
         ylab = 'Spending Score',
         xlab = 'Annual Income')