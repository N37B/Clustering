
#ids would really be a list of rating-program-gender combinations
ids<-1:100

#MADE TEST DATA
#qts would be actual average qts for these rating-program-genders
qts<-rnorm(100,60,10)
#pops would be the actual population that qualified for these RPGs
pops<-runif(100,0,1000)
#goals would be the annual goals for these RPGs
goals<-runif(100,0,300)

#scaled (normalized) test data
mydata<-data.frame(qts, pops, goals)
mydata<-scale(mydata)

#appended RPGs (ids) as row names (non-numeric data lables)
row.names(mydata)<-ids

# Determine number of clusters
#will determine the sum of squares within each group and sum
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
  }

#plots the sum of the groups sum of squares against the number of clusters...  the idea is to identify a bend in the plot
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 6) # 6 clusters per my chart knee bend

# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)

# append cluster assignment
mydata.kmean <- data.frame(mydata, fit$cluster) 

#hierarchical clustering
#build distance matrix
d <- dist(mydata, method = "euclidean")

#perform hierarchical clutstering on d
fit <- hclust(d, method="ward")

#show dendrogram
plot(fit)

#cut dendrogram in natrual spot, I chose 5
groups <- cutree(fit, k=5) 

#draw groups on dendrogram
rect.hclust(fit, k=5, border="red") 

mydata.hclust<-data.frame(mydata,groups)

#compare group assignments using the two different methods
head(mydata.hclust,10)
head(mydata.kmean,10)

