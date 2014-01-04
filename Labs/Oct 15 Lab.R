#k-means clustering


#adjacency graphs -- directed graphs, and undirected graphs


#build a function that samples 3k random rows
#build a function that calcs average degree
#build a function that calls functions 1 and 2
#call function 3, 10k times 
# -- options:  replicate, for loop, aaply, foreach

#Method II:  conditional row sum, >>> TRUE/FALSE >>> use as a multiplyer 



install.packages("RTextTools")

wine<- read.table("http://www.jaredlander.com/data/wine.csv", header=TRUE, sep=",",stringsAsFactors=FALSE)
require(useful)
corner(wine)

set.seed(278613)
wineK3 <- kmeans(x=wine,centers=3)
wineK3$cluster
wineK3$centers
View(wineK3$centers)
wineK3$totss #total sum of squares
wineK3$size
#how do you know how many clusters are correct?
#Hardigans rule, and the gap statistic

#Hartigans rule:
wineBest <- FitKMeans(x=wine,max.clusters=20,seed=278613)
wineBest
#if the Hartigan number is greater than 10, then use that many clusters (or at least it's better than using the number of clusters below it)


#The gap statistics:
require(cluster)
theGap <- clusGap(wine,K.max=20, FUNcluster=pam)
plot(theGap)
#measures how far apart your kmeans was from the theoretical correctness
#so the closest you get is with 5 clusters

#Medoids --- when you have categorical data
x <- c(1,4,5)
mean(x)
median(x)
pam #partition around medoids

winePam <- pam(x=wine, k=5) #k-medoids is resistant to outliers
plot(winePam)

#go over foreach loops
require(parallel)
require(doParallel)
cl <- makeCluster(2)
registerDoParallel(cl)
answer <- foreeach(i=1:100) %dopar%
{
  i*2
}
answer



