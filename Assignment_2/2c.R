setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_2/social_network_dataset")
linkage <- read.csv(file="Linkage.csv",header=TRUE, sep=",")
setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_2")
q2a <- read.csv(file="Assignment2_2a.csv",header=TRUE,sep=",")

#rename the imported data
linkdf <- linkage
#delete the 1st column, "X", which is simply the userid
linkdf$X <- NULL

#take a random sample
index <- sample(1:10000,3000)

#Take the average using Method I
m1 <- linkdf[index,]
m1sum <- sum(m1)
m1average = m1sum/3000
m1average

#Take the average using Method II
m2 <- linkdf[index,index]
m2sum <- sum(m2)
m2average = m2sum/3000
m2average

#Take the true average of the entire 10,000 x 10,000 matrix
trueaverage <- sum(linkdf)/10000
trueaverage

#The best sampling method, compared to the true average, is Method I. 
#Method I's average was nearly the the same as the true average of the original dataset.
#Method II is using a conditional summation, which further reduces the sampled population. 
# You're considering a 3000x3000 matrix instead of a 3000x10000 matrix.  Since the number of connections
#is reduced, the total number of connections, and therefore the average, is dramatically reduced.

#In fact, the aveerge using Method II is almost a perfect ratio to the reduced dataset
3000/10000
32.14533/109.235 #Method II / True average

