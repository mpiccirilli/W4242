setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_2")
code2b <- read.csv(file="Assignment2_2b.csv",header=TRUE,sep=",")
setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_2/social_network_dataset") #load tge original profile data
profile <- read.csv(file="profile.csv",header=TRUE,sep=",")
require(ggplot2)
require(Metrics)
require(e1071)
require(DAAG)
require(stats)

#rename the imported dataframe
df <- code2b
#Add in the categorical data from the original dataset
df$relation_status_num <- profile$relation_status
df$gender_num <- profile$gender


###########################################
#2.d.1: Build a model to predict the total number of action
#create a simple model using the total_visits
colnames(df)
df1 <- df[,c(8,11)] 
fit1 <- lm(total_action ~ total_visits, data=df1)
summary(fit1)
cv.lm(df = df1, form.lm = formula(total_action ~ total_visits))

#creating another seimple model but now using total_new_friends
df2 <- df[,c(8,14)] 
fit2 <- lm(total_action ~ total_new_friends, data=df2)
summary(fit2) #the r-sqr has improved dramatically
cv.lm(df = df, form.lm = formula(total_action ~ total_new_friends)) #we can see there is a better relationship between the modeled and actual data

#Since total_new_friends improved the prediciton, I will create a multiple regression
#using that variable along with total_time_spend
df3 <- df[,c(8,14, 13)]
fit3 <- lm(total_action ~ total_new_friends + total_time_spend, data=df3)
summary(fit3) #we see a continued improvement of r-sqr
cv.lm(df = df3, form.lm = formula(total_action ~ total_new_friends + total_time_spend))

#Since the total_time_spent is related to the total_visits, I want to see how
# much more predictive power total_visits adds onto the previous regression
df4 <- df[,c(8,14,13,11)]
fit4 <- lm(total_action ~ total_new_friends + total_time_spend + total_visits, data=df4)
summary(fit4) #there is an improvement, but it only improves the r-sqr by about .04
cv.lm(df = df4, form.lm = formula(total_action ~ total_time_spend + total_new_friends + total_visits))

#Since the total_time_spend and total_visits seem to have similar predictive power, 
#I decided to multiple the total_time_spend by the total_visits, and divide by 2
#I'm not quite sure if this is correct or good way to manipulate predictors,
#but as you will see, using this newly created predictor drastically increases the predictive powers
df5 <- df[,c(8,14,13,11)]
df5$combined <- (df$total_time_spend * df$total_visits)
cv.lm(df = df5, form.lm = formula(total_action ~ combined + total_new_friends + total_time_spend + total_visits))
fit5 <- lm(total_action ~ combined + total_new_friends +total_time_spend + total_visits, data=df5)
summary(fit5) #As you can see, a much higher r-sqr 


##############################################
#2.d.2: Run a k-means clustering 

#I will use the dataframe that is utilizing the created predictor to run k-means 
dfkc <- df5
dfkc$total_action <- NULL
dfkc$total_time_spend <- NULL
dfkc$total_visits <- NULL

#Run k-means and plot the data for each test
kc2 <- kmeans(dfkc,2)
plot(dfkc[c("total_new_friends","combined")], col=kc2$cluster)
points(kc2$centers[,c("total_new_friends", "combined")],col=3:4, pch=8, cex=3)
kc2

kc3 <- kmeans(dfkc,3)
plot(dfkc[c("total_new_friends","combined")], col=kc3$cluster)
points(kc3$centers[,c("total_new_friends", "combined")],col=1:3, pch=4, cex=3)
kc3

kc4 <- kmeans(dfkc,4)
plot(dfkc[c("total_new_friends","combined")], col=kc4$cluster)
points(kc4$centers[,c("total_new_friends", "combined")],col=5:8, pch=10, cex=3)

kc5 <- kmeans(dfkc,5)
plot(dfkc[c("total_new_friends","combined")], col=kc5$cluster)
points(kc5$centers[,c("total_new_friends", "combined")],col=1:5, pch=2, cex=6)
head(kc5)

kcbest <- FitKMeans(dfkc,max.clusters=20)
PlotHartigan(kcbest) #### new, re-run
kcbest

#In the range of 2-5Ks, the best number to use is 5.  After looking at the sum of 
#squares of each clustering, the best sum of squares result is from k=5. 
#However the best number of Ks to use is actually much larger then 5.
#Using Hartigan's rule, and running FitKMeans, I found that optimal number of means to use
#is somewhere in the neighborhood of 15-17.  It is around these numbers that the 
#Hartigan number drops significantly, and sometimes even turns negative. 



##################################
#2.d.3:
#The number of friends within each cluster can be seen by simply using the table() function
table(kc5$cluster)

#No you wouldn't be able to reproduce the same data without the user specific information
#Some of the data can cluster certain users together based on location, preferences, age, relationship status, 
# and other information that you wouldn't be able to get just out of get out of binary data.  

