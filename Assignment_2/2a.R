setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_2/social_network_dataset")
mon <- read.csv(file="Monday.csv",header=TRUE, sep=",")
tue <- read.csv(file="Tuesday.csv",header=TRUE, sep=",")
wed <- read.csv(file="Wednesday.csv",header=TRUE, sep=",")
thur <- read.csv(file="Thursday.csv",header=TRUE, sep=",")
fri <- read.csv(file="Friday.csv",header=TRUE, sep=",")
sat <- read.csv(file="Saturday.csv",header=TRUE, sep=",")
sun <- read.csv(file="Sunday.csv",header=TRUE, sep=",")
fullweek <- rbind(mon,tue,wed,thur,fri,sat,sun)

#since there are 50 users missing in the "fullweek" of data, I am creating a 
#data frame numbered from 1 - 10,000, and will combine the full week with 
#this dataset, then reduce based on the "X" column, which is also the "userid"
dummydf <- data.frame(X=1:10000, number_visit=0,number_posts=0,time_spend=0,new_friend=0,userid=0)

#load in the the linkage file to extract the total number of friends for each user
linkage <- read.csv(file="Linkage.csv",header=TRUE, sep=",")

#add the totalfriend column to the dummydf
#sum the rows of the Linkage file, minus the X row-value, which is included in the sum
dummydf$totalfriends <- rowSums(linkage)-dummydf$X

#add the totalfriends column to the fullweek dataset
fullweek$totalfriends <- 0

#rbind the fullweek dataset and dummydf dataset
dummyandfullweek <- rbind(fullweek,dummydf)


#now combine dummydf with the fullweek data so as to include the 50 missing users 
#who apparently didn't log in that week
require(plyr)
completeDF <- ddply(dummyandfullweek,"X",numcolwise(sum))

#reset the userid to equal the "X" column
completeDF$userid <- completeDF$X

#rename the columns with the total(summed) data
colnames(completeDF) <- c("X","total_visits","total_posts","total_time_spend","total_new_friends","userid","totalfriends")


#read in the user profile data
userprofile <- read.csv(file="profile.csv",header=TRUE, sep=",")

#Merge the completeDF with the userprofile based on the X and/or userid
Q2a <- merge(userprofile,completeDF, by = "userid", all=TRUE)

#deleting unecessary columns in final dataset
Q2a$X.x <- NULL
Q2a$X.y <- NULL

#write to a CSV file
write.table(Q2a,file="Assignment2_2a.csv",sep=",",row.names=F)
