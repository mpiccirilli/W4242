setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_2")
q2a <- read.csv(file="Assignment2_2a.csv",header=TRUE,sep=",")
require(ggplot2)
require(hexbin)
require(scales)
require(plyr)
q2a$sign_up <- as.Date(q2a$sign_up)

#Transform some of the numerical data into character
q2a$gender <- gsub("1","Male",q2a$gender)
q2a$gender <- gsub("0","Female",q2a$gender)
q2a$relation_status <- gsub("1","single",q2a$relation_status)
q2a$relation_status <- gsub("2","in a relationship",q2a$relation_status)
q2a$relation_status <- gsub("3","engaged",q2a$relation_status)
q2a$relation_status <- gsub("4","married",q2a$relation_status)


#What is the distibution of the number of friends?
ggplot(q2a, aes(x=totalfriends, fill=relation_status)) + geom_density(alpha=.3)
#Answer:
#A majority of the users have under 100 friends


#What's the relation between the number of friends and age?
ggplot(q2a, aes(x=age,y=totalfriends)) + geom_point() #this is just the points
ggplot(q2a, aes(x=age,y=totalfriends)) + geom_hex(bins=45) #This shows the ponts along with a count-index
coef(lm(totalfriends ~ age, data = q2a))
#Answer:
#As you can see in the graph, there are a lot of members within the age range of 15 - 30
#who have around 50 friends, howevre there are few members between that age range
#how have more than 100 friends


#Does age affect whether users move away from their hometown to live
#another city?
#Answer:
#I created a new column in the dataset to represent the absolute value of the distance
#each user has travelled from their hometown. 
q2a$absdistance <- abs(q2a$living - q2a$hometown)
qplot(factor(age), data=q2a, geom="bar", fill=factor(absdistance))
#A majorty of the users are represented by those who are in their mid-teens to about 30
#and most of which haven't moved away from home, but the second largest group
#of those users have moved 1 region away from their hometown.
#Most of the users outside of this age range are living in the region of their hometown. 
#And interestingly, everyone has either moved only by 1 region, or 5 regions, nothing in between. 


#Characterize the relation between age and relationship status.
ggplot(q2a, aes(x=factor(age),fill=relation_status)) + geom_bar() + coord_polar(theta="x")
#Answer:
#I've created a circular graph with ages of the users, and filled in the area on each age line
#with the count of the relationship status, in blocks of 200. 

#Draw scatter plots between the number of total actions, visits, posts and
#time spent.
pairs(q2a[,c(8,11,12,13)])


#How does the sign up date influence the number of new friends they made?
ggplot(q2a, aes(x=sign_up,y=total_new_friends))+geom_line(aes(color=gender)) + scale_x_date(breaks="6 months",labels=date_format("%b/%Y"))
#Answer: 
#This graph depicts the number of total new friends on the y-axis,
#paired with the sign_up date on the x-axis which is in 6 month increments on major lines, and 3 months on minor lines.
#Members who signed up between ~August 2009 and ~June 2013 all added about 12 friends in the past week.
#However members who joined between ~July 2013 and October 2013 added between 12 and 112 friends,
#with the higher number of adds closer to October 2013.

#Is there any relation between age and users' actions?
ggplot(q2a, aes(x=age,y=total_action)) + geom_point()
ggplot(q2a, aes(x=age,y=total_action)) + geom_hex(bins=40)
#A majority of the the high action users are between 15 and 30.
#However there are many more users between that age who have very few actions


#Do earlier users have more friends?
ggplot(q2a, aes(x=sign_up,y=totalfriends))+geom_line(aes(color=gender)) + scale_x_date(breaks="6 months",labels=date_format("%b/%Y"))
#Answer:  
#I'm not sure if I ran this graph correctly, because the line from beginning of time
#until present seems to be too predictable (too linear).  But based on this graph,
#you can see that there is a relationship between the number of friends people have and 
#when they signed up.  
#The members who signed up earlier have more friends than members who signed up in May 2013
#And while there is a linear trend downwards, there is a spike in total friends by users who signed up
#in ~September and October 2013. 


write.table(q2a,file="Assignment2_2b.csv",sep=",",row.names=F)