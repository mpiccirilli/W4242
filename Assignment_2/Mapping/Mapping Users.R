#Set your working directory to wherever you want to run the files from
setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_2")

#Load the dataset I created from Question 2a on the previous assignment
q2a <- read.csv(file="Assignment2_2a.csv",header=TRUE,sep=",")

#I've created a new mapping of the US States, 
#I replaced state names with region numbers
#It's based on the state map that comes preloaded in the "reshape2" package
#You can view the original file by using the following command: packagedmap <- map_data("state")
#Load my state map file from your current working directory:
state_map <- read.csv(file="state_map_by_group.csv",header=TRUE,sep=",")


#required packages:
require(plyr)
require(maps)
require(ggplot2)
require(reshape2)



################ summarize the data from the original dataset ##########
hometown <- q2a$hometown
hometowncount <- table(hometown)
hometowncount <- as.data.frame(hometowncount)
colnames(hometowncount)
userinfo <- hometowncount #create the new dataset for the mappings
colnames(userinfo) <- c("region", "users_by_hometown") #rename the columns

living <- q2a$living
livingcount <- table(living)
livingcount <- as.data.frame(livingcount)
colnames(livingcount)
userinfo$users_by_current_location <- livingcount$Freq

action <- q2a$total_action
avgactions <- aggregate(total_action~living,q2a,mean)
colnames(avgactions)
userinfo$avg_total_action <- avgactions$total_action

totfriends <- q2a$totalfriends
avgfriends <- aggregate(totfriends~living,q2a,mean)
colnames(avgfriends)
userinfo$avg_total_friends <- avgfriends$totfriends

subtime <- q2a$total_time_spend
avgtime <- aggregate(subtime~living,q2a,mean)
colnames(avgtime)
userinfo$avg_time_spent <- avgtime$subtime

subage <- q2a$age
averageage <- aggregate(subage~living,q2a,mean)
colnames(averageage)
userinfo$avg_age <- averageage$subage
#################### End of data summarization ###########################




################   Begin making individual maps  #####################
ggplot(userinfo, aes(map_id = region)) + geom_map(aes(fill = avg_total_action), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "lightblue", high = "black")
ggplot(userinfo, aes(map_id = region)) + geom_map(aes(fill = users_by_current_location), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "lightblue", high = "black")
ggplot(userinfo, aes(map_id = region)) + geom_map(aes(fill = users_by_hometown), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "lightblue", high = "black")
ggplot(userinfo, aes(map_id = region)) + geom_map(aes(fill = avg_total_friends), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "lightblue", high = "black")
ggplot(userinfo, aes(map_id = region)) + geom_map(aes(fill = avg_time_spent), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "lightblue", high = "black")
ggplot(userinfo, aes(map_id = region)) + geom_map(aes(fill = avg_age), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "lightblue", high = "black")

ggplot(userin, aes(map_id = region)) + geom_map(aes(fill = avg_age), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "lightblue", high = "black")
##### Create each map on one page #####
### First I benchmark the data so they're all comparable, the lowest value = 100
benchmarked_data <- userinfo

bench_hometown <- min(benchmarked_data[,2])
benchmarked_data$users_by_hometown <- (benchmarked_data$users_by_hometown/bench_hometown)*100

bench_location <- min(benchmarked_data[,3])
benchmarked_data$users_by_current_location <- (benchmarked_data$users_by_current_location/bench_location)*100

bench_actions <- min(benchmarked_data[,4])
benchmarked_data$avg_total_action <- (benchmarked_data$avg_total_action/bench_actions)*100

bench_totalfriends <- min(benchmarked_data[,5])
benchmarked_data$avg_total_friends <- (benchmarked_data$avg_total_friends/bench_totalfriends)*100

bench_avgtime <- min(benchmarked_data[,6])
benchmarked_data$avg_time_spent <- (benchmarked_data$avg_time_spent/bench_avgtime)*100

bench_avgage <- min(benchmarked_data[,7])
benchmarked_data$avg_age <- (benchmarked_data$avg_age/bench_avgage)*100

# now melt the data to create the map
benchmark_melt <- melt(benchmarked_data, id = 1)

#plot the maps on one page:
ggplot(benchmark_melt, aes(map_id = region)) + geom_map(aes(fill = value), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + facet_wrap( ~ variable) + scale_fill_gradient(low = "lightblue", high = "black")

