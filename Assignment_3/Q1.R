
########################## Question 1, Part a #######################

setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_3")
UFOData <- read.csv(file="NewUFO.csv",header=T,sep=",")


#required packages:
require(ggplot2)
require(plyr)
require(reshape2)
require(grid)
require(randomForest)
require(DAAG)
require(FNN)


#### Clean and structure the dataset ####

#change the date column to dates
UFOData$date <- as.Date(UFOData$date, "%m/%d/%y")
UFOData$post_date <- as.Date(UFOData$post_date, "%m/%d/%y")

#Change uppercase letters to lower
UFOData$shape <- gsub("C","c",UFOData$shape)
UFOData$shape <- gsub("F","f",UFOData$shape)
UFOData$shape <- gsub("T","t",UFOData$shape)

#substring the year into it's own column
UFOData$year <- substr(UFOData$date,1,4)
UFOData$year <- as.numeric(UFOData$year)

#substring the month into it's own column
UFOData$month <- substr(UFOData$date,6,7)
UFOData$month <- as.numeric(UFOData$month)

#get ride of data with year >2013 as it is probably unreliable
UFOData <- subset(UFOData, year<=2013)

#Remove empty state entries as well as entries made from foreign territories
UFOData <- subset(UFOData, 
                      state!=""&
                      state!="AB"&
                      state!="BC"&
                      state!="DC"&
                      state!="MB"&
                      state!="NB"&
                      state!="NF"&
                      state!="NS"&
                      state!="NT"&
                      state!="ON"&
                      state!="PE"&
                      state!="PQ"&
                      state!="PR"&
                      state!="QC"&
                      state!="SA"&
                      state!="SK"&
                      state!="VI"&
                      state!="YK"&
                      state!="YT")

#Update any upper/lower case inconsistencies 
UFOData$state <- gsub("Ca","CA",UFOData$state)

#Transform the NAs in the duration column to zero
UFOData$duration[is.na(UFOData$duration)] <- 0

UFOData$state_index <- match(UFOData$state, unique(UFOData$state))
UFOData$shape_index <- match(UFOData$shape, unique(UFOData$shape))


#classify the part of the day
UFOData$Day_Night <- UFOData$time
UFOData$Day_Night <- substr(UFOData$time,1,2)
UFOData$Day_Night <- gsub("00","Night",UFOData$Day_Night)
UFOData$Day_Night <- gsub("23","Night",UFOData$Day_Night)
UFOData$Day_Night <- gsub("22","Night",UFOData$Day_Night)
UFOData$Day_Night <- gsub("21","Night",UFOData$Day_Night)
UFOData$Day_Night <- gsub("20","Evening",UFOData$Day_Night)
UFOData$Day_Night <- gsub("19","Evening",UFOData$Day_Night)
UFOData$Day_Night <- gsub("18","Evening",UFOData$Day_Night)
UFOData$Day_Night <- gsub("17","Evening",UFOData$Day_Night)
UFOData$Day_Night <- gsub("16","Afternoon",UFOData$Day_Night)
UFOData$Day_Night <- gsub("15","Afternoon",UFOData$Day_Night)
UFOData$Day_Night <- gsub("14","Afternoon",UFOData$Day_Night)
UFOData$Day_Night <- gsub("13","Afternoon",UFOData$Day_Night)
UFOData$Day_Night <- gsub("12","Afternoon",UFOData$Day_Night)
UFOData$Day_Night <- gsub("11","Morning",UFOData$Day_Night)
UFOData$Day_Night <- gsub("10","Morning",UFOData$Day_Night)
UFOData$Day_Night <- gsub("09","Morning",UFOData$Day_Night)
UFOData$Day_Night <- gsub("08","Morning",UFOData$Day_Night)
UFOData$Day_Night <- gsub("07","Morning",UFOData$Day_Night)
UFOData$Day_Night <- gsub("06","Morning",UFOData$Day_Night)
UFOData$Day_Night <- gsub("05","Morning",UFOData$Day_Night)
UFOData$Day_Night <- gsub("04","Morning",UFOData$Day_Night)
UFOData$Day_Night <- gsub("03","Night",UFOData$Day_Night)
UFOData$Day_Night <- gsub("02","Night",UFOData$Day_Night)
UFOData$Day_Night <- gsub("01","Night",UFOData$Day_Night)

UFOData$time_index <- match(UFOData$Day_Night, unique(UFOData$Day_Night))


#index the season
#1 = summer
#2 = fall
#3 = winter
#4 = spring
table(UFOData$season)
UFOData$season <- UFOData$month
UFOData$season <- gsub("12","winter",UFOData$season)
UFOData$season <- gsub("11","fall",UFOData$season)
UFOData$season <- gsub("10","fall",UFOData$season)
UFOData$season <- gsub("9","fall",UFOData$season)
UFOData$season <- gsub("8","summer",UFOData$season)
UFOData$season <- gsub("7","summer",UFOData$season)
UFOData$season <- gsub("6","summer",UFOData$season)
UFOData$season <- gsub("5","spring",UFOData$season)
UFOData$season <- gsub("4","spring",UFOData$season)
UFOData$season <- gsub("3","spring",UFOData$season)
UFOData$season <- gsub("2","winter",UFOData$season)
UFOData$season <- gsub("1","winter",UFOData$season)

UFOData$season_index <- match(UFOData$season, unique(UFOData$season))

#### End of data cleaning and structuring ####



#### Begin answering bullets #####

#Create a boxplot based on the shapes
summary(UFOData$duration)

ggplot(UFOData, aes(y=duration,x=shape,fill=shape)) +  ylim(0, 1000) + geom_boxplot()

#Create a time series figure with the number of sightings per year:
#First create a subset of only the shapes and years
#then assign that dataset to a table to acquire the frequency for each shape
shape_year_sub <- subset(UFOData,shape=="circle"|shape=="fireball"|shape=="triangle", select=c(shape,year))
timeseries_table <- table(shape_year_sub)
timeseries_table <- as.data.frame(timeseries_table)
ggplot(timeseries_table,aes(x=year,y=Freq,colour=shape,group=shape)) + geom_line() + theme(axis.text.x = element_text(colour = 'black', angle = 45, size = 8, hjust = .5, vjust = .5),axis.title.x=element_blank())

#Create a bar chart for sightings by state:
ggplot(UFOData, aes(state, fill=shape)) + geom_bar(position="dodge")

#Create a custom plot to explore an interesting aspect of the data
#What months do the most sightings occur? 
ggplot(UFOData, aes(x=factor(month),fill=Day_Night)) + geom_bar() + coord_polar(theta="x")


########################## End Part A #######################



###################### Begin Part B ##############################

#normalize the data by state
shape_state_sub <- subset(UFOData,shape=="circle"|shape=="fireball"|shape=="triangle", select=c(shape,state))
state_table <- table(shape_state_sub)
state_table <- as.data.frame(state_table)

#Get the state index to include in the prediction
state_index_sub <- subset(UFOData,select=c(state,state_index))
state_index_table <- table(state_index_sub)
state_index_table <- as.data.frame(state_index_table)
state_index_table <- subset(state_index_table,Freq!=0)
state_index_table$Freq <- NULL


#aggregate all the state sightings by suming frequency
state_freq <- aggregate(Freq~state,state_table,sum)

#import the census data:
setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_3")
population_data <- read.csv(file="population_data.csv",header=T,sep=",")
sapply(population_data,class)
population_data$state <- as.character(population_data$state)

#merge the UFO sightings by state with the population data
sightings_population <- merge(state_freq,population_data, by.x="state",by.y="state")
sapply(sightings_population,class)

#add a normalization column to to the merged dataset and update colnames
sightings_population$normalized_sightings <- (sightings_population$Freq/sightings_population$population)*100
colnames(sightings_population) <- c("state","freq","region","population","group","normalized_sightings")

summary(sightings_population)
#Observation: Once the sightings are normalized to the population you can see there is 
##a very small percentage of sightings per population.  The max percentage of sightings
###per population is .01625%.



#Visualization of the distributions: 
#I used the pre-packaged map from reshape2 which has all the US state lat/long info
#I then refernce the merged sightings_population dataset 
state_map <- map_data("state")


#I create a visualization with two maps on it. You must have "grid" package loaded
#This is the setup to plot both ggplot maps on the same page
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
plot1 <- ggplot(sightings_population, aes(map_id = region)) + geom_map(aes(fill = population), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "lightblue", high = "black")
plot2 <- ggplot(sightings_population, aes(map_id = region)) + geom_map(aes(fill = normalized_sightings), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "lightblue", high = "black")
pushViewport(viewport(layout = grid.layout(2, 1)))
print(plot2, vp = vplayout(1, 1))
print(plot1, vp = vplayout(2, 1))
rm(vplayout) #this removes the vplayout function 
#After plotting the information on the maps
#it's interesting to see that states with a high percentage of 
#sightings are more northern states or western states. 
ggplot(sightings_population, aes(map_id = region)) + geom_map(aes(fill = freq), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "lightblue", high = "black")

#Now explore the data and create two additional questions beyond the basic
#data analysis above. 

#1)  What is the distribution of the length of sightings? 
#ie, what states had the longest/shortest sightings? 
#
shape_state_duration <- subset(UFOData,shape=="circle"|shape=="fireball"|shape=="triangle", select=c(shape,state,duration))
duration_aggregate <- aggregate(duration~shape+state,shape_state_duration,sum)
#delete rows with no listed state

sub_aggregate <- aggregate(duration~state,duration_aggregate,sum)
#convert seconds to hours
sub_aggregate$duration <- (sub_aggregate$duration/60)/60
sub_aggregate$region <- sightings_population$region

ggplot(sub_aggregate, aes(map_id = region)) + geom_map(aes(fill = duration), map = state_map) + expand_limits(x = state_map$long, y = state_map$lat) + scale_fill_gradient(low = "antiquewhite", high = "black")
#as you can see, the people of california have experienced
#by far the longest average sightings per person


#2) What shapes are seen at what part of the day? 
ggplot(UFOData, aes(x=Day_Night,fill=shape)) + geom_bar(position="dodge")


############################### End Part B #########################



###################### Begin Part C ##############################
#Build a classifer to predict the shape of the UFO

#I have already indexed the shapes of the UFOS while cleaning and structuring
#I'm going to break the dataset down into 3 different subsets, one for each shape
#Then I'll build a classifer and use it on each of the datasets


#subset of predicting the shapes with standard linear model
linreg <- UFOData

#standard linear model for each shape 
#circle, shape index number = 1
linreg_c <- linreg 
linreg_c$shape_index <- gsub("2","0",linreg_c$shape_index)
linreg_c$shape_index <- gsub("3","0",linreg_c$shape_index)
circlefit <- lm(shape_index ~ duration+season_index+year+state_index+time_index, data=linreg_c)
summary(circlefit)
cv.lm(df = linreg_c, form.lm = formula(shape_index ~ duration+season_index+year+state_index+time_index))

#lin reg fireball
#shape index # = 3
linreg_f <- linreg 
linreg_f$shape_index <- gsub("1","0",linreg_f$shape_index)
linreg_f$shape_index <- gsub("2","0",linreg_f$shape_index)
fireballfit <- lm(shape_index ~ duration+season_index+year+state_index+time_index, data=linreg_f)
summary(fireballfit)
cv.lm(df = linreg_f, form.lm = formula(shape_index ~ duration+month+year+state_index))

#lin reg triangle
#shape index # = 2
linreg_t <- linreg 
linreg_t$shape_index <- gsub("1","0",linreg_t$shape_index)
linreg_t$shape_index <- gsub("3","0",linreg_t$shape_index)
trianglefit <- lm(shape_index ~ duration+season_index+year+state_index+time_index, data=linreg_t)
summary(trianglefit)
cv.lm(df = linreg_t, form.lm = formula(shape_index ~ duration+season_index+year+state_index+time_index))


####change to a randomForest, 

circleForest <- randomForest(shape_index ~ season_index+time_index+duration+state_index+month, data=circle_prediction, importance=TRUE, do.trace=100)
print(circleForest)


#create test set
circle_test <- linreg_c
circle_test$shape_index <- NULL


c_prediction <- predict(circleForest,circle_test)
c_prediction <- as.data.frame(c_prediction)
#variance explained was so high, I'm just takeing 1-prediction
c_prediction$c_prediction <- 1-c_prediction$c_prediction
c_prediction$c_prediction <- round(c_prediction$c_prediction)



#regress shapes with the state
fit1 <- lm(shape_index ~ state_index, data=UFOData)
summary(fit1)
cv.lm(df = UFOData, form.lm = formula(shape_index ~ state_index))

#shape by duration
fit2 <- lm(shape_index ~ duration+state_index, data=UFOData)
summary(fit2)
cv.lm(df = UFOData, form.lm = formula(shape_index ~ duration+state_index))

fit3 <- lm(shape_index ~ duration+month+year+state_index, data=UFOData)
summary(fit3)
cv.lm(df = UFOData, form.lm = formula(shape_index ~ duration+month+year+state_index))

#predict the shape based 
shape_predict <- predict(fit3,UFOData)
shape_predict <- round(shape_predict)
shape_predict <- as.data.frame(shape_predict)

UFOpredict <- UFOData
UFOpredict$shape_predict <- shape_predict$shape_predict

####test area
pop_index_group <- merge(state_index_table,population_data,by="state")
colnames(pop_index_group) <- c("state","state_index","region","population","group")

city_table <- table(UFOData$city)
city_table <- as.data.frame(city_table)
city_table <- city_table[order(-city_table$Freq),]




