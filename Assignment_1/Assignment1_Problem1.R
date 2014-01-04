#import data by shape of the UFO
install.packages("XML")
require(XML)
fblink <- "http://www.nuforc.org/webreports/ndxsFireball.html"
cirlink <- "http://www.nuforc.org/webreports/ndxsCircle.html"
trilink <- "http://www.nuforc.org/webreports/ndxsTriangle.html"
fireballtable <- readHTMLTable(fblink, which=1, header=TRUE, stringsASFactors=FALSE)
circletable <- readHTMLTable(cirlink, which=1, header=TRUE, stringsASFactors=FALSE)
triangletable <- readHTMLTable(trilink, which=1, header=TRUE, stringsASFactors=FALSE)

#combine all the datasets
combinedtables <- rbind(circletable,fireballtable,triangletable)
df <- combinedtables  #rename it because "df" is shorter to spell

#Cleaning misc items out of the Duration column in the main dataset
df$Duration <- combinedtables$Duration
df$Duration <- gsub("\\~","",df$Duration)
df$Duration <- gsub(">","",df$Duration)
df$Duration <- gsub("<","",df$Duration)
df$Duration <- gsub("@","",df$Duration)
df$Duration <- gsub(".+-","",df$Duration)
df$Duration <- gsub("._","",df$Duration)
df$Duration <- gsub("Approx","",ignore.case=TRUE,df$Duration)
df$Duration <- gsub("aprox","", ignore.case=T,df$Duration)
df$Duration <- gsub("\\?+","",df$Duration)
df$Duration <- gsub(".+to","",df$Duration)
df$Duration <- gsub("few","3",df$Duration)
df$Duration <- gsub("at least", "",df$Duration)
df$Duration <- gsub("at lest","",df$Duration)
df$Duration <- gsub("min\\.","minutes",df$Duration)
df$Duration <- gsub("sec\\.","minutes",ignore.case=T,df$Duration)
df$Duration <- gsub("secs","seconds",ignore.case=T,df$Duration)
df$Duration <- gsub("secs\\.","seconds",ignore.case=T,df$Duration)
df$Duration <- gsub("about","",df$Duration)
df$Duration <- gsub("or","",df$Duration)
df$Duration <- gsub("so","",df$Duration)
df$Duration <- gsub("approx","",ignore.case=T,df$Duration)
df$Duration <- gsub("minutes","minutes",ignore.case=T,df$Duration)
df$Duration <- gsub("minutes\\.","minutes",ignore.case=T,df$Duration)
df$Duration <- gsub("minutes","temp1",ignore.case=T,df$Duration)
df$Duration <- gsub("min","minute",ignore.case=T,df$Duration)
df$Duration <- gsub("temp1","minutes",ignore.case=T,df$Duration)
df$Duration <- gsub("seconds","temp2",ignore.case=T,df$Duration)
df$Duration <- gsub("sec","seconds",ignore.case=T,df$Duration)
df$Duration <- gsub("temp2","seconds",ignore.case=T,df$Duration)
df$Duration <- gsub("minuteute","minutes",ignore.case=T,df$Duration)
df$Duration <- gsub("unknown","",ignore.case=T,df$Duration)
df$Duration <- gsub("hour","hour",ignore.case=T,df$Duration)


#break the somewhat cleaned data into 4 categories patterns, then convert into seconds

##### subsec -- takes rows with "sec" in the duration field
subsec <- grep("sec",df$Duration,value=T,ignore.case=T)

subsec <- as.data.frame(subsec)

subsec <- gsub("^[^0-9]+","",subsec)
subsec1 <- as.data.frame(subsec)

subsec <- gsub("[^0-9]+$","",subsec)
subsec2 <- as.data.frame(subsec)

#once the dataset is cleaned for only numbers, then return the location of the subset data
subseclocation <- grep("sec",df$Duration,value=F,ignore.case=T)
#load the cleaned data into the original dataset
df$Duration[subseclocation] <- subsec

##### submin -- takes rows with "min" in the Duration field
submin <- grep("min",df$Duration,value=T,ignore.case=T)
submin <- gsub("^[^0-9]+","",submin)
submin <- gsub("[^0-9]+$","",submin)
as.numeric(submin)*60
as.character(submin)
#once the dataset is cleaned for only numbers, then return the location of the subset data
subminlocation <- grep("min",df$Duration,value=F,ignore.case=T)
#load the cleaned data into the original dataset
df$Duration[subminlocation] <- submin

###### subhour -- takes rows with "hour" in the duration field
subhour <- grep("hour",df$Duration,value=T,ignore.case=T)
subhour <- gsub("^[^0-9]+","",subhour)
subhour <- gsub("[^0-9]+$","",subhour)
as.numeric(subhour)*60*60
as.character(subhour)
#once the dataset is cleaned for only numbers, then return the location of the subset data
subhourlocation <- grep("hour",df$Duration,value=F,ignore.case=T)
#load the cleaned data into the original dataset
df$Duration[subhourlocation] <- subhour

##### subhr -- takes rows with "hr", the abbreviated version of "hour" in the Duration field
subhr <- grep("hr",df$Duration,value=T,ignore.case=T)
subhr <- gsub("^[^0-9]+","",subhr)
subhr <- gsub("[^0-9]+$","",subhr)
as.numeric(subhr)*60*60
as.character(subhr)
#once the dataset is cleaned for only numbers, then return the location of the subset data
subhrlocation <- grep("hr",df$Duration,value=F,ignore.case=T)
#load the cleaned data into the original dataset
df$Duration[subhrlocation] <- subhr

#Convert all the numbers in the Duration column into numeric value which eliminates all misc
#or truly messy data
df$Duration <- as.numeric(df$Duration)

#############update on 10/25/13######################################

#rename the columns
colnames(df) <- c("Date","City","State","Shape","Duration","Summary","Poster")
sapply(df,class)



df$Date <- gsub("[0-9]{1,2}[:punct:]{1}[0-9]{1,2}","",df$Date)
class(df$Date)
df$Date <- as.Date(df$Date, "%m/%d/%y")
class(test$Time)



setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_3")
write.table(df,file="UFOData.csv",sep=",",row.names=F)
############## End update 10/25/13############################


sum(is.na(df$Duration)) #gives the number of omitted rows


#Question 1, Part 2

#How many sightings in Alaska?
##make a dataframe with only alaska, then aggregate

aggregate(Posted ~ State,length,data=df)
###Answer:  72

#How many sightings of duration of less than 2 minutes in NY?
#120 seconds in 2 minutes
lowduration <- df[df$Duration < 120, ]
aggregate(Posted ~ State,length,data=lowduration)
###Answer:  792


#The average duration of the fireball sightings?
##make dataframes for the total sightings and total duration, then take avg
##I know this is not the "best" way to do it, but it gets the job done
aggregate(Posted ~ Shape, df, length)
aggregate(Duration ~ Shape, df, sum)
totalfbsightings <- 6031+2
totalfbdur <- 58544.72+45
avgduration <- totalfbdur/totalfbsightings
avgduration
##Answer: 9.71154

#Which year has the maximum number of sightings?
#Create a new column for just the years
sapply(df,class)
as.character(df$"Date / Time")
df$Date2 <- as.Date(df$"Date / Time",format="%m/%d/%y")
require(lubridate)
df$Year <- year(df$Date2)
aggregate(Posted ~ Year, length, data=df)
#Answer: 2012

#During the December 2012, how many states witnessed UFO more than 30 times?
df$Month <- 

