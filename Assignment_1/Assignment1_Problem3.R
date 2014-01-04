avepop <- read.csv("/Users/michaelpiccirilli/Desktop/Pop.csv", head=TRUE)
UFOpop <- merge(x=df, y=avepop, by=c("State"))

# Describe the dataset and why you chose it ----------
##avepop is simply the average population by state for the year for 2002 and 2006. 
###I chose this data set because it is simply the easiest dataset to work with 
####because we have to match the match the dataset from infochimps to the UFO, and this can be
#####matched up by the states

#Create 5 questions based on the joined data set and answer them --------
#1st Question: Which state has the most sightings of circle shape?
#2nd Question: How many sightings does NY have?
#3rd Question: What states has the minimum population?
#4rd Question: Which shape has the highest frequencies?
#5th Question: What is the mean population in terms of the Fireball shape?

plot(UFOpop$State,UFOpop$Year.x)


