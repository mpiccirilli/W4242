ufo <- df
head(df$Duration,10000)
sapply(ufo,class)
as.character(ufo$Date)

ufo$Date2 <- as.Date(ufo$Date, format="%m/%d/%y")
ufo$Date2

colnames(ufo)

require(lubridate)
ufo$Year <- year(ufo$Date2)
ufo$Month <- month(ufo$Date2)
#Summing the posts done per month
aggregate(Posted ~ Month, length, data=ufo)

write.csv(ufo, file="ufo.csv",row.names=FALSE)
getwd()

require(stringr)
load(url("http://www.jaredlander.com/data/presidents.rdata"))
head(presidents)
tail(presidents)
presidents <- presidents[1:64,]
tail(presidents)
presidents$YEAR
theTimes <- str_split(string=presidents$YEAR, pattern="-",n=2)
theTimes

load(url("http://www.jaredlander.com/data/warTimes.rdata"))
head(warTimes,10)
theTimes <- str_split(string=warTimes, pattern="(ACAEA)|-",n=2)
theTimes
theState <- sapply(theTimes, function(x) x[1]) #gets the first element of the list
theState

presidents$PRESIDENT
#finding any president with the word "John"
str_sub(string=presidents$PRESIDENT,start=1, end=4)
str_detect(string=presidents$PRESIDENT, pattern="John")
str_extract(string=presidents$PRESIDENT, pattern="John")
presidents$PRESIDENT[str_detect(string=presidents$PRESIDENT,pattern="John")] #use this to find all "john"
str_detect(string=presidents$PRESIDENT, pattern="john") #lowercase
str_detect(string=presidents$PRESIDENT, pattern=ignore.case("John")) #ignores case sensitivity
str_extract(string=theState, pattern="January")
head(theState,10)
str_extract(string=theState, pattern="[0-9][0-9][0-9][0-9]")
str_extract(string=theState, pattern="\\d{4}")
str_extract(string=theState, pattern="\\d{1,3}") #finds one, two or three digits
head(theState,20)
str_extract(string=theState, pattern="^\\d{4}")
str_extract(string=theState, pattern="\\d{4}$")
str_extract(string=theState, pattern="^\\d{4}$")

str_replace(string=theState, pattern="\\d",replacement="x") #replaces just the first
str_replace_all(string=theState, pattern="\\d",replacement="x") #replaces all the digits
str_replace(string=theState, pattern="\\d+",replacement="x") #replaces all the digits w/ one "x"

commands <- c("<a href=index.html> This is a link</a>", "<b> This is bold text</b>")
commands
#want to find just in between the tags, ex use of "wild-cards"
str_replace(string=commands, pattern="<.+?>(.+?)<.+?>",replacement="\\1") #find everything in between opening and closings brackets from an element and replaces it with whatever is in between those elements 
str_replace(string=commands[1], pattern="<.+?>(.+?)<.+?>",replacement="\\1") #returns just the first 
str_replace(string=commands, pattern="<.+?>(.+?)<(.+?)>",replacement="\\2")
