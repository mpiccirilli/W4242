connection<-file("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_1/news.txt", open='r')
newsdata<-readLines(connection)
close(connection)
newsdata_structure<-matrix(newsdata,ncol=8,byrow=T)
colnames(newsdata_structure) <- c("Title", "Summary", "Link", "Views", "Date", "Source", "Category", "Other")
news<- newsdata_structure[,-8]
news<-data.frame(news) 

#Which category of news is most popular?
##Aggregate by length
aggregate(Source ~ Category, length, data=news)
#Sport is the most popular


#How many business events happened during April to May 2011?
Bnews <- news[news$Field=="business",]
april <- Bnews[grep("Apr",Bnews$Date),]
may<- Bnews[grep("May",Bnews$Date),]
apr.count <- dim(april)[1]
may.count <- dim(may)[1]
Total<- may.count+apr.count
Total
#Answer: 3481 between April 2011 and May 2011


#How many news events come from usatoday.com?
usatoday <- grep("usatoday.com",news$Link,value=T)
length(usatoday)
#Answer:  10,110

#Which newspaper website has the longest title in average?
colnames(news)
usatoday<-news[grep("usatoday.com",news$Link),]
usatoday=as.character(usatoday$Title)
aveUSA<-mean(nchar(usatoday))

nyt<-news[grep("nytimes.com",news$Link),]
nyt<-as.character(nyt$Title)
aveNYT<-mean(nchar(nyt))

Reuters<-news[grep("reuters.com",news$Link),]
Reuters<-as.character(Reuters$Title)
aveReuters<-mean(nchar(Reuters))

list(aveUSA,aveNYT,aveReuters)
#usatoday is 49.48081
#nyt is 49.47588
#reuters is 48.97514
#Answer: usatoday has the longest average title