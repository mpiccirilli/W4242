##### Required packages #####
require(plyr)
require(rjson)
require(useful)
require(RTextTools)
require(caret)
require(stats)

#####Run queries for Arts, Business, Obiuaries, Sports, and World sections#####

### Arts Query ###
ArtsQuery <- "http://api.nytimes.com/svc/search/v1/article?query=nytd_section_facet:[Arts]&fields=url,title,body,date,nytd_section_facet&offset=%s&rank=newest&api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311"
ArtsResults <- vector("list",200)

for(i in 0:199)
{
  AtempURL <- sprintf(ArtsQuery, i)
  AtempResult <- fromJSON(file=AtempURL)
  ArtsResults[[i+1]] <- ldply(AtempResult$results, as.data.frame)
}
ArtsResultsDF <- ldply(ArtsResults)
ArtsResultsDF <- subset(ArtsResultsDF, nytd_section_facet=="Arts")
ArtsResultsDF$desk_facet <- "Arts/Culture"
ArtsResultsDF <- ArtsResultsDF[,c(1,2,6,3,4,5)]


### Business Query ###
BusinessQuery <- "http://api.nytimes.com/svc/search/v1/article?query=nytd_section_facet:[Business]&fields=url,title,body,date,desk_facet,nytd_section_facet&offset=%s&rank=newest&api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311"
BusinessResults <- vector("list",200)

for(i in 0:199)
{
  BtempURL <- sprintf(BusinessQuery, i)
  BtempResult <- fromJSON(file=BtempURL)
  BusinessResults[[i+1]] <- ldply(BtempResult$results, as.data.frame)
}
BusinessResultsDF <- ldply(BusinessResults)
BusinessResultsDF <- subset(BusinessResultsDF, nytd_section_facet=="Business")


### Obituary Query ###
ObituaryQuery <- "http://api.nytimes.com/svc/search/v1/article?query=nytd_section_facet:[Obituaries]&fields=url,title,body,date,desk_facet,nytd_section_facet&offset=%s&rank=newest&api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311"
ObituaryResults <- vector("list",200)

for(i in 0:199)
{
  OtempURL <- sprintf(ObituaryQuery, i)
  OtempResult <- fromJSON(file=OtempURL)
  ObituaryResults[[i+1]] <- ldply(OtempResult$results, as.data.frame)
}
ObituaryResultsDF <- ldply(ObituaryResults)
ObituaryResultsDF <- subset(ObituaryResultsDF, nytd_section_facet=="Obituaries")


### Sports Query ###
SportsQuery <- "http://api.nytimes.com/svc/search/v1/article?query=nytd_section_facet:[Sports]&fields=url,title,body,date,desk_facet,nytd_section_facet&offset=%s&rank=newest&api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311"
SportsResults <- vector("list",200)

for(i in 0:199)
{
  StempURL <- sprintf(SportsQuery, i)
  StempResult <- fromJSON(file=StempURL)
  SportsResults[[i+1]] <- ldply(StempResult$results, as.data.frame)
}
SportsResultsDF <- ldply(SportsResults)
SportsResultsDF <- subset(SportsResultsDF, nytd_section_facet=="Sports")


### World Query ###
WorldQuery <- "http://api.nytimes.com/svc/search/v1/article?query=nytd_section_facet:[World]&fields=url,title,body,date,desk_facet,nytd_section_facet&offset=%s&rank=newest&api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311"
WorldResults <- vector("list",200)

for(i in 0:199)
{
  WtempURL <- sprintf(WorldQuery, i)
  WtempResult <- fromJSON(file=WtempURL)
  WorldResults[[i+1]] <- ldply(WtempResult$results, as.data.frame)
}
WorldResultsDF <- ldply(WorldResults)
WorldResultsDF <- subset(WorldResultsDF, nytd_section_facet=="World")

# combine the all the queries #
colnames(ArtsResultsDF)
colnames(BusinessResultsDF)
colnames(ObituaryResultsDF)
colnames(SportsResultsDF)
colnames(WorldResultsDF)
AllResults <- rbind(ArtsResultsDF,BusinessResultsDF,ObituaryResultsDF,SportsResultsDF,WorldResultsDF)




##### Write the file to the current working directory ####
setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_4")
AllResults <- AllResults[,c(6,5,1,4)]
write.table(AllResults,file="AllResults.csv",sep="\t",row.names=F)
rm(list=ls(all=TRUE))

##### Load in the file with all the results ####
setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_4")
AllResults <- read.table("AllResults.csv",header=T,sep="\t", stringsAsFactors = FALSE)

##### Combined Title and Body #####
AllResults$text <- do.call(paste, c(AllResults[c("body", "title")], sep = ""))



##### Create a document term matrix #####

# Since creating a DTM based on the full 10k observations crashing my computer
# I will create a smaller random sample and run the naiveBayes 
# on the smaller random sample
# The code to run on the full 10k come after the nB of the sample at the bottom of the script
sample.size <- floor(.05 * nrow(AllResults))
sample.index <- sample(nrow(AllResults),size=sample.size)
sample <- AllResults[sample.index, ]

master.doc.matrix <- create_matrix(sample$text,language = "english",removeNumbers=TRUE, removeStopwords = TRUE, stemWords=TRUE, removePunctuation=TRUE, toLower=TRUE)
master.matrix <- as.matrix(master.doc.matrix)
master.matrix.for.later <- master.matrix
master.matrix <- apply(as.matrix(master.matrix),2,as.character)
master.matrix[master.matrix>1] <- 1
master.matrix.df <- as.data.frame(master.matrix)

master.classifer <- as.numeric(as.factor(sample$nytd_section_facet))
master.classifer <- as.data.frame(master.classifer)
master.classifer$master.classifer <- as.factor(master.classifer$master.classifer)

master.matrix.df$class.label <- master.classifer$master.classifer

#naiveBayes model
nb.model <- naiveBayes(class.label~., data=master.matrix.df,subset=1:250)


#modeling against the training set itself to see time usage
system.time(nb.train.predict <- predict(nb.model,master.matrix.df[1:250,]))
confusionMatrix(nb.train.predict,master.matrix.df[1:250,5943])
#user time: 117.515
#system time: 24.613
#elapsed time: 141.973

#modeling against test set, we can see it has taken longer to run
system.time(nb.test.predict1 <- predict(nb.model,master.matrix.df[251:500,]))
confusionMatrix(nb.test.predict1,master.matrix.df[251:500,5943])
#user time: 120.039
#system time: 25.637
#elapsed time: 145.527

##### Observations of naiveBayes prediction ##### 
#the higher the sensitivity of each Class, the better predictive
#power the naiveBayes model had on the test dataset
#When sensitivity was low, the positive predivitive value dropped significantly
#however the Neg Pred Value was still high

#Based on the confusionMatrix, we can see that the class with the highest
#sucess of prediction was Class 3, Obituaries, followed by Class 4, Sports,
#and Class 5, World. 



table(nb.test.predict1,master.matrix.df[251:500,5943])
#Regarding the columns/categories:
# Arts = 1
# Business = 2
# Obituaries = 3
# Sports = 4
# World = 5


## change alpha and beta, using the laplace smoothing 
nb.test.predict2 <- predict(nb.model,master.matrix.df[251:500,],laplace=3)
confusionMatrix(nb.test.predict2,master.matrix.df[251:500,5943])

nb.test.predict3 <- predict(nb.model,master.matrix.df[251:500,],laplace=.5)
confusionMatrix(nb.test.predict3,master.matrix.df[251:500,5943])

nb.test.predict4 <- predict(nb.model,master.matrix.df[251:500,],laplace=8)
confusionMatrix(nb.test.predict4,master.matrix.df[251:500,5943])

###After changing the laplace smoother to several different variables,
#the result is that there is no noticable change to the prediction
#of the naiveBayes model


###create a dataframe of the naiveBayes tables to extract
###information about which words were the most informative
model.tables <- nb.model$tables
model.tables <- as.data.frame(t(sapply(model.tables, matrix)))
model.tables$word <- rownames(model.tables)

colnames(model.tables)

#separate the table of into columns labeled as 1 or 0
#1 meant the word was something, 0 meant it wasn't
#Regarding the columns/categories:
# Arts = 1
# Business = 2
# Obituaries = 3
# Sports = 4
# World = 5
model.table1 <- model.tables[,c(11,1:5)] 
model.table1$sum <- rowSums(model.table1[,2:6])
colnames(model.table1)[2:6] <- c("Arts","Business","Obituaries","Sports","World")

#looking at the top 10 most informative words for each 
#category doesn't really do much for us since there are many more
#than 10 words than give us the same classification likelyhood

top10.table1.Art <- head(model.table1[order(-model.table1[,2]),],10)
top10.table1.Art <- top10.table1.Art[,c(1,2)]

top10.table1.Business <- head(model.table1[order(-model.table1[,3]),],10)
top10.table1.Business <- top10.table1.Business[,c(1,3)]

top10.table1.Obituary <- head(model.table1[order(-model.table1[,4]),],10)
top10.table1.Obituary <- top10.table1.Obituary[,c(1,4)]

top10.table1.Sport <- head(model.table1[order(-model.table1[,5]),],10)
top10.table1.Sport <- top10.table1.Sport[,c(1,5)]

top10.table1.World <- head(model.table1[order(-model.table1[,6]),],10)
top10.table1.World <- top10.table1.World[,c(1,6)]



#columns/categories 6:10 are the same order as 1:5
model.table0 <- model.tables[,c(11,6:10)] 
model.table0$sum <- rowSums(model.table0[,2:6])
colnames(model.table0)[2:6] <- c("Arts","Business","Obituaries","Sports","World")


#the top 10 hardest to classify articles
  #I'm taking the top 10 articles with the least amount
  # of words that occur in the document matrix

master.matrix.for.later <- data.frame(master.matrix.for.later)
master.matrix.for.later$article <- rownames(master.matrix.for.later)
master.matrix.for.later$word.sum <- rowSums(master.matrix.for.later[,1:5942])

top10.hardest.articles <- master.matrix.for.later[,c(5943,5944)]
top10.hardest.articles <- head(top10.hardest.articles[order(top10.hardest.articles[,2]),],10)



#Now that we have a matrix of words accompanied by their classifiers,
#we can use these words/classes to predict the class of articles from 
#other sources.  If certain words w/in certain categories are used within
#specific time periods, but not others, we may see the prediction level drop

#This is because we will be using prior hyperparameters that may not 
#be too biased, which will result in a drastic change to the posterior
#hyperparameters 




## the code for the full 10k observations is here: ##
master.doc.matrix <- create_matrix(AllResults$text,language = "english",removeNumbers=TRUE, removeStopwords = TRUE, stemWords=TRUE)
master.matrix <- as.matrix(master.doc.matrix)
master.matrix <- apply(as.matrix(master.matrix),2,as.character)
master.matrix[master.matrix>1] <- 1
master.matrix.df <- as.data.frame(master.matrix)

master.classifer <- as.numeric(as.factor(AllResults$nytd_section_facet))
master.classifer <- as.data.frame(master.classifer)
master.classifer$master.classifer <- as.factor(master.classifer$master.classifer)

master.matrix.df$class.label <- master.classifer$master.classifer

nb.model <- naiveBayes(class.label~., data=master.matrix.df,subset=1:250)
nb.test.predict <- predict(nb.model,master.matrix.df[251:500,])

confusionMatrix(nb.test.predict,master.matrix.df[251:500,5943])

## end of naiveBayes on 10k observations ##





