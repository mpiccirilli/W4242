http://api.nytimes.com/svc/search/v2/articlesearch.json?api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311&fq=news_desk:("Sports")&fl=web_url,news_desk,lead_paragraph,word_count,headline&begin_date=20130601&end_date=20131112
require(rjson)
require(plyr)

theQuery <- "http://api.nytimes.com/svc/search/v2/articlesearch.json?api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311&fq=news_desk:(\"Sports\")&fl=web_url,news_desk,lead_paragraph,word_count,headline&begin_date=20130601&end_date=20131112"
theQuery
theResults <- fromJSON(file=theQuery)
View(theResults)
str(theResults)
View(theResults$response)
theResults$response$meta
str(theResults$response$docs)
theResults$response$docs[[1]]


TestQueryV1 <- "http://api.nytimes.com/svc/search/v1/article?query=nytd_section_facet:[Business]&fields=url,title,body,date,desk_facet&offset=1&begin_date=20071231&end_date=20131112&rank=newest&api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311"
TestQueryV1
QResultsV1 <- fromJSON(file=TestQueryV1)
View(QResultsV1)
View(QResultsV1$results)
QueryDF <- ldply(QResultsV1$results, as.data.frame)


BusQuery <- "http://api.nytimes.com/svc/search/v1/article?query=nytd_section_facet:[Business]&fields=url,title,body,date,desk_facet&offset=%s&begin_date=20071231&end_date=20131117&rank=newest&api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311"
BusResults <- vector("list",3)

for(i in 0:2)
{
  tempURL <- sprintf(BusQuery, i)
  tempResult <- fromJSON(file=tempURL)
  BusResults[[i+1]] <- ldply(tempResult$results, as.data.frame)
}

length(BusResults)
BusResultsDF <- ldply(BusResults)

####Sports query
SportsQuery <- "http://api.nytimes.com/svc/search/v1/article?query=nytd_section_facet:[Sports]&fields=url,title,body,date,desk_facet&offset=%s&rank=newest&api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311"
SportsResults <- vector("list",200)

for(i in 0:199)
{
  StempURL <- sprintf(SportsQuery, i)
  StempResult <- fromJSON(file=StempURL)
  SportsResults[[i+1]] <- ldply(StempResult$results, as.data.frame)
}

length(SportsResults)
SportsResultsDF <- ldply(SportsResults)




resultDF <- ldply(theResults$response$docs, as.data.frame)

theQuery <- "http://api.nytimes.com/svc/search/v2/articlesearch.json?api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311&fq=news_desk:(\"Sports\")&fl=web_url,news_desk,lead_paragraph,word_count,headline&begin_date=20130601&end_date=20131112&page=1"

resultsSports <- vector("list",3)
resultsSports

baseQuery <- "http://api.nytimes.com/svc/search/v2/articlesearch.json?api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311&fq=news_desk:(\"Sports\")&fl=web_url,news_desk,lead_paragraph,word_count,headline&begin_date=20130601&end_date=20131112&page=%s"
#sprintf
for(i in 0:2)
{
  tempURL <- sprintf(baseQuery, i)
  tempResult <- fromJSON(file=tempURL)
  resultsSports[[i+1]] <- ldply(tempResult$response$docs, as.data.frame)
}

str(resultsSports)
length(resultsSports)
resultsSportsAll <- ldply(resultsSports)
class(resultsSportsAll)
resultsSportsAll


#turn this into a function
theString <- "Hello %s, what are you for %s?"
sprintf(theString, "Bob","dinner")

get.api <- function(section, lastPage=0)
{
  results <- veector("list",lastPage+1)
  baseQuery <- "http://api.nytimes.com/svc/search/v2/articlesearch.json?api-key=45fcda6fe28f92056a19a87c939b477a:5:68404311&fq=news_desk:(\"%s\")&fl=web_url,news_desk,lead_paragraph,word_count,headline&begin_date=20130601&end_date=20131112&page=%s"
  for i in 0:lastPage)
{
  tempURL<- sprintf(baseQuery, section, i)
  tempResult <- fromJSON(file=tempURL)
  results[[i+1]] <- ldply(tempResults$response$docs, as.data.frame)
}
  return(ldply(results))
}


require(RTextTools)
naiveBayes

doc_matrix <- create_matrix(resultsSportsAll$lead_paragraph,language = "english",removeNumbers=TRUE, removeStopwords = TRUE, stemWords=TRUE)
doc_matrix
require(useful)
topright(as.matrix(doc_matrix))

textX <- as.matrix(doc_matrix)
nb1 <- naiveBayes(x=textX, y=resultsSportsAll$news_desk)

head(nb1$tables)

class(nb1)
require(e1071)
e1071:::predict.naiveBayes





data(HouseVotes84, package = "mlbench")
model <- naiveBayes(Class ~ ., data = HouseVotes84)
predict(model, HouseVotes84[1:10,])


pred <- predict(model, HouseVotes84)
table(pred, HouseVotes84$Class)


data(iris)
m <- naiveBayes(Species ~ ., data = iris)
## alternatively:
m <- naiveBayes(iris[,-5], iris[,5])
m
table(predict(m, iris), iris[,5])

data(Titanic)
m <- naiveBayes(Survived ~ ., data = Titanic)
m
predict(m, as.data.frame(Titanic))

set.seed(12345)
test <- data.frame(pred=c(runif(50,0,75),runif(50,25,100)), group=c(rep("A",50), rep("B",50)) )
table(test$pred<50,test$group)
