# load required packages
require(plyr)
require(rjson)
require(RTextTools)
require(useful)
require(e1071)
require(SparseM)
require(tm)
require(stringr)
require(XML)
require(randomForest)
require(caret)
require(RWeka)

############## Question 1, Part A ##############

mapreduce.train.title <- as.character(train.kaggle$title)

keyval <- function(key, value=NULL)
{
  if(missing(value))
  {
    list(key=NULL,value=key)
  }else
  {
    list(key=key, value=value)
  }
}

values <- function (kv)
{
  kv$value
}

keys <- function(kv)
{
  kv$key
}


theMapper <- function(key, value)
{
  theSplit <- unlist(strsplit(x=value,split=" "))
  values <- rep(x=1, times=NROW(theSplit))
  keyval(key=theSplit, value=values)
}


theReducer <- function(key,value)
{
  keyval(key=key,value=sum(value))  
}

mapreduce <- function(data, map, reduce)
{
  mappedResults <- map(., data)
  organizedResults <- tapply(X=values(mappedResults),
                             INDEX=keys(mappedResults),
                             FUN=function(x) x)
  theReduced <- mapply(reduce,
                       key=names(organizedResults),value=organizedResults,
                       SIMPLIFY=FALSE, USE.NAMES=FALSE)
  keyval(key=sapply(theReduced, keys),value=sapply(theReduced, values))
}

mapreduce.word.count <- mapreduce(data=mapreduce.train.title, map=theMapper, reduce=theReducer)

mapreduce.result <- as.data.frame(t(laply(mapreduce.word.count, matrix)))
colnames(mapreduce.result)[1:2] <- c("word","count")





############## Question 1, Part B ##############

### Set the current working directory and load in the Kaggle Data ###
setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Kaggle_Competition")
train.kaggle <- read.table("train.tsv",header=T,sep="\t", stringsAsFactors = FALSE)
test.kaggle <- read.table("test.tsv",header=T,sep="\t",stringsAsFactors=FALSE)

dummy.train <- read.table("train.tsv",header=T,sep="\t", stringsAsFactors = FALSE)
dummy.test <- read.table("test.tsv",header=T,sep="\t",stringsAsFactors=FALSE)

### create twenty features of the Kaggle data ###
##must create and impliment features on

#1,2,3) strip out the title, body, and URL from the boilerplate
jsonDataTrain <- sapply(train.kaggle$boilerplate, fromJSON)
train.kaggle$title <- sapply(1:nrow(train.kaggle), function(i, jsonDataTrain) unlist(jsonDataTrain[[i]])["title"], jsonDataTrain)
train.kaggle$body <- sapply(1:nrow(train.kaggle), function(i, jsonDataTrain) unlist(jsonDataTrain[[i]])["body"], jsonDataTrain)
train.kaggle$bp_url <- sapply(1:nrow(train.kaggle), function(i, jsonDataTrain) unlist(jsonDataTrain[[i]])["url"], jsonDataTrain)

jsonDataTest <- sapply(test.kaggle$boilerplate, fromJSON)
test.kaggle$title <- sapply(1:nrow(test.kaggle), function(i, jsonDataTest) unlist(jsonDataTest[[i]])["title"], jsonDataTest)
test.kaggle$body <- sapply(1:nrow(test.kaggle), function(i, jsonDataTest) unlist(jsonDataTest[[i]])["body"], jsonDataTest)
test.kaggle$bp_url <- sapply(1:nrow(test.kaggle), function(i, jsonDataTest) unlist(jsonDataTest[[i]])["url"], jsonDataTest)

train.kaggle$all.text <- do.call(paste, c(AllResults[c("body", "title")], sep = ""))

#4) number of words in the title of each website
title.words <- as.data.frame(str_count(train.kaggle$title,"\\w+"))
colnames(title.words)[1] <- "title.word.count"
train.kaggle$title.word.count <- title.words$title.word.count

title.test.words <- as.data.frame(str_count(test.kaggle$title,"\\w+"))
colnames(title.test.words)[1] <- "title.word.count"
test.kaggle$title.word.count <- title.test.words$title.word.count


#5) number of words in body from boilerplate
body.words <- as.data.frame(str_count(train.kaggle$body,"\\w+"))
colnames(body.words)[1] <- "body.word.count"
train.kaggle$body.word.count <- body.words$body.word.count

body.test.words <- as.data.frame(str_count(test.kaggle$body,"\\w+"))
colnames(body.test.words)[1] <- "body.word.count"
test.kaggle$body.word.count <- body.test.words$body.word.count

#6) number of characters in the url (nchar) 
url.char.train <- as.data.frame(nchar(train.kaggle$bp_url))
colnames(url.char.train)[1] <- "url.char.count"
train.kaggle$url.char.count <- url.char.train$url.char.count

url.char.test <- as.data.frame(nchar(test.kaggle$bp_url))
colnames(url.char.test)[1] <- "url.char.count"
test.kaggle$url.char.count <- url.char.test$url.char.count

rm(body.test.words, body.words,title.test.words,title.words,url.char.test,url.char.train, jsonDataTest, jsonDataTrain)

#7) Total # of words on page based on:
#### A) avglinksize = (total # of hypertext words)/(total # of <a>'s)
#### B) linkwordscore = (total # of hypertext words)/(total # of words on page) -- is given in a whole number, must divide by 100 to get %
#### C) numberOfLinks = (total # of <a>'s)

#result will be (A/B)*C

train.kaggle$linkwordscore <- train.kaggle$linkwordscore/100 #B is given as a whole number
total.words.train <- as.data.frame(train.kaggle$avglinksize/train.kaggle$linkwordscore)
colnames(total.words.train)[1] <- "total.words.on.page"

total.words.train$total.words.on.page <- gsub("inf","na",total.words.train$total.words.on.page,ignore.case=TRUE)
total.words.train$total.words.on.page <- gsub("nan","na",total.words.train$total.words.on.page,ignore.case=TRUE)

any(is.na(total.words.train$total.words.on.page))
class(total.words.train$total.words.on.page)
total.words.train$total.words.on.page <- as.numeric(total.words.train$total.words.on.page)
any(is.na(total.words.train$total.words.on.page))
total.words.train$total.words.on.page[is.na(total.words.train$total.words.on.page)] <- 0

total.words.train$total.words.on.page <- round(total.words.train$total.words.on.page*train.kaggle$numberOfLinks)
train.kaggle$total.words.on.page <- total.words.train$total.words.on.page



test.kaggle$linkwordscore <- test.kaggle$linkwordscore/100 #B is given as a whole number
total.words.test <- as.data.frame(test.kaggle$avglinksize/test.kaggle$linkwordscore)
colnames(total.words.test)[1] <- "total.words.on.page"

total.words.test$total.words.on.page <- gsub("inf","na",total.words.test$total.words.on.page,ignore.case=TRUE)
total.words.test$total.words.on.page <- gsub("nan","na",total.words.test$total.words.on.page,ignore.case=TRUE)

any(is.na(total.words.test$total.words.on.page))
class(total.words.test$total.words.on.page)
total.words.test$total.words.on.page <- as.numeric(total.words.test$total.words.on.page)
any(is.na(total.words.test$total.words.on.page))
total.words.test$total.words.on.page[is.na(total.words.test$total.words.on.page)] <- 0

total.words.test$total.words.on.page <- round(total.words.test$total.words.on.page*test.kaggle$numberOfLinks)
test.kaggle$total.words.on.page <- total.words.test$total.words.on.page


rm(total.words.test, total.words.train)
#8) Total number of words in hypertext
#### A) avglinksize = (total # of hypertext words)/(total # of <a>'s)
#### C) numberOfLinks = (total # of <a>'s)

#result will be (A*C)

train.kaggle$total.hypertext.words <- train.kaggle$avglinksize*train.kaggle$numberOfLinks
test.kaggle$total.hypertext.words <- test.kaggle$avglinksize*test.kaggle$numberOfLinks


#9) Total number of non-markup(hypertext) words on page

#### result will be:
   # dataset$total.words.on.page - dataset$total.hypertext.words

train.kaggle$total.nonhyper.words <- train.kaggle$total.words.on.page - train.kaggle$total.hypertext.words
test.kaggle$total.nonhyper.words <- test.kaggle$total.words.on.page - test.kaggle$total.hypertext.words

#10) index the alchemy categories 
train.kaggle$alch.cat.index <- as.numeric(as.factor(train.kaggle$alchemy_category))
test.kaggle$alch.cat.index <- as.numeric(as.factor(test.kaggle$alchemy_category))


#11) take the log of commonlinkratio_1
train.kaggle$commonlinkratio_1 <- log(train.kaggle$commonlinkratio_1)
test.kaggle$commonlinkratio_1 <- log(test.kaggle$commonlinkratio_1)

train.kaggle$commonlinkratio_1 <- gsub("-Inf","0",train.kaggle$commonlinkratio_1)
train.kaggle$commonlinkratio_1 <- as.numeric(train.kaggle$commonlinkratio_1)

test.kaggle$commonlinkratio_1 <- gsub("-Inf","0",test.kaggle$commonlinkratio_1)
test.kaggle$commonlinkratio_1 <- as.numeric(test.kaggle$commonlinkratio_1)


#12) take the log of commonlinkratio_2
train.kaggle$commonlinkratio_2 <- log(train.kaggle$commonlinkratio_2)
test.kaggle$commonlinkratio_2 <- log(test.kaggle$commonlinkratio_2)

train.kaggle$commonlinkratio_2 <- gsub("-Inf","0",train.kaggle$commonlinkratio_2)
train.kaggle$commonlinkratio_2 <- as.numeric(train.kaggle$commonlinkratio_2)

test.kaggle$commonlinkratio_2 <- gsub("-Inf","0",test.kaggle$commonlinkratio_2)
test.kaggle$commonlinkratio_2 <- as.numeric(test.kaggle$commonlinkratio_2)


#13) take the log of commonlinkratio_3
train.kaggle$commonlinkratio_3 <- log(train.kaggle$commonlinkratio_3)
test.kaggle$commonlinkratio_3 <- log(test.kaggle$commonlinkratio_3)

train.kaggle$commonlinkratio_3 <- gsub("-Inf","0",train.kaggle$commonlinkratio_3)
train.kaggle$commonlinkratio_3 <- as.numeric(train.kaggle$commonlinkratio_3)

test.kaggle$commonlinkratio_3 <- gsub("-Inf","0",test.kaggle$commonlinkratio_3)
test.kaggle$commonlinkratio_3 <- as.numeric(test.kaggle$commonlinkratio_3)



#14) take the lag of commonlinkratio_4
train.kaggle$commonlinkratio_4 <- log(train.kaggle$commonlinkratio_4)
test.kaggle$commonlinkratio_4 <- log(test.kaggle$commonlinkratio_4)

train.kaggle$commonlinkratio_4 <- gsub("-Inf","0",train.kaggle$commonlinkratio_4)
train.kaggle$commonlinkratio_4 <- as.numeric(train.kaggle$commonlinkratio_4)

test.kaggle$commonlinkratio_4 <- gsub("-Inf","0",test.kaggle$commonlinkratio_4)
test.kaggle$commonlinkratio_4 <- as.numeric(test.kaggle$commonlinkratio_4)



#15) root website name
http.train <- grep("http",train.kaggle$url)
webinfo.train <- gsub("[[:alpha:]]{4}[[:punct:]]{3}[[:alpha:]]{3}[[:punct:]]{1}([0-9,A-Z,a-z]+)[[:punct:]]{1}.+",replacement="\\1",train.kaggle$url[http.train])
webinfo.train <- as.data.frame(webinfo.train)
webinfo.train <- gsub("[[:alpha:]]{4}[[:punct:]]{3}([0-9,A-Z,a-z]+)[[:punct:]]{1}.+",replacement="\\1",webinfo.train$webinfo[http.train])
webinfo.train <- as.data.frame(webinfo.train)
train.kaggle$website <- webinfo.train$webinfo.train


http.test <- grep("http",test.kaggle$url)
webinfo.test <- gsub("[[:alpha:]]{4}[[:punct:]]{3}[[:alpha:]]{3}[[:punct:]]{1}([0-9,A-Z,a-z]+)[[:punct:]]{1}.+",replacement="\\1",test.kaggle$url[http.test])
webinfo.test <- as.data.frame(webinfo.test)
webinfo.test <- gsub("[[:alpha:]]{4}[[:punct:]]{3}([0-9,A-Z,a-z]+)[[:punct:]]{1}.+",replacement="\\1",webinfo.test$webinfo.test[http.test])
webinfo.test <- as.data.frame(webinfo.test)
test.kaggle$website <- webinfo.test$webinfo.test
test.kaggle$website <- as.character(test.kaggle$website)

ncol(test.kaggle)

#16) use HTML parser to take the information each website, using the
#url provided in the provided $url
#something along these lines:

#require(XML)
#theurl <- "http://en.wikipedia.org/wiki/Brazil_national_football_team"
#url.tables <- readHTMLTable(theurl)
#n.rows <- unlist(lapply(url.tables, function(t) dim(t)[1]))

#or looking at what other websites have been linked within the page provided


#17) log of compression_ratio
train.kaggle$compression_ratio <- log(train.kaggle$compression_ratio)
test.kaggle$compression_ratio <- log(test.kaggle$compression_ratio)

train.kaggle$compression_ratio <- gsub("-Inf","0",train.kaggle$compression_ratio)
train.kaggle$compression_ratio <- as.numeric(train.kaggle$compression_ratio)

test.kaggle$compression_ratio <- gsub("-Inf","0",test.kaggle$compression_ratio)
test.kaggle$compression_ratio <- as.numeric(test.kaggle$compression_ratio)




#18) log of frameTagRatio
train.kaggle$frameTagRatio <- log(train.kaggle$frameTagRatio)
test.kaggle$frameTagRatio <- log(test.kaggle$frameTagRatio)

train.kaggle$frameTagRatio <- gsub("-Inf","0",train.kaggle$frameTagRatio)
train.kaggle$frameTagRatio <- as.numeric(train.kaggle$frameTagRatio)

test.kaggle$frameTagRatio <- gsub("-Inf","0",test.kaggle$frameTagRatio)
test.kaggle$frameTagRatio <- as.numeric(test.kaggle$frameTagRatio)



#19) positive/negative sentiment analysis on body 

#possibly using the "qdap" package, utilizing their dataset of words
#with associated average happiness score along with the polarity() function


#20) word matrix on $title column
train.title.kaggle.matrix <- create_matrix(train.kaggle$title, language = "english",
                                   removeNumbers=TRUE, removeStopwords = TRUE,
                                   stemWords=TRUE, removePunctuation=TRUE,
                                           toLower=TRUE)
train.title.kaggle.matrix <- as.matrix(train.title.kaggle.matrix)


test.title.kaggle.matrix <- create_matrix(test.kaggle$title, language = "english",
                                           removeNumbers=TRUE, removeStopwords = TRUE,
                                           stemWords=TRUE, removePunctuation=TRUE,
                                          toLower=TRUE)
test.title.kaggle.matrix <- as.matrix(test.title.kaggle.matrix)



#21) 3-or-2-gramLength word matrix on the $body column

train.body.kaggle.matrix <- create_matrix(train.kaggle$body, language = "english",
                                           removeNumbers=TRUE, removeStopwords = TRUE,
                                          removePunctuation=TRUE,
                                          toLower=TRUE)


sample.size <- floor(. * nrow(train.kaggle))
sample.index <- sample(nrow(train.kaggle),size=sample.size)
sample <- train.kaggle[sample.index, ]


train.body.kaggle.matrix <- apply(as.matrix(train.body.kaggle.matrix),2,as.character)
train.body.kaggle.matrix[train.body.kaggle.matrix>1] <- 1
train.body.kaggle.matrix <- as.data.frame(train.body.kaggle.matrix)


findFreqTerms(train.body.kaggle.matrix, lowfreq=100)


inspect(train.sp.body.kaggle.matrix)

test.body.kaggle.matrix <- create_matrix(test.kaggle$body, language = "english",
                                          removeNumbers=TRUE, removeStopwords = TRUE,
                                         removePunctuation=TRUE,
                                         toLower=TRUE, ngramLength=3)

findFreqTerms(test.body.kaggle.matrix, lowfreq=100)


### I will use the following examples to utilize the tokenizer
#as I cannot simply convert the 2 or 3-gram DTM into a matrix
#library("RWeka")
#library("tm")

#TrigramTokenizer <- function(x) NGramTokenizer(x, 
                                               Weka_control(min = 3, max = 3))
#tdm <- TermDocumentMatrix(Corpus(VectorSource(Text)), control = list(tokenize = TrigramTokenizer))


#22) Take the number of synonyms in the body of text provided


############## Question 1, Part C ##############
#create a model based on some of the features created in part B

#### Fist submission ####
#naiveBayes on the words in the title 

train.title.kaggle.matrix <- create_matrix(train.kaggle$title, language = "english",
                                           removeNumbers=TRUE, removeStopwords = TRUE,
                                           stemWords=TRUE, removePunctuation=TRUE,
                                           toLower=TRUE)
train.title.kaggle.matrix <- as.matrix(train.title.kaggle.matrix)
train.title.kaggle.matrix <- apply(as.matrix(train.title.kaggle.matrix),2,as.character)
train.title.kaggle.matrix[train.title.kaggle.matrix>1] <- 1
train.title.kaggle.matrix.df <- as.data.frame(train.title.kaggle.matrix)
train.kaggle.classifer <- data.frame(as.factor(train.kaggle$label))
colnames(train.kaggle.classifer)[1] <- "label.classifer"
train.title.kaggle.matrix.df$label.classifer <- train.kaggle.classifer$label.classifer


test.title.kaggle.matrix <- create_matrix(test.kaggle$title, language = "english",
                                          removeNumbers=TRUE, removeStopwords = TRUE,
                                          stemWords=TRUE, removePunctuation=TRUE,
                                          toLower=TRUE)
test.title.kaggle.matrix <- as.matrix(test.title.kaggle.matrix)
test.title.kaggle.matrix <- apply(as.matrix(test.title.kaggle.matrix),2,as.character)
test.title.kaggle.matrix[test.title.kaggle.matrix>1] <- 1


train.nb.model <- naiveBayes(label.classifer ~., data=train.title.kaggle.matrix.df)

test.kaggle.nb.predict <- predict(train.nb.model,test.title.kaggle.matrix)
head(test.kaggle.nb.predict)

test.kaggle.label <- as.matrix(test.kaggle.nb.predict)
test.kaggle.label <- data.frame(test.kaggle.label)
any(is.na(test.kaggle.label))

kaggle.submit.1 <- data.frame(test.kaggle$urlid)
kaggle.submit.1$label <- test.kaggle.label$test.kaggle.label
colnames(kaggle.submit.1)[1] <- "urlid"

#make first submission, which is based on running a naiveBayes
#on just the words in the titles of the websites

setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_4/")
write.table(kaggle.submit.1,file="A4_Q1c_submit1.csv",sep=",",row.names=F)
#result score:  0.74132



#### Second submission  ####
   
#I will use randomForest on some numeric features I created from Part B
#Then I will take the avg of the randomForest prediction and the 
#naiveBayes prediction


#I used the following features I created above for the randomForest


#1,2,3) strip out the title, body, and URL from the boilerplate
#4) number of words in the title of each website
#5) number of words in body from boilerplate
#6) number of characters in the url (nchar) 
#7) Total # of words on page based on:
#8) Total # of words in hypertext
#9) Total # of nonhypertext words
#10) index the alchemy categories 
#11) take the log of commonlinkratio_1
#12) take the log of commonlinkratio_2
#13) take the log of commonlinkratio_3
#14) take the log of commonlinkratio_4
#17) log of compression_ratio
#18) log of frameTagRatio

#to be included in the randomForest set: 
    #avglinksize
    #linkwordscore
    #numberOfLinks



train.kaggle$alchemy_category_score <- gsub("\\?+",".4",train.kaggle$alchemy_category_score)
train.kaggle$alchemy_category_score <- as.numeric(train.kaggle$alchemy_category_score)

train.kaggle$news_front_page <- gsub("\\?+",".5",train.kaggle$news_front_page)
train.kaggle$news_front_page <- as.numeric(train.kaggle$news_front_page)

train.kaggle$is_news <- gsub("\\?+","0",train.kaggle$is_news)
train.kaggle$is_news <- as.numeric(train.kaggle$is_news)


test.kaggle$alchemy_category_score <- gsub("\\?+",".4",test.kaggle$alchemy_category_score)
test.kaggle$alchemy_category_score <- as.numeric(test.kaggle$alchemy_category_score)

test.kaggle$news_front_page <- gsub("\\?+",".5",test.kaggle$news_front_page)
test.kaggle$news_front_page <- as.numeric(test.kaggle$news_front_page)

test.kaggle$is_news <- gsub("\\?+","0",test.kaggle$is_news)
test.kaggle$is_news <- as.numeric(test.kaggle$is_news)


sapply(test.kaggle,class)



head(train.kaggle$alchemy_category_score)

colnames(train.kaggle)
colnames(test.kaggle)
#take out all the "-Inf" from the log columns
rf.kaggle.train.subset <- train.kaggle[,c(6,8,9,11,14,17,20,22,23,26,27,31,32,34:37)]
sapply(rf.kaggle.train.subset,class)


colnames(rf.kaggle.train.subset)

#runing a randomforest 
train.rf.model <- randomForest(label ~., data=rf.kaggle.train.subset, mtry=4, ntree=2500, do.trace=100, importance=TRUE, nodesize=1)
print(train.rf.model)
importance(train.rf.model,type=1)

rf.kaggle.test.subset <- test.kaggle

test.rf.predict <- round(predict(train.rf.model, rf.kaggle.test.subset, type="response"))

test.rf.predict <- data.frame(test.rf.predict)
colnames(test.rf.predict)[1] <- "label"


setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_4")
nB.submit <- read.table("A4_Q1c_submit1.csv",header=T,sep=",", stringsAsFactors = FALSE)


new.label <- data.frame((test.rf.predict$label + nB.submit$label)/2)
colnames(new.label)[1] <- "label"
new.label[new.label>=.5] <- 1

rf.nb.submit <- data.frame(test.kaggle$urlid)
colnames(rf.nb.submit)[1] <- "urlid"
rf.nb.submit$label <- new.label$label

setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_4/")
write.table(rf.nb.submit,file="rf-nb-combo.csv",sep=",",row.names=F)

