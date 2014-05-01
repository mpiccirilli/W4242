fakeData <-
  c("Statistics is a small part of data science but data sicence is not a part statistics",
    "Data science uses statistics and data mining",
    "antother word for data mining is machine learning or statistical learning")

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

firstKV <- keyval(key=c(1,2,3),value=c("a","b","c"))

values <- function (kv)
{
  kv$value
}

keys <- function(kv)
{
  kv$key
}

values(firstKV)
keys(firstKV)

"data"
d 1 
a 2 
t 1

unlist(strsplit(x=fakeData,split=""))
theDF <- data.frame(A=1:20)
theDF
theDF$Counter <- 1
theDF
sum(theDF$Counter)



theMapper <- function(key, value)
{
  theSplit <- unlist(strsplit(x=value,split=""))
  values <- rep(x=1, times=NROW(theSplit))
  keyval(key=theSplit, value=values)
}

firstMap <- theMapper(value=fakeData)
firstMap

theReducer <- function(key,value)
{
  keyval(key=key,value=sum(value))  
}

firstMap

##
theDF <- data.frame(key=keys(firstMap), value=values(firstMap))
theDF
aggregate(value~key, data=theDF, sum)  #this is essentially map reduce

###something coool
simpleFunc <- function(data, func)
{
  func(data)
}
simpleFunc(1:10,mean)
simpleFunc(1:10,sum)
###


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

trial <- mapreduce(data=fakeData, map=theMapper, reduce=theReducer)

sapply(trial, values)



#if rmr works
rmr2::mapreduce(data=fakeData, map=theMapper, reduce=theReducer)
