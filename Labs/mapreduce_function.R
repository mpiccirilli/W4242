

##### Question 1, Part A ##### 
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

