require(plyr)
install.packages("rgl")
require(rgl)

setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_3")
kaggle <- read.table(file="kaggle_data.csv",header=T,sep=",")

#rename the dataset to test on it
df <- kaggle


###try http
test <- grep("http",df$url)

testinfo <- gsub("[[:alpha:]]{4}[[:punct:]]{3}[[:alpha:]]{3}[[:punct:]]{1}([0-9,A-Z,a-z]+)[[:punct:]]{1}.+",replacement="\\1",df$url[test])
testinfo <- as.data.frame(testinfo)
testinfo <- gsub("[[:alpha:]]{4}[[:punct:]]{3}([0-9,A-Z,a-z]+)[[:punct:]]{1}.+",replacement="\\1",testinfo$testinfo[test])
testinfo <- as.data.frame(testinfo)

#put the http link info back into the kaggle dataset
#the use the aggregate function
df$website <- testinfo$testinfo

#Find the frequency of each website:
test_table <- table(testinfo$testinfo)
test_table <- as.data.frame(test_table)


sp_err <- aggregate(spelling_errors_ratio~website,df,mean)
label_agg <- aggregate(label~website,df,mean)
is_news <- aggregate(is_news~website,df,mean)
words_urls <- aggregate(numwords_in_url~website,df,mean)
alchemy_sc <- aggregate(alchemy_category_score~website,df,mean)
alchemy_cat <- aggregate(alchemy_category~website,df,mean)
alchemy_cat$alchemy_category <- round(alchemy_cat$alchemy_category)
#0 category represents a ? 
colnames(df)

test_table$label_mean <- label_agg$label
test_table$spelling_errors_ratio_mean <- sp_err$spelling_errors_ratio
test_table$alchemy_score_mean <- alchemy_sc$alchemy_category_score
test_table$alchemy_category <- alchemy_cat$alchemy_category
test_table$is_news <- is_news$is_news


test_table <- test_table[order(-test_table$Freq),]

colnames(test_table) <- c("Website","Freq","label_mean","alchemy_category","is_news","spelling_errors_ratio_mean","alchemy_score_mean") 
test_table <- test_table[order(-test_table$label_mean,-test_table$Freq),]
rm(test_table)

head10 <- head(test_table,10)

ggplot(test_table, aes())

### rgl test
plot3d(test_table$Freq,test_table$label_mean,test_table$alchemy_category, col=1:3)

###
ggplot(dftrain, aes(x=factor(df$alchemy_category),fill=alchemy_category)) + geom_bar() + coord_polar() + theme(axis.text.x = element_text(colour = 'black', size = 13),axis.title.x=element_blank()) + labs(title = "Count of Each Alchemy Category")

setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Kaggle_Competition")
train <- read.table("train.tsv",header=T,sep="\t", stringsAsFactors = FALSE)

dftrain <- df
dftrain$alchemy_category <- train$alchemy_category
dftrain$alchemy_category <- gsub("\\?+","0",dftrain$alchemy_category)
dftrain$alchemy_category <- gsub("0","unknown",dftrain$alchemy_category)
dftrain$alc