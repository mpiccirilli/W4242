#Display name:  Michael Piccirilli

#I tried to create a randomForest based simply on the numerical data that was 
#given in the original training file.

#Below is the code I used to create my entry


setwd("/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Kaggle_Competition")
ktrain <- read.table("train.tsv",header=T,sep="\t")
ktest <- read.table("test.tsv",header=T,sep="\t")
require(plyr)

#rename the imported data to prevent from making errors
dftrain <- ktrain
dftest <- ktest

#take out the question marks in columns
dftrain$alchemy_category <- gsub("\\?+","NA",dftrain$alchemy_category)
dftrain$alchemy_category_score <- gsub("\\?+","0",dftrain$alchemy_category_score)
dftest$alchemy_category <- gsub("\\?+","NA",dftest$alchemy_category)
dftest$alchemy_category_score <- gsub("\\?+","0",dftest$alchemy_category_score)

#reduce the dataframe
reduced1 <- dftrain[,c(2,5:12,27)]

#runing a randomforest
trainr <- randomForest(label ~., data=reduced1, importance=TRUE, do.trace=100)
print(trainr)

k.predict = predict(trainr, dftest)
k.predict
dfpredict <- as.data.frame(k.predict)
dfpredict$k.predict[dfpredict$k.predict < .5] <- 0
dfpredict$k.predict[dfpredict$k.predict >= .5] <- 1

predict1 <- dftest$urlid
predict1 <- as.data.frame(predict1)
predict1$label <- dfpredict$k.predict
colnames(predict1) <- c("urlid","label")

write.table(predict1,file="kaggle_submit_1.csv",sep=",",row.names=F)

