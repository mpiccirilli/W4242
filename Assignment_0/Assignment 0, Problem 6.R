import6 <- "/users/michaelpiccirilli/desktop/Fall_2013/W4242/Assignment_0/train.csv"
table6 <- read.table(file=import6,header=TRUE,sep=",")
head(table6)
summary(table6)

require(ggplot2)
attach(table6)


aggregate(Survived~Sex,table6,sum)
aggregate(Survived~Pclass,table6,sum)
aggregate(Survived~Pclass+Sex,table6,sum)
aggregate(Survived~Pclass+Sex+Embarked,table6,sum)
aggregate(cbind(Fare,Age)~Pclass+Sex+Survived+Embarked,table6,mean)
aggregate(Fare~Pclass+Embarked,table6,mean)
aggregate(Survived~Parch+SibSp,table6,sum)
summary(table6)

