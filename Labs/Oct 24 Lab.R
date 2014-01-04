First we see how a standard for loop is done
result <- rep(NA,10)
for(i in 1:10)
{
    result[i] <- i*2
}
result

load(url("http://www.jaredlander.com/data/credit.rdata"))
head(credit)
require(rpart)
creditTree <- rpart(Credit ~ CreditAmount + Age + CreditHistory + Employment, data=credit)
creditTree
require(rpart.plot)
install.packages("rpart.plot")
rpart.plot(creditTree, extra=4)
require(randomForest)
creditFormula <- Credit ~ CreditHistory + Purpose + Employment + Duration + Age + CreditAmount
creditFormula

CreditForest <- randomForest(creditFormula, data=credit) #does not work

require(useful)
install.packages("devtools")
require(devtools)
install_github("useful","jaredlander")

creditX <- build.x(creditFormula, data=credit)
head(creditX)

creditY <- build.y(creditFormula, data=credit)
head(creditY)

creditForest <- randomForest(x=creditX, y=creditY)
creditForest

predict(creditForest, newdata=creditNew)

#when you have a too many columns, you can reduce the dimensionality  by using elastic net

require(glmnet)

acs <- read.table("http://www.jaredlander.com/data/acs_ny.csv", header=T, sep=",", stringsAsFactors=FALSE)

acs$Income <- with(acs,FamilyIncome >= 150000)

topright(acs)
colnames(acs)
acsFormula <- Income ~ NumBedrooms + NumChildren + NumPeople + NumRooms + NumUnits + NumVehicles + NumWorkers + OwnRent + YearBuilt + ElectricBill + FoodStamp + HeatingFuel + Insurance + Language - 1
#minus 1 means to not include an intercept
acsFormula


acsX <- build.x(acsFormula, data=acs,contrasts=FALSE)
head(build.x(Credit~CreditHistory, data=credit))
unique(credit$CreditHistory)
head(build.x(Credit~CreditHistory, data=credit,contrasts=FALSE))
acsY <- build.y(acsFormula,data=acs)
acs1 <- cv.glmnet(x=acsX, y=acsY, family="binomial",nfold=5)
acs1$lambda.min
plot(acs1)
plot(acs)
coef(acs1,s="lambda.min")
coef(acs1, s="lambda.1se")

