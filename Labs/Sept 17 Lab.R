# code for problem #2
theXmatrix <- matrix(c(-1, -2, 1, 4, 0, -6, -8, -7, 9), nrow=3,ncol=3)
theYmatrix <- matrix(1:9,3,3)
theBvector <- matrix(1:3,3)
theXmatrix %*% theYmatrix
theYmatrix%*%theXmatrix
theXmatrix%*%theBvector
t(theXmatrix)
solve(theXmatrix)
#still need to get the rank of the theXmatrix

#starting number 1
assignno1matrix <- "/Users/michaelpiccirilli/Desktop/Fall '13/W4242/Basics of R/sort_data.txt"
importmatrix1 <- read.table(file=assignno1matrix, header=FALSE)
importmatrix1
importmatrix1[1,]
sort(importmatrix1[1:10,1])
sort(importmatrix1[1,1:10])
importmatrix1
sortmatrix1 <- c(importmatrix1[1,],importmatrix1[2,])
importmatrix1[1:10,1]
a<- importmatrix1[1:10,1]
b<- importmatrix1[1:10,2]
thenewDF <- data.frame(a,b)
thenewDF
importmatrix1 <- mattrix
newmatrix <- data.matrix(importmatrix1)
newmatrix
#still need to figure out how to sort the tabe/matrix

#starting number 3
import3 <- "/Users/michaelpiccirilli/Desktop/prostate_data.csv"
prostate <- read.table(file=import3,header=TRUE,sep=",")
prostate
head(prostate)
require(graphics)

## Annette Dobson (1990) "An Introduction to Generalized Linear Models".
## Page 9: Plant Weight Data.
ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
weight <- c(ctl, trt)
lm.D9 <- lm(weight ~ group)
lm.D90 <- lm(weight ~ group - 1) # omitting intercept

anova(lm.D9)
summary(lm.D90)

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(lm.D9, las = 1)      # Residuals, Fitted, ...
par(opar)
weight
order(newmatrix[1:10,1]
head(prostate)
plot1 <- lm(lcavol~lweight, data=prostate)
plot(plot1)
plot2 <- lm(lcavol~age, data=prostate)
plot(plot2)
plot1
plot2
summary(plot1)
summary(prostate)
plot(sort(prostate$age))
plot(density(prostate$age))
head(prostate)
      plot(lcavol~lweight, data=prostate)
first.lm <- lm(lcavol~lweight, data=prostate)
summary(first.lm)
plot(first.lm)
plot(lcavol~lweight, data=prostate)
      summary(first.lm)
      plot(first.lm)
      second.lm <- lm(lweight~lcavol, data=prostate)
      plot(second.lm)
      plot(lweight~lcavol,data=prostate)
      
head(prostate)
model <- lm(lcavol~lweight, data=prostate)
model
plot(lcavol~lweight, data=prostate)
lines(prostate$lweight,model$fit)


     
      