#Assignment 0, Problem 3
import3 <- "/Users/michaelpiccirilli/Desktop/Fall_2013/W4242/Assignment_0/prostate_data.csv"
prostate <- read.table(file=import3, header=TRUE, sep=",")
drops <- c("svi","gleason")
newtable <- prostate[,!(names(prostate) %in% drops)]

library(ggplot2)

#first plotting them individually. This runs extremely slow, but this is the only way
#I could successfully plot each pair.  I was able to use a combn() function to run a regression and plot all the pairs, 
#but then for some reason I couldn't get the regression line in each plot. 


plot1 <- ggplot(newtable,aes(lcavol,lweight))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot1
plot2 <- ggplot(newtable,aes(lcavol,age))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot2
plot3 <- ggplot(newtable,aes(lcavol,lbph))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot3
plot4 <- ggplot(newtable,aes(lcavol,lcp))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot4
plot5 <- ggplot(newtable,aes(lcavol,pgg45))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot5
plot6 <- ggplot(newtable,aes(lcavol,lpsa))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot6
plot7 <- ggplot(newtable,aes(lweight,age))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot7
plot8 <- ggplot(newtable,aes(lweight,lbph))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot8
plot9 <- ggplot(newtable,aes(lweight,lcp))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot9
plot10 <- ggplot(newtable,aes(lweight,pgg45))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot10
plot11 <- ggplot(newtable,aes(lweight,lpsa))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot11
plot12 <- ggplot(newtable,aes(age,lbph))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot12
plot13 <- ggplot(newtable,aes(age,lcp))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot13
plot14 <- ggplot(newtable,aes(age,pgg45))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot14
plot15 <- ggplot(newtable,aes(age,lpsa))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot15
plot16 <- ggplot(newtable,aes(lbph,lcp))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot16
plot17 <- ggplot(newtable,aes(lbph,pgg45))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot17
plot18 <- ggplot(newtable,aes(lbph,lpsa))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot18
plot19 <- ggplot(newtable,aes(lcp,pgg45))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot19
plot20 <- ggplot(newtable,aes(lcp,lpsa))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot20
plot21 <- ggplot(newtable,aes(pgg45,lpsa))+geom_point(colour="black")+geom_smooth(method="lm",colour="red")
plot21


#This will plot them all on one page
install.packages("gridExtra")
library(gridExtra)
grid.arrange(plot1,plot2,plot3, plot4, plot5, plot6, plot7, plot8, plot9, plot10,
              plot11, plot12, plot13, plot14, plot15, plot16, plot17, plot18, plot19,
              plot20, plot21
            )