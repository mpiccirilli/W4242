require(ggplot2)
data(diamonds)
head(diamonds)

ggplot(diamonds, aes(x=price)) + geom_histogram()
ggplot(diamonds, aes(x=price)) + geom_density()
ggplot(diamonds, aes(x=price)) + geom_density(color="blue", fill="blue")
ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
g <- ggplot(diamonds, aes(x=carat, y=price))
g + geom_point(aes(color=cut))
g + geom_point(aes(color=cut)) + xlim(0,3)
g + geom_point (aes(color=cut)) + facet_wrap(~clarity, ncol = 2)
g + geom_point (aes(color=cut)) + facet_grid(color~clarity) #color grid going down, clarity across
ggplot(diamonds, aes(x=price, color=cut, fill=cut)) + geom_histogram()
head(economics)
ggplot(economics, aes(x=date,y=pce)) + geom_line()
require(lubridate)
economics$Year <- year(economics$date)
economics$Month <- month(economics$date)
head(economics)
econ2000 <- economics[economics$Year >= 2000,]
ggplot(econ2000, aes(x=Month, y=pce, color=as.factor(Year))) + geom_line() #looking at as continuous data, want discrete

#####regressions & modeling
install.packages("UsingR")
require(UsingR)
head(father.son)
ggplot(father.son, aes(x=fheight, y = sheight)) + geom_point()
ggplot(father.son, aes(x=fheight, y = sheight)) + geom_point() + geom_smooth(method="lm")
heightMod <- lm(sheight~fheight,data=father.son)
heightMod

33.8866+.514*fheight
data("tips", package="reshape2")
head(tips)
ggplot(tips, aes(x=total_bill, y=tip, color=day,shape=sex)) + geom_point()
mod1 <- lm(tip ~ total_bill + sex + day, data=tips)
mod1
head(model.matrix(tip ~ total_bill + sex + day, data=tips))
summary(mod1)
install.packages("coefplot")
require(coefplot)
coefplot(mod1,lwdInner=2, lwdOuter=1) #narrow conf int 
mod2 <- lm(tip ~ total_bill + sex + day + time + smoker, data =tips)
multiplot(mod1,mod2)


require(boot) #cross validation
tip1 <- glm(tip ~ total_bill + sex + day, data =tips, family=gaussian(link="identity"))
tipCV1 <- cv.glm(data=tips,glmfit=tip1,K=5)
tipCV1$delta #gives MSE, lower is better
tip2 <- glm(tip ~ total_bill + sex + day + time + smoker, data = tips, family=gaussian(link="identity"))
tipCV2 <- cv.glm(data=tips, glmfit=tip2, K=5)
tipCV2$delta

