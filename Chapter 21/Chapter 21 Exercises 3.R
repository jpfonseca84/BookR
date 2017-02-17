#First Part ----
library("MASS")
?cats

#a ----
cat.original.fit<-lm(Hwt~Bwt+Sex,data=cats)
summary(cat.original.fit)
cat.fit<-lm(Hwt~Bwt*Sex,data=cats)
summary(cat.fit)

#The significance of the Bwt had decreased, ut the remaining variables 
#significance had increased. ALso, Bwt Main interaction had decreased compared
# with the new model with interactive interactions. Also, being a male had a 
#slight positive interative effect on Bwt.

#b ----
plot(cats$Hwt~cats$Bwt,
     col=c("red","blue")[cats$Sex],
     xlab="Body Weight",
     ylab="Heart Weight",
     main="Cat's Body Weight by Heart Weight")
legend("topleft",
       legend=c("Female","male"),
       fill=c("red","blue"))

#Define the coeficients
cat.male<-c(coef(cat.fit)[1]+coef(cat.fit)[3],coef(cat.fit)[2]*coef(cat.fit)[4])
cat.fema<-c(coef(cat.fit)[1],coef(cat.fit)[2])
abline(cat.male,col="blue")
abline(cat.fema,col="red")

#Now, the interactive interactions are impacting each other, being a Female 
#or being a Male have impact on the regressions HeartWeight. Previously, 
#using only the Main effects, no great difference between sex was identified.

#c ----
predict(cat.fit, #interactive predict
        newdata = data.frame(Bwt=3.4,Sex="F"),
        interval="prediction")
predict(cat.original.fit,#main predict
                      newdata = data.frame(Bwt=3.4,Sex="F"),
                      interval="prediction")

#The new model, with interactive effects can show the impact of being a 
#female has in the overall weight, bringing a lighter weight than predicted 
#previously

#Second Part ----

#d ----
library(faraway)
?trees

tree.mainfit<-lm(Volume~Girth+Height,data=trees)
summary(tree.mainfit)
tree.intfit<-lm(Volume~Girth*Height,data=trees)
summary(tree.intfit)

#The new model has a greater R-Squared value altough have a greater pvalue for
#the variables. Still, statistically acceptable 

#e ----
tree.log.mainfit<-lm(log(Volume)~log(Girth)+log(Height),data=trees)
summary(tree.log.mainfit)
tree.log.intfit<-lm(log(Volume)~log(Girth)*log(Height),data=trees)
summary(tree.log.intfit)
#we got an even greater Rsquared value, but now, all coefficients have huge
# significane levels, not acceptable. We cannot assume they have a log
# relation

#Third Part ----
#f ----
?mtcars
cars.fit<-lm(mpg~hp*factor(cyl)+wt,data=mtcars)
summary(cars.fit)

#g ----
#The hp variable is a continuous and the factor(cyl) is a categorical. Each of 
#'their interactions can be identified as a change in the slope of the 
#'model.

#h ----
#mom wants mpg>=25 
mycars<-data.frame(cyl=c(4,8,6),
                   hp=c(100,210,200),
                   wt=c(2.1,3.9,2.0),
                   row.names = c("Car1","Car2","Car3"))

predict(cars.fit,newdata = mycars,interval="confidence")

#i
#My predictive model suggest that the only car to achieve the desired MPG is
#car1

#ii 
#The regression includes for the car3 a upper limit that includes 25Mpg. 
#There is 95% of confidence the average for that car is between those values