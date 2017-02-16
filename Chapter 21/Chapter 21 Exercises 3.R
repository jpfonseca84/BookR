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
# with the new model with interactive interactions.

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

#Now, the interactive interactions are impacting each other, being a Female or 
#being a Male have impact on the regressions HeartWeight. Previously, using only 
#the Main effects, no great difference between sex was identified.

#c ----
interactive.predict<-predict(cat.fit,
        newdata = data.frame(Bwt=3.34,Sex="F"),
        interval="prediction")
main.predict<-predict(cat.original.fit,
                      newdata = data.frame(Bwt=3.34,Sex="F"),
                      interval="prediction")

#The new model, with interactive effects can show the impact of being a female
#has in the overall weight.

#Second Part 
#d


#Second Part
#d ----
library(faraway)
?trees

tree.mainfit<-lm(Volume~Girth+Height,data=trees)
summary(tree.fit)
tree.intfit<-lm(Volume~Girth*Height,data=trees)
summary(tree.intfit)

#The new model has a greater R-Squared value altough have a greater pvalue for
#the variables. Still, statistically acceptable 

#e ----
tree.log.intfit<-lm(log(Volume)~log(Girth)*log(Height),data=trees)
summary(tree.log.intfit)
#we got an even greater Rsquared value, but now, all coefficients have huge
# significane levels, not acceptable. We cannot assume they have a log
# relation



#Third Part
#f ----
?mtcars
cars.fit<-lm(mpg~hp*cyl,data=mtcars)
summary(cars.fit)

#g ----
#the summary for the car.fit presents the model as statistical relevant with 
#it's R-Squared value and also, presents good p-values for the variables 
#themselves. The interaction between hp and Cyl is still relevant, but it has 
#low impact in the regression.

#h
