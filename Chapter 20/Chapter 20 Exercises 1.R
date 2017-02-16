#A ----
library("MASS")
names(survey)
survfit<-lm(Height~Wr.Hnd,data=survey)
newdata<-data.frame(Wr.Hnd=(c(12,15.2,17,19.9)))
predict(survfit,newdata=newdata,interval="confidence",level = .99 )
#B ----
incomplete.obs<-which(is.na(survey$Height)|is.na(survey$Wr.Hnd))
#calculate Beta.hat0 and Beta.hat1

survey.cobs<-survey[-incomplete.obs,] #Survey complete obs
Betahat.1<-cor(survey.cobs$Wr.Hnd,survey.cobs$Height)*
      sd(survey.cobs$Height)/
      sd(survey.cobs$Wr.Hnd)

Betahat.0<-mean(survey.cobs$Height)-Betahat.1*mean(survey.cobs$Wr.Hnd)
#C ----
names(survey)
#i
survfit2<-lm(Height~Pulse,data=survey)
plot(Height~Pulse,data=survey)
abline(survfit2)

#ii
#The point estimate of the slope states that, the higher the pulse, the shortest
# the subject. 
summary(survfit2)
#we don't have enought statistical evidence to reject H0, that Beta1 is !=0.
confint(survfit2,level=0.90)
# the confidence interval for the slope (Betahat.0) contains the "0". reinforcing 
#the  hypothesis test.

#iii
abline(survfit2)
xvals<-data.frame(Pulse=seq(30,110,length=200)) #values for the plot lines
ci.val<-predict(survfit2,newdata=xvals,interval="confidence",level=0.90)
pi.val<-predict(survfit2,newdata=xvals,level=0.90,interval="prediction")

plot(Height~Pulse,data=survey,typ="n")
points(Height~Pulse,data=survey)
abline(survfit2)
lines(xvals[,1],ci.val[,2],lty=3,col="red")
lines(xvals[,1],ci.val[,3],lty=3,col="red")
lines(xvals[,1],pi.val[,2],lty=3,col="green")
lines(xvals[,1],pi.val[,3],lty=3,col="green")
legend("topright",legend=c("CI","PI"),lty=c(3,3),col=c("blue","green"))

##PLaying of painting a polygon

rxvals<-data.frame(Pulse=sort(xvals[,1],decreasing=TRUE))#to create the polygon
rci.val<-predict(survfit2,newdata=rxvals,interval="confidence",level=0.90)

plot(Height~Pulse,data=survey,type="n")
points(Height~Pulse,data=survey)
polygon(x=c(xvals[,1],rxvals[,1]),
        y=c(ci.val[,2],rci.val[,3]),
        col="blue",
        density=30
)
lines(xvals[,1],ci.val[,2],lty=3,col="red")
lines(xvals[,1],ci.val[,3],lty=3,col="red")
lines(xvals[,1],pi.val[,2],lty=3,col="green")
lines(xvals[,1],pi.val[,3],lty=3,col="green")
legend("topright",legend=c("CI","PI"),lty=c(3,3),col=c("blue","green"))


#iv
incomplete.obs<-which(
      is.na(
            survey$Pulse)|
            is.na(survey$Height)) #define missing points

#calculate mean of data used in chart
height.mean<-mean(survey$Height[-incomplete.obs])
abline(h=height.mean,col="purple",lwd=4)

#The chart supports my conclusion that the Beta1.hat might be 0 as it is inside 
#the  confidence Interval.

#d
mtcars
#i
plot(mpg~wt,data=mtcars)

#e
carfit<-lm(mpg~wt,data=mtcars)
abline(carfit)
#the linear regression model is the one above

#f
#the regression equation is Y.hat= 37.285- 5.344 * x
summary(carfit)
#Both parameters are statistically significant due to their low P.value

#g
testvalue<-data.frame(wt=6)
predict(carfit,testvalue,interval="prediction")
# I do not believe the model on this range as the lwr level of PI includes negative values, 
# what doesn't make sense.