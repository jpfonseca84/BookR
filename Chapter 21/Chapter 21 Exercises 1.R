library("MASS")
head(cats)
?cats
#a ----

plot(cats$Hwt ~ cats$Bwt,
     typ = "n",
     xlab = "Body Weight",
     ylab = "Heart Wheight")
points(cats$Hwt[cats$Sex == "M"] ~ cats$Bwt[cats$Sex == "M"],
       col = "Blue")
points(cats$Hwt[cats$Sex == "F"] ~ cats$Bwt[cats$Sex == "F"],
       col = "red", pch = 4)
legend(
      "topleft",
      legend = c("Male", "Female"),
      col = c("Blue", "red"),
      pch = c(1, 4)
)

#b ----
fitcats <- lm(Hwt ~ Bwt + Sex, data = cats)
summary(lm(cats$Hwt ~ cats$Bwt))

#i
#the Equation is: y = -0.4149 + 4.0758*Bwt - 0.0821*SexM
#there is a good statistic relevance to the BWT with Pvalue being small, but
#there is not statistic relevance for the sex, as the pvalue is too small.
#this states that only one of them are relevant for the fit model.

#ii
#The coefficient of determination is 0.6468. It affirms that 65% of the variation
# of the Hwt can be explained by the model.even if it includes a not statistical
#relevant predictor variable in the model. 

#c ----

predict(fitcats,
        newdata = data.frame(Bwt = (3.4), Sex = ("F")),
        interval = "prediction")

#d ----
catcoef <- coef(fitcats)
abline(catcoef[1], catcoef[2], col = "grey", lwd = 2)
abline(catcoef[1] + catcoef[3], catcoef[2], col = "pink", lwd = 2)
#The lines follows the coeficient of the Summary, Sex is not changing the
#prediction relevance.
#Second Part ----

library("boot")
?nuclear

#e ----
pairs(nuclear)

#f ----
nucfit <- lm(cost ~ t1 + t2, data = nuclear)
summary(nucfit)
#coef are:    -242.146+ 29.908*t1+ 4.689*t2
# sign are:   0.37373    0.00262    0.12224

#g ----

nucfit2 <- lm(cost ~ t1 + t2 + date, data = nuclear)
summary(nucfit2)
#now the coefficient have changed and the most relevant by then, is the least
#relevant now. It appears the year the construction was issue is more relevant
# then the T1 and T2 variables.
#coef are:         -9232.833 -5.918*t1 +4639*t2 +138.324*date
#significance:     0.00434    0.68176   0.08535   0.00519

#h ----

nucfit3 <- lm(cost ~ date + cap + ne, data = nuclear)
summary(nucfit3)
#Equation is: -6458.3889006 +95.4385587*date + 0.4157157*cap + 126.1287688*ne
confint(nucfit3)
#All intervals exclude the 0 value. reflecting the variables significance.

#Third Part ----
security<-data.frame(Murders=c(8.6,8.9,8.52,8.89,13.07,14.57,21.36,28.03,31.49,37.39,46.26,47.24,52.33),
                     Police=c(260.35,269.8,272.04,272.96,272.51,261.26,268.89,295.99,319.87,341.43,356.59,376.69,390.19),
                     Unemployment=c(11,7,5.2,4.3,3.5,3.2,4.1,3.9,3.6,7.1,8.4,7.7,6.3),
                     Guns=c(178.15,156.41,198.02,222.10,301.92,391.22,665.56,1131.21,837.6,794.9,817.74,583.17,709.59))
security
#i ----
pairs(security)
# the Police variable has the most strongly related to the murder rate

#j ----
secfit<-lm(Murders~Police+Unemployment+Guns,data=security)
summary(secfit)
#It is not reasonable to state that all relationshipps between response and 
#predictors are casual. We can see that Guns also presents a strong statistical 
# evidence of relationship with murders.
#k ----
secfit2<-lm(Murders~Police+Guns,data=security)
summary(secfit2)
#Removing the unemployment predictor variable there is a much better statistical
#relation between the predictors and the answer. But hte model keeps having a 
#great prediction capability.

#l ----
predict.lm(
      secfit2,
      newdata = data.frame(Police = 300, Guns = c(500, 0)),
      interval = "confidence",
      level = 0.99
)
