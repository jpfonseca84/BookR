#a
library("boot")
?nuclear
head(nuclear)

cost.null<-lm(cost~1,data=nuclear)
cost.step<-step(cost.null,
     scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt)
summary(cost.step) #this is the stepwise model.

#b
#The forward selection is the biggest of all, altough being the one that 
#most explains the variance. The decision on what to use might be dependend 
#solelly on externam knowledge of the variables by the data scientist.

#c
Galileu<-data.frame(Iniheight=c(1000,800,600,450,300,200,100),
                    Dist=c(573,534,495,451,395,337,257))
gal.null<-lm(Dist~1,data=Galileu)
gal.1<-lm(Dist~Iniheight,data=Galileu)
gal.2<-lm(Dist~Iniheight+I(Iniheight^2),data=Galileu)
gal.3<-lm(Dist~Iniheight+I(Iniheight^2)+I(Iniheight^3),data=Galileu)
gal.4<-lm(Dist~Iniheight+I(Iniheight^2)+I(Iniheight^3)+I(Iniheight^4),data=Galileu)

#d
anova(gal.null,gal.1,gal.2,gal.3,gal.4)
#the adition of a Iniheight ^3 is a good adition to the model, as it is 
#still relevant and adding ^4 does not bring any improvment to the model.

#e
library("faraway")
?diabetes
c.diabetes <- diabetes[-which(
      is.na(diabetes$chol) |
            is.na(diabetes$age) |
            is.na(diabetes$gender) |
            is.na(diabetes$height) |
            is.na(diabetes$weight) |
            is.na(diabetes$frame) |
            is.na(diabetes$waist) |
            is.na(diabetes$hip) |
            is.na(diabetes$location)
), ]

#f
names(c.diabetes)
dia.full<-lm(chol~age*gender*weight*frame+waist*height*hip+location,
             data=c.diabetes)
dia.null<-lm(chol~1,data=c.diabetes)

#g
dia.step<-step(dia.null,
               chol.~.+age*gender*weight*frame+waist*height*hip+
                     location)
summary(dia.step)

#h
add1(dia.null,scope=.~.+age*gender*weight*frame+waist*height*hip+location,
     test="F")
dia.add.1<-lm(chol~frame,data=c.diabetes)
add1(dia.add.1,scope=.~.+age*gender*weight*frame+waist*height*hip+location,
     test="F")
dia.add.2<-lm(chol~frame+waist,data=c.diabetes)
add1(dia.add.2,scope=.~.+age*gender*weight*frame+waist*height*hip+location,
     test="F")
summary(dia.add.2)
#this is not the same model, THe forward is a lot simpler and also, less
# capable of explaining the variance as the R2 is smaller then the step

#i
dia.step.full<-step(dia.full,
                    scope = . ~ . + age * gender * weight * frame + 
                          waist * height * hip + location,
                    test = "F")
#the model is different and far more complex with several interactive 
#effects being mapped. This is probably based to, since the begining, we 
#accessed smallers AIC from the beginning.

#j
library("MASS")
car.null<-lm(mpg~1,data=mtcars)
car.step<-step(car.null,
               scope=.~.+wt*hp*factor(cyl)*disp+am+factor(gear)+drat+vs+
                                                                qsec+carb)
car.null2<-lm(1/mpg~1,data=mtcars)
car.step2<-step(car.null2,
                scope=.~.+wt*hp*factor(cyl)*disp+am+factor(gear)+drat+vs+
                      qsec+carb)

summary(car.step)
summary(car.step2)
#the new model, with GPM is simpler, with slightly less R2 than the 
#previous one, with MPG.