library("MASS")
head(survey)

#a
table(survey$Exer)
plot(Height~Exer,data=survey)

#b
survfit4<-lm(Height~Exer,data=survey)
#The default reference level of the predictor is the FREQ level.
summary(survfit4)
#This provide the estimates of Betas for the categorical variable's levels.

#c
#It appears that frequency impacts somehow the height. the less Exercices 
#someone does the shorter it will be. Also, the overal significance of the 
#predictor is supported by the global F-stat and it's P-value.

#d
one.of.each<-factor(levels(survey$Exer))
predict(survfit4,
        data.frame(Exer = one.of.each),
        interval = "prediction",
        level = 0.95)

#e
summary(aov(Height~Exer,data=survey))
#Using an ANOVA table I get the same F-stat and the same p-value. as expected

#f ------------- REVIEW
rel.Exer<-relevel(one.of.each,ref="None")
summary(aov(Height~Exer,data=survey))
#No diference in the output as the global test does not depend on the baseline 
#value of the response. 

#g
names(mtcars)
table(mtcars$gear)

boxplot(qsec~gear,data=mtcars,ylab="seconds for quarter mile",
        xlab="Number of gears")
points(qsec~gear,data=mtcars)

summary(lm(qsec~gear,data=mtcars))
# the model summary states that we don't have enought statistical evidence to 
#confirm an interference in the qsec variable caused by the number of gears 
# in a linear model. It also states that we expect a decrease in the qsec per 
#aditionar gear number.

#h
fac.gear<-factor(mtcars$gear)
car.fit2<-lm(mtcars$qsec~fac.gear)
summary(car.fit2)
#Now, I see that there is a relevance. Having 4 gears increase the speed, while 
#having 5 decrease the speed.
plot(mtcars$qsec ~ mtcars$gear,
     ylab = "seconds for quarter mile",
     xlab = "Number of gears")
abline(car.fit)

#i
#Considering the categorical values as continual values, we have a negative 
#relation overall. But this is not accurate as the points do not follow the LM.
#with a multilevel analisys, we can get closer to the accurate values.