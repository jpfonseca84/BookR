#First part
quakes

#a ----
depth.f<-cut(quakes$depth,breaks=c(0,200,400,680),include.lowest = F)

#b ----
#Check normality and variance for a Anova or Kruskal-Wallis Test

boxplot(quakes$depth~depth.f) # the bloxplot seems ok for the Variance.
depth.sd<-tapply(quakes$depth,depth.f,sd)
max(depth.sd)/min(depth.sd) # the rule of thumb is OK for equal variance.

depth.mean<-tapply(quakes$depth,depth.f,mean)
depth.meancen<-quakes$depth-depth.mean[as.numeric(depth.f)]
hist(depth.meancen)
qqnorm(depth.meancen)
qqline(depth.meancen)
#Assume the normality of the distribution per the quantil-quantile comparsion
#We can proceed with the ANOVA test.

#c ----
#Assume alpha= 0.01
# the hypotheses are h0 all mean are equal //HA: not all means are equal 
summary(aov(quakes$depth~depth.f))
#As the p is lesser then alpha, we can assume not all means are equal.

#d
library("MASS")
Cars93
cars.mean<-aggregate(Cars93$Length,
          by=list(Airbags=Cars93$AirBags,
                  ManualTransmission=Cars93$Man.trans.avail),
          FUN=mean)

#e
interaction.plot(cars.mean[,1],
                 cars.mean[,2],
                 response=cars.mean[,3],
                 xlab = "Air Bags",
                 ylab="mean length",
                 trace.label = "Manual\n   Trans")
#it appears to exist an interaction between Airbags and Manual transmission
#considering only these two variables.

#f
#Being the H0 the variables have no effect on each other
# and HA, at least one variable have effect on  the other.

summary(aov(Length~AirBags+Man.trans.avail+AirBags:Man.trans.avail,Cars93))

#There is not enouhgt statistical evidence to confirm the interactive effect.
#but we do have enought statistical evidence to confirm the Main effect of both 
# variables on the length.