#First Part ----
#h0: All means are equal // Ha: One mean is not equal

info<-data.frame("dep"=c(93,120,65,105,115,82,99,87,100,90,78,95,93,88,110,85,
                         45,80,28,75,70,65,55,50,40,100,75,65,40,73,65,50,30,
                         45,50,45,55,96,58,95,90,65,80,85,95,82),
                 "site"=c(rep("Site i",15),
                          rep("Site ii",10),
                          rep("Site iii",12),
                          rep("Site iv",9)))
#a 
infomean<-tapply(info$dep,info$site,mean)

boxplot(info$dep~info$site,
        xlab="Site",
        ylab="Depth (cm)",
        main="Boxplot of Depth by cm by Site")
points(1:4,infomean,pch=4)

#b
infomeancen<-info$dep-infomean[as.numeric(info$site)]

qqnorm(infomeancen)+
      qqline(infomeancen)
#The chart provides enought information to accept the normality of the data

info.sds<-tapply(info$dep,info$site,sd)
max(info.sds)/min(info.sds)
#to check the equality of variances we used the rule of thumb. as it is <2. We 
#accept it

#c
infoaov<-aov(dep~site,info)
summary(infoaov)
#the p value is very small we have strong evidence to reject H0

dev.off()

#Second Part ----
#Digagnosis checking what suits an ANOVA analysis
#Sepal.Length ----
sl.mean<-tapply(iris$Sepal.Length,iris$Species,mean) #mean
sl.meancen<-iris$Sepal.Length-sl.mean[as.numeric(iris$Species)] #mean centered
sl.sd<-tapply(iris$Sepal.Length,iris$Species,sd)

#checking variance
boxplot(iris$Sepal.Length~iris$Species)
      points(1:3,sl.mean,pch=4)

max(sl.sd)/min(sl.sd)
#the variance in Boxplot looks acceptable and the rule of thumb <2 confirms this

#checking normality
qqnorm(sl.meancen)+
      qqline(sl.meancen)
#the data gathered looks normal as well. we can use this information for an ANOVA.
#Sepal.width ----
sw.mean<-tapply(iris$Sepal.Width,iris$Species,mean) #mean
sw.meancen<-iris$Sepal.Width-sw.mean[as.numeric(iris$Species)] #mean centered
sw.sd<-tapply(iris$Sepal.Width,iris$Species,sd)

#checking variance
boxplot(iris$Sepal.Width~iris$Species)+
      points(1:3,sl.mean,pch=4)

max(sw.sd)/min(sw.sd)
#the variance in Boxplot looks acceptable and the rule of thumb <2 confirms this

#checking normality
qqnorm(sw.meancen)
qqline(sw.meancen)
#the data gathered looks normal as well. we can use this information for an 
#ANOVA. This distributions looks mor normal then the previous
#Petal.Length ----
pl.mean<-tapply(iris$Petal.Length,iris$Species,mean) #mean
pl.meancen<-iris$Petal.Length-pl.mean[as.numeric(iris$Species)] #mean centered
pl.sd<-tapply(iris$Petal.Length,iris$Species,sd)

#checking variance
boxplot(iris$Petal.Length~iris$Species)+
      points(1:3,sl.mean,pch=4)

max(pl.sd)/min(pl.sd)
#the variance in Boxplot does not look acceptable, and the rule of thumb is not 
#<2. this is not a good data to proceed with ANOVA.

#checking normality
qqnorm(pl.meancen)
qqline(pl.meancen)
#the data gathered looks normal as well. we can use this information for an 
#ANOVA. This distributions looks mor normal then the previous
#Petal.width----
pw.mean<-tapply(iris$Petal.Width,iris$Species,mean) #mean
pw.meancen<-iris$Petal.Width-pw.mean[as.numeric(iris$Species)] #mean centered
pw.sd<-tapply(iris$Petal.Width,iris$Species,sd)

#checking variance
boxplot(iris$Petal.Width~iris$Species)+
      points(1:3,sl.mean,pch=4)

max(pw.sd)/min(pw.sd)
#the variance in Boxplot does not look acceptable
#and the rule of thumb >2 confirms this

#checking normality
qqnorm(pw.meancen)
qqline(pw.meancen)
#the data gathered looks normal as well. we can use this information for an 
#ANOVA. This distributions looks mor normal then the previous
#the best way to use a ANOVA is with the Sepal analysis

#b
#ANOVA for Sepal.Lenght
sl.aov<-aov(Sepal.Length~Species,iris)
summary(sl.aov)
#The pvalue is much less than the F value. We reject H0. 

sw.aov<-aov(Sepal.Width~Species,iris)
summary(sl.aov)
#The pvalue is much less than the F value. We reject H0. 