#a
library("boot")
nuclear
#fitting the model
nuc.0<-lm(cost~1,data=nuclear)
add1(nuc.0,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
nuc.1<-update(nuc.0,formula=.~.+date)
add1(nuc.1,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
nuc.2<-update(nuc.1,formula=.~.+cap)
add1(nuc.2,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
nuc.3<-update(nuc.2,formula=.~.+pt)
add1(nuc.3,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
nuc.4<-update(nuc.3,formula=.~.+ne)
add1(nuc.4,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
#summary of the model
summary(nuc.4)

#b
plot(nuc.4,which=1)
plot(nuc.4,which=2)
#The assumprions to the error component are good in this case, we have a
#good normality in the residuals vs fitter, and also have a good QQ chart 
#progression, as the information is very close to theoretical quantiles.

#c
plot(nuc.4,which=4)
abline(h=4/nrow(nuclear),lty=2)
#there is only one observation that presents as being too influential, 
#that is the 19.

#d
plot(nuc.4,which=5,cook.levels = c(4/nrow(nuclear),0.5,1))
#the individually influential points are characterized as outside of the 
#red lines on the edges of the 0.

#e
newnuc<-nuclear[-19,]
nnuc.0<-lm(cost~1,data=newnuc)
add1(nnuc.0,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
nnuc.1<-update(nnuc.0,formula=.~.+date)
add1(nnuc.1,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
nnuc.2<-update(nnuc.1,formula=.~.+cap)
add1(nnuc.2,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
nnuc.3<-update(nnuc.2,formula=.~.+ne)
add1(nnuc.3,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
nnuc.4<-update(nnuc.3,formula=.~.+ct)
add1(nnuc.4,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
nnuc.5<-update(nnuc.4,formula=.~.+pt)
add1(nnuc.5,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")
#Removing the oulier, the model is completely changed, with a new predictor 
#in it.

#f
library("faraway")
dia.fit<-lm(chol~age*frame+waist,data=diabetes)
summary(dia.fit)

#g
plot(dia.fit,which=1)
#the residuals appear to be well centred in the 0.
plot(dia.fit,which=2)
#the middle part of the chart appears to be well dentered as well, but 
#the more extreme the values get, the more residuals we have. the model is 
#not explaining some relation between the predictors and response.

#h
n<-nrow(diabetes)-15
plot(dia.fit,which=4, cook.levels = 4/n)
abline(h=4/n,lty=2)
plot(dia.fit,which=5, cook.levels = c(4/n,4*2/n,4*3/n))
plot(dia.fit,which=6, cook.levels = 4/n)


#i ----
dia.url<-"http://www.amstat.org/publications/jse/v9n2/4cdata.txt"
diamonds<-read.table(dia.url)
names(diamonds)<-c("Carat","Color","Clarity","Cert","Price")

plot(diamonds$Carat, #PLot by Clarity
     diamonds$Price,
     col=as.numeric(diamonds$Clarity),
     pch=19)
legend("topleft",
       pch=19,
       col=1:length(as.numeric(levels(diamonds$Clarity))),
       legend=levels(diamonds$Clarity))
plot(diamonds$Carat, #Plot by Color
     diamonds$Price,
     col=as.numeric(diamonds$Color),
     pch=16)
legend("topleft",
       col=1:length(as.numeric(levels(diamonds$Color))),
       pch=16,
       legend=levels(diamonds$Color))
plot(Price~Carat,
     data=diamonds,
     col=as.numeric(diamonds$Cert),
     pch=16)
legend("topleft",
       legend=levels(diamonds$Cert),
       col=1:length(as.numeric(levels(diamonds$Cert))),
       pch=16)

#j
dia.lm<-lm(Price~Carat+Color+Clarity+Cert,data=diamonds)
summary(dia.lm)
plot(dia.lm,which=1)
plot(dia.lm,which=2)
plot(dia.lm,which=3)
# - the first analysis, Residuals X Fitted presents an unexpected tendency.
#indicating the errorslinearity is not truth.
# - The normal QQ plot indicates that on extreme values, the preditction
#tends to be wrong. Specially in the upper tail, we cannot assume we have a 
#normal distribution.
# - The same happens to the last chart, the resituals are floating around a

#k
dia.log.lm<-lm(log(Price)~Carat+Color+Clarity+Cert,data=diamonds)
summary(dia.log.lm)
plot(dia.log.lm,which=1)
plot(dia.log.lm,which=2)
plot(dia.log.lm,which=3)
#Log transformation has done nothing to make the relation better.
#But the extreme values got their severe non-normality reduced agains the 
#rest of the data.

#l
dia.log.lm2<-lm(log(Price)~Carat+I(Carat^2)+Color+Clarity+Cert,data=diamonds)
plot(dia.log.lm2,which=1)
plot(dia.log.lm2,which=2)
plot(dia.log.lm2,which=3)
#The carat^2 improved the linearity of the errors, also brought the erros
#distribution to a more normal form and with less risk of heteroscedacity
