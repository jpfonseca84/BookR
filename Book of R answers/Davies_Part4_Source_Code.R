options(prompt="R> ")

##################
### CHAPTER 17 ####
##################

################
# Section 17.1 ####
################

## 17.1.1 ##

xvals <- seq(16,28,by=0.1)
fx.samp <- dnorm(xvals,22,1.5/sqrt(5))
plot(xvals,fx.samp,type="l",lty=2,lwd=2,xlab="",ylab="")
abline(h=0,col="gray")
fx <- dnorm(xvals,22,1.5)
lines(xvals,fx,lwd=2)
legend("topright",legend=c("raw obs. distbn.","sampling distbn. (mean)"),lty=1:2,lwd=c(2,2),bty="n") # this plot is continued below

#

pnorm(21.5,mean=22,sd=1.5)

#

pnorm(21.5,mean=22,sd=1.5/sqrt(5))

#

abline(v=21.5,col="gray")
xvals.sub <- xvals[xvals<=21.5]
fx.sub <- fx[xvals<=21.5]
fx.samp.sub <- fx.samp[xvals<=21.5]
polygon(cbind(c(21.5,xvals.sub),c(0,fx.sub)),density=10)
polygon(cbind(c(21.5,xvals.sub),c(0,fx.samp.sub)),density=10,angle=120,lty=2)

#

obs <- rnorm(5,mean=22,sd=1.5)  # In the text, my five values are approximately as follows: obs <- c(22.92233,23.09505,20.98653,20.10941,22.33888)
obs

#

obs.mean <- mean(obs)
obs.mean
obs.sd <- sd(obs)
obs.sd

#

obs.mean.se <- obs.sd/sqrt(5)
obs.mean.se

#

t4 <- (21.5-obs.mean)/obs.mean.se
t4

#

pt(t4,df=4)

#

xvals <- seq(-5,5,length=100)
fx.samp.t <- dt(xvals,df=4)
plot(xvals,dnorm(xvals),type="l",lty=2,lwd=2,col="gray",xlim=c(-4,4),xlab="",ylab="")
abline(h=0,col="gray")
lines(xvals,fx.samp.t,lty=3,lwd=2)
polygon(cbind(c(t4,-5,xvals[xvals<=t4]),c(0,0,fx.samp.t[xvals<=t4])),density=10,lty=3)
legend("topright",legend=c("N(0,1) standard","t (4 df)"),col=c("gray","black"),lty=2:3,lwd=c(2,2),bty="n")


## 17.1.2 ####

p.hat <- 80/118
p.hat

#

118*p.hat
118*(1-p.hat)

#

p.se <- sqrt(p.hat*(1-p.hat)/118)
p.se

#

pvals <- seq(p.hat-5*p.se,p.hat+5*p.se,length=100)
p.samp <- dnorm(pvals,mean=p.hat,sd=p.se)
plot(pvals,p.samp,type="l",xlab="",ylab="",xlim=p.hat+c(-4,4)*p.se,ylim=c(0,max(p.samp)))
abline(h=0,col="gray")

#

pvals.sub <- pvals[pvals>=0.7 & pvals<=0.75]
p.samp.sub <- p.samp[pvals>=0.7 & pvals<=0.75]
polygon(cbind(c(0.7,pvals.sub,0.75),c(0,p.samp.sub,0)),border=NA,col="gray")

#

pnorm(0.75,mean=p.hat,sd=p.se) - pnorm(0.7,mean=p.hat,sd=p.se)


## 17.1.3 ##

# no R code



################
# Section 17.2 ####
################

## 17.2.1 ##

temp.sample <- rnorm(n=5,mean=22,sd=1.5) # In the text, my five values are approximately as follows: temp.sample <- c(20.46097,21.45658,21.06410,20.49367,24.92843)
temp.sample

#

temp.mean <- mean(temp.sample)
temp.mean
temp.sd <- sd(temp.sample)
temp.sd
temp.se <- temp.sd/sqrt(5)
temp.se

#

1-0.05/2
critval <- qt(0.975,df=4)
critval

#

pt(critval,4)-pt(-critval,4)

#

temp.mean-critval*temp.se
temp.mean+critval*temp.se

#

temp.mean+c(-1,1)*qt(p=0.9,df=4)*temp.se
temp.mean+c(-1,1)*qt(p=0.995,df=4)*temp.se


## 17.2.2 ##

p.hat <- 80/118
p.hat
p.se <- sqrt(p.hat*(1-p.hat)/118)
p.se

#

qnorm(0.95)

#

p.hat+c(-1,1)*qnorm(0.95)*p.se


## 17.2.3 ##

# no R code


## 17.2.4 ##

# no R code




##################
### CHAPTER 18 ###
##################

################
# Section 18.1 #
################

dbinom(18,20,0.07) + dbinom(19,20,0.07) + dbinom(20,20,0.07)


## 18.1.1 ##

# no R code


## 18.1.2 ##

# no R code


## 18.1.3 ##

# no R code


## 18.1.4 ##

# no R code


## 18.1.5 ##

# no R code



################
# Section 18.2 #
################

## 18.2.1 ##

snacks <- c(87.7,80.01,77.28,78.76,81.52,74.2,80.71,79.5,77.87,81.94,80.7,82.32,
            75.78,80.19,83.91,79.4,77.52,77.62,81.4,74.89,82.95,73.59,77.92,77.18,
            79.83,81.23,79.28,78.44,79.01,80.47,76.23,78.89,77.14,69.94,78.54,79.7,
            82.45,77.29,75.52,77.21,75.99,81.94,80.41,77.7)

#

n <- length(snacks)
snack.mean <- mean(snacks)
snack.mean
snack.sd <- sd(snacks)
snack.sd

#

snack.se <- snack.sd/sqrt(n)
snack.se

#

snack.T <- (snack.mean-80)/snack.se
snack.T

#

pt(snack.T,df=n-1)

#

snack.mean+c(-1,1)*qt(0.975,n-1)*snack.se

#

t.test(x=snacks,mu=80,alternative="less")

#

t.test(x=snacks,mu=80,alternative="two.sided")$conf.int  


## 18.2.2 ##

snacks2 <- c(80.22,79.73,81.1,78.76,82.03,81.66,80.97,81.32,80.12,78.98,79.21,
             81.48,79.86,81.06,77.96,80.73,80.34,80.01,81.82,79.3,79.08,79.47,
             78.98,80.87,82.24,77.22,80.03,79.2,80.95,79.17,81)

#

snack2.mean <- mean(snacks2)
snack2.mean
snack2.sd <- sd(snacks2)
snack2.sd

#

t.test(x=snacks2,y=snacks,alternative="greater",conf.level=0.9)

#

(snack2.mean-snack.mean) + c(-1,1)*qt(0.95,df=60)*sqrt(snack.sd^2/44+snack2.sd^2/31)

#

snack.sd/snack2.sd

#

men <- c(102,87,101,96,107,101,91,85,108,67,85,82)
women <- c(73,81,111,109,143,95,92,120,93,89,119,79,90,126,62,92,77,106,105,111)

#

mean(men)
sd(men)
mean(women)
sd(women)

#

sd(women)/sd(men)

#

t.test(x=men,y=women,alternative="two.sided",conf.level=0.95,var.equal=TRUE)

#

rate.before <- c(52,66,89,87,89,72,66,65,49,62,70,52,75,63,65,61) 
rate.after <- c(51,66,71,73,70,68,60,51,40,57,65,53,64,56,60,59) 

#

rate.d <- rate.after-rate.before
rate.d

#

rate.dbar <- mean(rate.d)
rate.dbar
rate.sd <- sd(rate.d)
rate.sd

#

rate.T <- rate.dbar/(rate.sd/sqrt(16))
rate.T
pt(rate.T,df=15)

#

t.test(x=rate.after,y=rate.before,alternative="less",conf.level=0.95,paired=TRUE)

#

rate.dbar-qt(0.975,df=15)*(rate.sd/sqrt(16))

#

rate.dbar+qt(0.975,df=15)*(rate.sd/sqrt(16))



################
# Section 18.3 #
################

## 18.3.1 ####

sick <- c(0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,0,0,0,1)

#

sum(sick)
p.hat <- mean(sick)
p.hat

#

29*0.2
29*0.8

#

Z <- (p.hat-0.2)/sqrt(0.2*0.8/29)
Z

#

2*(1-pnorm(Z))

#

p.hat+c(-1,1)*qnorm(0.975)*sqrt(p.hat*(1-p.hat)/29)

#

prop.test(x=sum(sick),n=length(sick),p=0.2,correct=FALSE)


## 18.3.2 ####

x1 <- 180
n1 <- 233
p.hat1 <- x1/n1
p.hat1
x2 <- 175
n2 <- 197
p.hat2 <- x2/n2
p.hat2

#

p.star <- (x1+x2)/(n1+n2)
p.star

#

Z <- (p.hat2-p.hat1)/sqrt(p.star*(1-p.star)*(1/n1+1/n2))
Z

#

1-pnorm(Z)

#

prop.test(x=c(x2,x1),n=c(n2,n1),alternative="greater",correct=FALSE)

#

(p.hat2-p.hat1)+c(-1,1)*qnorm(0.975)*sqrt(p.hat1*(1-p.hat1)/n1+p.hat2*(1-p.hat2)/n2)



################
# Section 18.4 #
################

## 18.4.1 ##

hairy <- c(2,3,2,3,2,1,3,3,2,2,3,2,2,2,3,3,3,2,3,2,2,2,1,3,2,2,2,1,2,2,3,2,2,2,2,1,2,1,1,1,2,2,2,3,1,2,1,2,1,2,1,3,3)

#

x <- seq(0,20,length=100)
plot(x,dchisq(x,df=1),type="l",xlim=c(0,15),ylim=c(0,0.5),ylab="density")
lines(x,dchisq(x,df=5),lty=2)
lines(x,dchisq(x,df=10),lty=3)
abline(h=0,col="gray")
abline(v=0,col="gray")
legend("topright",legend=c("df=1","df=5","df=10"),lty=1:3)

#

n <- length(hairy)
n
hairy.tab <- table(hairy)
hairy.tab
hairy.tab/n

#

expected <- 1/3*n
expected
hairy.matrix <- cbind(1:3,hairy.tab,expected,(hairy.tab-expected)^2/expected)
dimnames(hairy.matrix) <- list(c("clean","beard OR mous.","beard AND mous."),c("i","Oi","Ei","(Oi-Ei)^2/Ei"))
hairy.matrix

#

X2 <- sum(hairy.matrix[,4])
X2

#

1-pchisq(X2,df=2)

#

chisq.test(x=hairy.tab)

#

chisq.test(x=hairy.tab,p=c(0.25,0.5,0.25))


## 18.4.2 ##

skin <- matrix(c(20,32,8,52,9,72,8,32,16,64,30,12),4,3,dimnames=list(c("Injection","Tablet","Laser","Herbal"),c("None","Partial","Full")))
skin

#

kr <- nrow(skin)
kc <- ncol(skin)

#

rowSums(skin)
colSums(skin)

#

rep(colSums(skin),each=kr)
rep(colSums(skin),each=kr)*rowSums(skin)
rep(colSums(skin),each=kr)*rowSums(skin)/sum(skin)
skin.expected <- matrix(rep(colSums(skin),each=kr)*rowSums(skin)/sum(skin),nrow=kr,ncol=kc,dimnames=dimnames(skin))
skin.expected


#

skin.array <- array(data=cbind(skin,skin.expected,(skin-skin.expected)^2/skin.expected),dim=c(kr,kc,3),dimnames=list(dimnames(skin)[[1]],dimnames(skin)[[2]],c("O[i,j]","E[i,j]","(O[i,j]-E[i,j])^2/E[i,j]")))
skin.array

#

X2 <- sum(skin.array[,,3])
X2

#

1-pchisq(X2,df=(kr-1)*(kc-1))

#

chisq.test(x=skin)



################
# Section 18.5 #
################

## 18.5.1 ##

# No R code


## 18.5.2 ##

typeI.tester <- function(mu0,sigma,n,alpha,ITERATIONS=10000){
  pvals <- rep(NA,ITERATIONS)
  for(i in 1:ITERATIONS){
    temporary.sample <- rnorm(n=n,mean=mu0,sd=sigma)
    temporary.mean <- mean(temporary.sample)
    temporary.sd <- sd(temporary.sample)
    pvals[i] <- 1-pt((temporary.mean-mu0)/(temporary.sd/sqrt(n)),df=n-1)
  }
  return(mean(pvals<alpha))
}

#

typeI.tester(mu0=0,sigma=1,n=40,alpha=0.05)

#

typeI.tester(mu0=-4,sigma=0.3,n=60,alpha=0.01)


## 18.5.3 ##

critval <- qnorm(1-0.05,mean=0,sd=1/sqrt(30))
critval

#

pnorm(critval,mean=0.5,sd=1/sqrt(30))

#

typeII.tester <- function(mu0,muA,sigma,n,alpha,ITERATIONS=10000){
  pvals <- rep(NA,ITERATIONS)
  for(i in 1:ITERATIONS){
    temporary.sample <- rnorm(n=n,mean=muA,sd=sigma)
    temporary.mean <- mean(temporary.sample)
    temporary.sd <- sd(temporary.sample)
    pvals[i] <- 1-pt((temporary.mean-mu0)/(temporary.sd/sqrt(n)),df=n-1)
  }
  return(mean(pvals>=alpha))
}

#

typeII.tester(mu0=0,muA=0.5,sigma=1,n=30,alpha=0.05)

#

typeII.tester(mu0=0,muA=0.5,sigma=1,n=30,alpha=0.01)

#

typeII.tester(mu0=0,muA=0.5,sigma=1.1,n=30,alpha=0.01)
typeII.tester(mu0=0,muA=0.5,sigma=1.2,n=30,alpha=0.01)

#

typeII.tester(mu0=0,muA=0.5,sigma=1.2,n=20,alpha=0.01)
typeII.tester(mu0=0,muA=0.5,sigma=1.2,n=40,alpha=0.01)

#

typeII.tester(mu0=0,muA=0.4,sigma=1.2,n=40,alpha=0.01)
typeII.tester(mu0=0,muA=0.6,sigma=1.2,n=40,alpha=0.01)


## 18.5.4 ##

power.tester <- function(nvec,...){
  nlen <- length(nvec)
  result <- rep(NA,nlen)
  pbar <- txtProgressBar(min=0,max=nlen,style=3)
  for(i in 1:nlen){
    result[i] <- 1-typeII.tester(n=nvec[i],...)
    setTxtProgressBar(pbar,i)
  }
  close(pbar)
  return(result)
}

#

sample.sizes <- 5:100

#

pow <- power.tester(nvec=sample.sizes,mu0=0,muA=0.6,sigma=1.2,alpha=0.01,ITERATIONS=5000)
pow

#

minimum.n <- sample.sizes[min(which(pow>=0.8))]
minimum.n

#

pow2 <- power.tester(nvec=sample.sizes,mu0=0,muA=0.6,sigma=1.2,alpha=0.05,ITERATIONS=5000)
minimum.n2 <- sample.sizes[min(which(pow2>0.8))]
minimum.n2

#

plot(sample.sizes,pow,xlab="sample size n",ylab="simulated power")
points(sample.sizes,pow2,col="gray")
abline(h=0.8,lty=2)
abline(v=c(minimum.n,minimum.n2),lty=3,col=c("black","gray"))
legend("bottomright",legend=c("alpha=0.01","alpha=0.05"),col=c("black","gray"),pch=1)


## 18.5.5 ##

# no R Code




##################
### CHAPTER 19 ###
##################

################
# Section 19.1 #
################

## 19.1.1 ##

table(chickwts$feed)
chick.means <- tapply(chickwts$weight,INDEX=chickwts$feed,FUN=mean)
chick.means

#

boxplot(chickwts$weight~chickwts$feed)
points(1:6,chick.means,pch=4,cex=1.5)

#

chick.sds <- tapply(chickwts$weight,INDEX=chickwts$feed,FUN=sd)
chick.sds

#

chick.meancen <- chickwts$weight-chick.means[as.numeric(chickwts$feed)]

#

qqnorm(chick.meancen,main="Normal QQ plot of residuals")
qqline(chick.meancen)


## 19.1.2 ##

# no R code


## 19.1.3 ##

chick.anova <- aov(weight~feed,data=chickwts)

#

summary(chick.anova)

#

195556/65



################
# Section 19.2 #
################

## 19.2.1 ##

tapply(warpbreaks$breaks,INDEX=list(warpbreaks$wool,warpbreaks$tension),FUN=mean)

#

wb.means <- aggregate(warpbreaks$breaks,by=list(warpbreaks$wool,warpbreaks$tension),FUN=mean)
wb.means


## 19.2.2 ##

summary(aov(breaks~wool,data=warpbreaks))
summary(aov(breaks~tension,data=warpbreaks))

#

summary(aov(breaks~wool+tension,data=warpbreaks))

#

summary(aov(breaks~wool+tension+wool:tension,data=warpbreaks))

#

interaction.plot(x.factor=wb.means[,2],trace.factor=wb.means[,1],response=wb.means$x,trace.label="wool",xlab="tension",ylab="mean warp breaks")



################
# Section 19.3 #
################

library("MASS")
boxplot(Age~Smoke,data=survey)
age.means <- tapply(survey$Age,survey$Smoke,mean)
age.meancen <- survey$Age-age.means[as.numeric(survey$Smoke)]
qqnorm(age.meancen,main="Normal QQ plot of residuals")
qqline(age.meancen)

#

tapply(survey$Age,survey$Smoke,sd)

#

kruskal.test(Age~Smoke,data=survey)




##################
### CHAPTER 20 ###
##################

################
# Section 20.1 #
################

library("MASS")
plot(survey$Height~survey$Wr.Hnd,xlab="Writing handspan (cm)",ylab="Height (cm)")

#

cor(survey$Wr.Hnd,survey$Height,use="complete.obs")

#

incomplete.obs <- which(is.na(survey$Height)|is.na(survey$Wr.Hnd))
length(incomplete.obs)



################
# Section 20.2 #
################

## 20.2.1 ##

# no R code


## 20.2.2 ##

# no R code


## 20.2.3 ##

survfit <- lm(Height~Wr.Hnd,data=survey)

#

survfit

#

plot(survey$Height~survey$Wr.Hnd,xlab="Writing handspan (cm)",ylab="Height (cm)")
abline(survfit,lwd=2)


## 20.2.4 ##

obsA <- c(survey$Wr.Hnd[197],survey$Height[197])
obsA
obsB <- c(survey$Wr.Hnd[154],survey$Height[154])
obsB

#

names(survfit)

#

mycoefs <- coef(survfit)
mycoefs
beta0.hat <- mycoefs[1]
beta1.hat <- mycoefs[2]

#

plot(survey$Height~survey$Wr.Hnd,xlab="Writing handspan (cm)",ylab="Height (cm)")
abline(survfit,lwd=2)
segments(x0=c(obsA[1],obsB[1]),y0=beta0.hat+beta1.hat*c(obsA[1],obsB[1]),x1=c(obsA[1],obsB[1]),y1=c(obsA[2],obsB[2]),lty=2)



################
# Section 20.3 #
################

## 20.3.1 ##

summary(survfit)


## 20.3.2 ##

confint(survfit,level=0.95)


## 20.3.3 ##

rho.xy <- cor(survey$Wr.Hnd,survey$Height,use="complete.obs")
rho.xy^2


## 20.3.4 ##

names(summary(survfit))

#

summary(survfit)$sigma



################
# Section 20.4 #
################

## 20.4.1 ##

# no R code


## 20.4.2 ##

as.numeric(beta0.hat+beta1.hat*14.5)
as.numeric(beta0.hat+beta1.hat*24)

#

xvals <- data.frame(Wr.Hnd=c(14.5,24))
xvals
mypred.ci <- predict(survfit,newdata=xvals,interval="confidence",level=0.95)
mypred.ci

#

mypred.pi <- predict(survfit,newdata=xvals,interval="prediction",level=0.95)
mypred.pi


## 20.4.3 ##

plot(survey$Height~survey$Wr.Hnd,xlim=c(13,24),ylim=c(140,205),xlab="Writing handspan (cm)",ylab="Height (cm)")
abline(survfit,lwd=2)

#

points(xvals[,1],mypred.ci[,1],pch=8)
segments(x0=c(14.5,24),x1=c(14.5,24),y0=c(mypred.pi[1,2],mypred.pi[2,2]),y1=c(mypred.pi[1,3],mypred.pi[2,3]),col="gray",lwd=3)
segments(x0=c(14.5,24),x1=c(14.5,24),y0=c(mypred.ci[1,2],mypred.ci[2,2]),y1=c(mypred.ci[1,3],mypred.ci[2,3]),lwd=2)

#

xseq <- data.frame(Wr.Hnd=seq(12,25,length=100))
ci.band <- predict(survfit,newdata=xseq,interval="confidence",level=0.95)
pi.band <- predict(survfit,newdata=xseq,interval="prediction",level=0.95)

#

lines(xseq[,1],ci.band[,2],lty=2)
lines(xseq[,1],ci.band[,3],lty=2)
lines(xseq[,1],pi.band[,2],lty=2,col="gray")
lines(xseq[,1],pi.band[,3],lty=2,col="gray")
legend("topleft",legend=c("Fit","95% CI","95% PI"),lty=c(1,2,2),col=c("black","black","gray"),lwd=c(2,1,1))


## 20.4.4 ##

predict(survfit,newdata=data.frame(Wr.Hnd=50),interval="confidence",level=0.95)



################
# Section 20.5 #
################

## 20.5.1 ##

class(survey$Sex)
table(survey$Sex)

#

plot(survey$Height~survey$Sex)

#

points(survey$Height~as.numeric(survey$Sex),cex=0.5)

#

means.sex <- tapply(survey$Height,INDEX=survey$Sex,FUN=mean,na.rm=TRUE)
means.sex
points(1:2,means.sex,pch=4,cex=3)

#

survfit2 <- lm(Height~Sex,data=survey)
summary(survfit2)

#

extra.obs <- factor(c("Female","Male","Male","Male","Female"))
extra.obs
predict(survfit2,newdata=data.frame(Sex=extra.obs),interval="confidence",level=0.9)


## 20.5.2 ##

is.factor(survey$Smoke)
table(survey$Smoke)
levels(survey$Smoke)

#

boxplot(Height~Smoke,data=survey)
points(1:4,tapply(survey$Height,survey$Smoke,mean,na.rm=TRUE),pch=4)

#

survfit3 <- lm(Height~Smoke,data=survey)
summary(survfit3)

#

one.of.each <- factor(levels(survey$Smoke))
one.of.each
predict(survfit3,newdata=data.frame(Smoke=one.of.each),interval="confidence",level=0.95)


## 20.5.3 ##

SmokeReordered <- relevel(survey$Smoke,ref="Never")
levels(SmokeReordered)


## 20.5.4 ##

boxplot(mtcars$mpg~mtcars$cyl,xlab="Cylinders",ylab="MPG")

#

class(mtcars$cyl)
carfit <- lm(mpg~cyl,data=mtcars)
summary(carfit)

#

plot(mtcars$mpg~mtcars$cyl,xlab="Cylinders",ylab="MPG")
abline(carfit,lwd=2)

#

carfit <- lm(mpg~factor(cyl),data=mtcars)
summary(carfit)


## 20.5.5 ##

summary(aov(Height~Smoke,data=survey))

#

sqrt(96.27)




##################
### CHAPTER 21 ###
##################

################
# Section 21.1 #
################

# no R code


################
# Section 21.2 #
################

## 21.2.1 ##

# no R code


## 21.2.2 ##

# No R code


## 21.2.3 ##

demo.data <- data.frame(y=c(1.55,0.42,1.29,0.73,0.76,-1.09,1.41,-0.32),x1=c(1.13,-0.73,0.12,0.52,-0.54,-1.15,0.20,-1.09),x2=c(1,0,1,1,0,1,0,1))
demo.data

#

Y <- matrix(demo.data$y) 
Y
n <- nrow(demo.data)
X <- matrix(c(rep(1,n),demo.data$x1,demo.data$x2),nrow=n,ncol=3)
X

#

BETA.HAT <- solve(t(X)%*%X)%*%t(X)%*%Y
BETA.HAT



################
# Section 21.3 #
################

## 21.3.1 ##

demo.fit <- lm(y~x1+x2,data=demo.data)
coef(demo.fit)

#

library("MASS")
survmult <- lm(Height~Wr.Hnd+Sex,data=survey)
summary(survmult)

#

survmult2 <- lm(Height~Wr.Hnd+Sex+Smoke,data=survey)
summary(survmult2)


## 21.3.2 ##

# no R code


## 21.3.3 ##

survcoefs <- coef(survmult)
survcoefs
as.numeric(survcoefs[1]+survcoefs[3])

#

plot(survey$Height~survey$Wr.Hnd,col=c("gray","black")[as.numeric(survey$Sex)],pch=16,xlab="Writing handspan",ylab="Height")
abline(a=survcoefs[1],b=survcoefs[2],col="gray",lwd=2)
abline(a=survcoefs[1]+survcoefs[3],b=survcoefs[2],col="black",lwd=2)
legend("topleft",legend=levels(survey$Sex),col=c("gray","black"),pch=16)


## 21.3.4 ##

confint(survmult2)


## 21.3.5 ##

R2 <- summary(survmult2)$r.squared
R2

#

n <- nrow(survey)-30
n

#

p <- length(coef(survmult2))-1
p

#

n-p-1

#

Fstat <- (R2*(n-p-1))/((1-R2)*p)
Fstat
1-pf(Fstat,df1=p,df2=n-p-1)


## 21.3.6 ##

predict(survmult,newdata=data.frame(Wr.Hnd=16.5,Sex="Male"),interval="confidence",level=0.95)

#

predict(survmult,newdata=data.frame(Wr.Hnd=13,Sex="Female"),interval="prediction",level=0.99)

#

survey[survey$Sex=="Female" & survey$Wr.Hnd==13,c("Sex","Wr.Hnd","Height")]



################
# Section 21.4 #
################

## 21.4.1 ##

x <- seq(-4,4,length=50)
y <- x
y2 <- x + x^2
y3 <- x + x^2 + x^3

#

plot(x,y,type="l")
plot(x,y2,type="l")
plot(x,y3,type="l")

#

plot(mtcars$disp,mtcars$mpg,xlab="Displacement (cu. in.)",ylab="MPG")

#

car.order1 <- lm(mpg~disp,data=mtcars)
summary(car.order1)

#

car.order2 <- lm(mpg~disp+I(disp^2),data=mtcars)
summary(car.order2)

#

car.order3 <- lm(mpg~disp+I(disp^2)+I(disp^3),data=mtcars)
summary(car.order3)

#

plot(mtcars$disp,mtcars$mpg,xlab="Displacement (cu. in.)",ylab="MPG")
abline(car.order1)

#

disp.seq <- seq(min(mtcars$disp)-50,max(mtcars$disp)+50,length=30)

#

car.order2.pred <- predict(car.order2,newdata=data.frame(disp=disp.seq))
lines(disp.seq,car.order2.pred,lty=2)

#

car.order3.pred <- predict(car.order3,newdata=data.frame(disp=disp.seq))
lines(disp.seq,car.order3.pred,lty=3)
legend("topright",lty=1:3,legend=c("order 1 (linear)","order 2 (quadratic)","order 3 (cubic)"))


## 21.4.2 ##

plot(1:1000,log(1:1000),type="l",xlab="x",ylab="",ylim=c(-8,8))
lines(1:1000,-log(1:1000),lty=2)
legend("topleft",legend=c("log(x)","-log(x)"),lty=c(1,2))

#

plot(mtcars$hp,mtcars$mpg,pch=19,col=c("black","gray")[factor(mtcars$am)],xlab="Horsepower",ylab="MPG")
legend("topright",legend=c("auto","man"),col=c("black","gray"),pch=19)

#

car.log <- lm(mpg~log(hp)+am,data=mtcars)
summary(car.log)

#

hp.seq <- seq(min(mtcars$hp)-20,max(mtcars$hp)+20,length=30)
n <- length(hp.seq)
car.log.pred <- predict(car.log,newdata=data.frame(hp=rep(hp.seq,2),am=rep(c(0,1),each=n)))

#

lines(hp.seq,car.log.pred[1:n])
lines(hp.seq,car.log.pred[(n+1):(2*n)],col="gray")


## 21.4.3 ##

# no R code


################
# Section 21.5 #
################

## 21.5.1 ##

# No R code


## 21.5.2 ##

library("faraway")
dia.fit <- lm(chol~age+frame+age:frame,data=diabetes)
summary(dia.fit)

#

dia.coef <- coef(dia.fit)
dia.coef

#

dia.small <- c(dia.coef[1],dia.coef[2])
dia.small
dia.medium <- c(dia.coef[1]+dia.coef[3],dia.coef[2]+dia.coef[5])
dia.medium
dia.large <- c(dia.coef[1]+dia.coef[4],dia.coef[2]+dia.coef[6])
dia.large

#

cols <- c("black","darkgray","lightgray")
plot(diabetes$chol~diabetes$age,col=cols[diabetes$frame],cex=0.5,xlab="age",ylab="cholesterol")
abline(coef=dia.small,lwd=2)
abline(coef=dia.medium,lwd=2,col="darkgray")
abline(coef=dia.large,lwd=2,col="lightgray")
legend("topright",legend=c("small frame","medium frame","large frame"),lty=1,lwd=2,col=cols)


## 21.5.3 ##

warp.fit <- lm(breaks~wool*tension,data=warpbreaks)
summary(warp.fit)

#

nd <- data.frame(wool=c("A","A","B","B"),tension=c("L","H","L","M"))
predict(warp.fit,newdata=nd,interval="confidence",level=0.9)


## 21.5.4 ##

car.fit <- lm(mpg~hp*wt,data=mtcars)
summary(car.fit)


## 21.5.5 ##

library("boot")
nuc.fit <- lm(cost~cap+cum.n*ne*ct,data=nuclear)
summary(nuc.fit)




##################
### CHAPTER 22 ###
##################

################
# Section 22.1 #
################

## 22.1.1 ##

# no R code


## 22.1.2 ##

# no R code



################
# Section 22.2 #
################

## 22.2.1 ##

library("MASS")
survmult <- lm(Height~Wr.Hnd+Sex,data=survey)
survmult2 <- lm(Height~Wr.Hnd+Sex+Smoke,data=survey)
survmult
survmult2

#

anova(survmult,survmult2)

#

library("faraway")
diab <- diabetes[-which(is.na(diabetes$age) | is.na(diabetes$frame)),]

#

dia.model1 <- lm(chol~1,data=diab)
dia.model2 <- lm(chol~age,data=diab)
dia.model3 <- lm(chol~age+frame,data=diab)
dia.model4 <- lm(chol~age*frame,data=diab)

#

anova(dia.model1,dia.model2,dia.model3,dia.model4)


## 22.2.2 ##

library(boot)
nuc.0 <- lm(cost~1,data=nuclear)
summary(nuc.0)

#

add1(nuc.0,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")

#

nuc.1 <- update(nuc.0,formula=.~.+date)
summary(nuc.1)

#

add1(nuc.1,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")

#

nuc.2 <- update(nuc.1,formula=.~.+cap)
add1(nuc.2,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")

#

nuc.3 <- update(nuc.2,formula=.~.+pt)
add1(nuc.3,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")

#

nuc.4 <- update(nuc.3,formula=.~.+ne)

#

add1(nuc.4,scope=.~.+date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,test="F")

#

summary(nuc.4)


## 22.2.3 ##

nuc.0 <- lm(cost~date+t1+t2+cap+pr+ne+ct+bw+cum.n+pt,data=nuclear)
summary(nuc.0)

#

drop1(nuc.0,test="F")

#

nuc.1 <- update(nuc.0,.~.-bw)

#

drop1(nuc.1,test="F")

#

nuc.2 <- update(nuc.1,.~.-pt)
drop1(nuc.2,test="F")

#

nuc.3 <- update(nuc.2,.~.-t1)
drop1(nuc.3,test="F")

#

nuc.4 <- update(nuc.3,.~.-ct)
drop1(nuc.4,test="F")

#

summary(nuc.4)


## 22.2.4 ##

car.null <- lm(mpg~1,data=mtcars)

#

car.step <- step(car.null,scope=.~.+wt*hp*factor(cyl)*disp+am+factor(gear)+drat+vs+qsec+carb)

#

summary(car.step)


## 22.2.5 ##

# no R code



################
# Section 22.3 #
################

## 22.3.1 ##

car.step <- step(car.null,scope=.~.+wt*hp*factor(cyl)*disp+am+factor(gear)+drat+vs+qsec+carb)
plot(car.step,which=1)

#

plot(car.step,which=3,add.smooth=FALSE,id.n=2)

#

gal <- data.frame(d=c(573,534,495,451,395,337,253),h=c(1,0.8,0.6,0.45,0.3,0.2,0.1))
gal.mod1 <- lm(d~h,data=gal)
gal.mod2 <- lm(d~h+I(h^2),data=gal)

#

plot(gal$d~gal$h,xlab="Height",ylab="Distance")
abline(gal.mod1)
plot(gal.mod1,which=1,id.n=0)
plot(gal.mod2,which=1,id.n=0)


## 22.3.2 ##

plot(car.step,which=2)

#

shapiro.test(rstandard(car.step))


## 22.3.3 ##

x <- c(1.1,1.3,2.3,1.6,1.2,0.1,1.8,1.9,0.2,0.75)
y <- c(6.7,7.9,9.8,9.3,8.2,2.9,6.6,11.1,4.7,3)

#

p1x <- 1.2
p1y <- 14
p2x <- 5
p2y <- 19
p3x <- 5
p3y <- 5

#

mod.0 <- lm(y~x)
mod.1 <- lm(c(y,p1y)~c(x,p1x))
mod.2 <- lm(c(y,p2y)~c(x,p2x))
mod.3 <- lm(c(y,p3y)~c(x,p3x))

#

plot(x,y,xlim=c(0,5),ylim=c(0,20))

#

points(p1x,p1y,pch=15,cex=1.5)
abline(mod.0)
abline(mod.1,lty=2)
text(2.5,1,labels="Outlier, low leverage, low influence",cex=1.4)

#

plot(x,y,xlim=c(0,5),ylim=c(0,20))
points(p2x,p2y,pch=15,cex=1.5)
abline(mod.0)
abline(mod.2,lty=2)
text(2.5,1,labels="Not outlier, high leverage, low influence",cex=1.4)

#

plot(x,y,xlim=c(0,5),ylim=c(0,20))
points(p3x,p3y,pch=15,cex=1.5)
abline(mod.0)
abline(mod.3,lty=2)
text(2.5,1,labels="Outlier, high leverage, high influence",cex=1.4)


## 22.3.4 ##

X <- cbind(rep(1,10),x)
hii <- diag(X%*%solve(t(X)%*%X)%*%t(X))
hii
plot(hii~x,ylab="Leverage",main="",pch=4)

#

hatvalues(mod.0)


## 22.3.5 ##

x1 <- c(x,p1x)
y1 <- c(y,p1y)
n <- length(x1)
param <- length(coef(mod.1))
yhat.full <- fitted(mod.1)
sigma <- summary(mod.1)$sigma
cooks <- rep(NA,n)
for(i in 1:n){
  temp.y <- y1[-i]
  temp.x <- x1[-i]
  temp.model <- lm(temp.y~temp.x)
  temp.fitted <- predict(temp.model,newdata=data.frame(temp.x=x1))
  cooks[i] <- sum((yhat.full-temp.fitted)^2)/(param*sigma^2)
}

#

cooks 

#

cooks.distance(mod.1)

#

plot(mod.1,which=4)
plot(mod.2,which=4)
plot(mod.3,which=4)
abline(h=c(1,4/n),lty=2)

#

plot(car.step,which=4)
abline(h=4/nrow(mtcars),lty=2)


## 22.3.6 ##

plot(mod.1,which=5,add.smooth=FALSE,cook.levels=c(4/11,0.5,1))
plot(mod.2,which=5,add.smooth=FALSE,cook.levels=c(4/11,0.5,1))
plot(mod.3,which=5,add.smooth=FALSE,cook.levels=c(4/11,0.5,1))

#

plot(mod.1,which=6,add.smooth=FALSE)
plot(mod.2,which=6,add.smooth=FALSE)
plot(mod.3,which=6,add.smooth=FALSE)

#

plot(car.step,which=5,cook.levels=c(4/nrow(mtcars),0.5,1))
plot(car.step,which=6,cook.levels=c(4/nrow(mtcars),0.5,1))



################
# Section 22.4 #
################

## 22.4.1 ##

# no R code


## 22.4.2 ##

cor(survey$Wr.Hnd,survey$NW.Hnd,use="complete.obs")

#

summary(lm(Height~Wr.Hnd,data=survey))

#

summary(lm(Height~NW.Hnd,data=survey))

#

summary(lm(Height~Wr.Hnd+NW.Hnd,data=survey))

