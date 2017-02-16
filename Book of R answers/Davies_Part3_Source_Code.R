options(prompt="R> ")

##################
### CHAPTER 13 ###
##################

################
# Section 13.1 #
################

## 13.1.1 ##

# no R code


## 13.1.2 ##

chickwts[1:5,]

#

chickwts$weight
chickwts$feed


## 13.1.3 ##

quakes[1:5,]

#

plot(quakes$long,quakes$lat,xlab="Longitude",ylab="Latitude")


## 13.1.4 ##

# no R code



################
# Section 13.2 #
################

## 13.2.1 ##

xdata <- c(2,4.4,3,3,2,2.2,2,4)

#

x.bar <- mean(xdata)
x.bar
m.bar <- median(xdata)
m.bar

#

xtab <- table(xdata)
xtab

#

min(xdata)
max(xdata)
range(xdata)

#

max(xtab)

#

d.bar <- xtab[xtab==max(xtab)]
d.bar

#

mean(chickwts$weight)
median(chickwts$weight)

#

Qtab <- table(quakes$mag)
Qtab[Qtab==max(Qtab)]

#

mean(c(1,4,NA))
mean(c(1,4,NaN))

#

mean(c(1,4,NA),na.rm=TRUE)
mean(c(1,4,NaN),na.rm=TRUE)

#

mean(chickwts$weight[chickwts$feed=="casein"])
mean(chickwts$weight[chickwts$feed=="horsebean"])
mean(chickwts$weight[chickwts$feed=="linseed"])
mean(chickwts$weight[chickwts$feed=="meatmeal"])
mean(chickwts$weight[chickwts$feed=="soybean"])
mean(chickwts$weight[chickwts$feed=="sunflower"])

#

tapply(chickwts$weight,INDEX=chickwts$feed,FUN=mean)


## 13.2.2 ##

table(chickwts$feed)

#

table(chickwts$feed)/nrow(chickwts)

#

sum(chickwts$feed=="soybean")/nrow(chickwts)
mean(chickwts$feed=="soybean")

#

mean(chickwts$feed=="soybean"|chickwts$feed=="horsebean")

#

tapply(chickwts$weight,INDEX=chickwts$feed,FUN=function(x) length(x)/nrow(chickwts))

#

round(table(chickwts$feed)/nrow(chickwts),digits=3)

#

round(mean(chickwts$feed=="soybean")*100,1)


## 13.2.3 ##

xdata <- c(2,4.4,3,3,2,2.2,2,4)
quantile(xdata,prob=0.8)

#

quantile(xdata,prob=c(0,0.25,0.5,0.75,1))

#

summary(xdata)

#

quantile(chickwts$weight,prob=c(0.25,0.75))

#

summary(quakes$mag[quakes$depth<400])


## 13.2.4 ##

xdata <- c(2,4.4,3,3,2,2.2,2,4)

#

ydata <- c(1,4.4,1,3,2,2.2,2,7)

#

mean(xdata)
mean(ydata)

#

plot(xdata,type="n",xlab="",ylab="data vector",yaxt="n",bty="n")
abline(h=c(3,3.5),lty=2,col="gray")
abline(v=2.825,lwd=2,lty=3)
text(c(0.8,0.8),c(3,3.5),labels=c("x","y"))
points(jitter(c(xdata,ydata)),c(rep(3,length(xdata)),rep(3.5,length(ydata))))

#

var(xdata)
sd(xdata)
IQR(xdata)

#

sqrt(var(xdata))
as.numeric(quantile(xdata,0.75)-quantile(xdata,0.25))

#

sd(ydata)
IQR(ydata)

#

sd(chickwts$weight)
IQR(chickwts$weight)

#

IQR(quakes$mag[quakes$depth<400])


## 13.2.5 ##

xdata <- c(2,4.4,3,3,2,2.2,2,4)
ydata <- c(1,4.4,1,3,2,2.2,2,7)
cov(xdata,ydata)
cov(xdata,ydata)/(sd(xdata)*sd(ydata))
cor(xdata,ydata)

#

plot(xdata,ydata,pch=13,cex=1.5)

#

plot(quakes$mag,quakes$stations,xlab="Magnitude",ylab="No. of stations")

#

cov(quakes$mag,quakes$stations)

#

cor(quakes$mag,quakes$stations)


## 13.2.6 ##

foo <- c(0.6,-0.6,0.1,-0.2,-1.0,0.4,0.3,-1.8,1.1,6.0)

#

plot(foo,rep(0,10),yaxt="n",ylab="",bty="n",cex=2,cex.axis=1.5,cex.lab=1.5)
abline(h=0,col="gray",lty=2)
arrows(5,0.5,5.9,0.1,lwd=2)
text(5,0.7,labels="outlier?",cex=3)

#

bar <- c(0.1,0.3,1.3,0.6,0.2,-1.7,0.8,0.9,-0.8,-1.0)
baz <- c(-0.3,0.9,2.8,2.3,1.2,-4.1,-0.4,4.1,-2.3,-100.0)

#

plot(bar,baz,axes=T,cex=2,cex.axis=1.5,cex.lab=1.5)
arrows(-0.5,-80,-0.94,-97,lwd=2)
text(-0.45,-74,labels="outlier?",cex=3)

#

mean(foo)

#

mean(foo[-10])

#

cor(bar,baz)
cor(bar[-10],baz[-10])




##################
### CHAPTER 14 ###
##################

################
# Section 14.1 #
################

## 14.1.1 ##

mtcars[1:5,]

#

cyl.freq <- table(mtcars$cyl)
cyl.freq

#

barplot(cyl.freq)

#

table(mtcars$cyl[mtcars$am==0])
table(mtcars$cyl[mtcars$am==1])

#

cyl.freq.matrix <- table(mtcars$am,mtcars$cyl)
cyl.freq.matrix

#

barplot(cyl.freq.matrix,beside=TRUE,horiz=TRUE,las=1,main="Performance car counts\nby transmission and cylinders",names.arg=c("V4","V6","V8"),legend.text=c("auto","manual"),args.legend=list(x="bottomright"))

#

library("ggplot2")
qplot(factor(mtcars$cyl),geom="bar")

#

qplot(factor(mtcars$cyl),geom="blank",fill=factor(mtcars$am),xlab="",ylab="",main="Performance car counts\nby transmission and cylinders") + geom_bar(position="dodge") + scale_x_discrete(labels=c("V4","V6","V8")) + scale_y_continuous(breaks=seq(0,12,2)) + scale_fill_grey(name="Trans.",labels=c("auto","manual"))  + theme_bw() + coord_flip()


## 14.1.2 ##

pie(table(mtcars$cyl),labels=c("V4","V6","V8"),col=c("white","gray","black"),main="Performance cars by cylinders")



################
# Section 14.2 #
################

mtcars$hp

#

hist(mtcars$hp)

#

hist(mtcars$hp,breaks=seq(0,400,25),col="gray",main="Horsepower",xlab="HP")
abline(v=c(mean(mtcars$hp),median(mtcars$hp)),lty=c(2,3),lwd=2)
legend("topright",legend=c("mean HP","median HP"),lty=c(2,3),lwd=2)

#

library("ggplot2")
qplot(mtcars$hp)

#

qplot(mtcars$hp,geom="blank",main="Horsepower",xlab="HP") + geom_histogram(color="black",fill="white",breaks=seq(0,400,25),closed="right") + geom_vline(mapping=aes(xintercept=c(mean(mtcars$hp),median(mtcars$hp)),linetype=factor(c("mean","median"))),show.legend=TRUE) + scale_linetype_manual(values=c(2,3)) + labs(linetype="")



################
# Section 14.3 #
################

## 14.3.1 ##

hist(quakes$mag)
boxplot(quakes$mag)


## 14.3.2 ##

stations.fac <- cut(quakes$stations,breaks=c(0,50,100,150))
stations.fac[1:5]

#

boxplot(quakes$mag~stations.fac,xlab="# stations detected",ylab="Magnitude",col="gray")

#

library("ggplot2")
qplot(stations.fac,quakes$mag,geom="boxplot",xlab="# stations detected",ylab="Magnitude")



################
# Section 14.4 #
################

iris[1:5,]


## 14.4.1 ##

plot(iris[,4],iris[,3],type="n",xlab="Petal Width (cm)",ylab="Petal Length (cm)")
points(iris[iris$Species=="setosa",4],iris[iris$Species=="setosa",3],pch=19,col="black")
points(iris[iris$Species=="virginica",4],iris[iris$Species=="virginica",3],pch=19,col="gray")
points(iris[iris$Species=="versicolor",4],iris[iris$Species=="versicolor",3],pch=1,col="black")
legend("topleft",legend=c("setosa","virginica","versicolor"),col=c("black","gray","black"),pch=c(19,19,1))

#

iris_pch <- rep(19,nrow(iris))
iris_pch[iris$Species=="versicolor"] <- 1
iris_col <- rep("black",nrow(iris))
iris_col[iris$Species=="virginica"] <- "gray"

#

plot(iris[,4],iris[,3],col=iris_col,pch=iris_pch,xlab="Petal Width (cm)",ylab="Petal Length (cm)")


## 14.4.2 ##

pairs(iris[,1:4],pch=iris_pch,col=iris_col,cex=0.75)

#

library("ggplot2")
qplot(iris[,4],iris[,3],xlab="Petal width",ylab="Petal length",shape=iris$Species) + scale_shape_manual(values=4:6) + labs(shape="Species")

#

library("GGally")
ggpairs(iris,mapping=aes(col=Species),axisLabels="internal")




##################
### CHAPTER 15 ###
##################

################
# Section 15.1 #
################

## 15.1.1 ##

# no R code


## 15.1.2 ##

# no R code


## 15.1.3 ##

(2/3)*(1/2)


## 15.1.4 ##

(1/2)+(1/2)-(1/3)


## 15.1.5 ##

# no R code



################
# Section 15.2 #
################

## 15.2.1 ##

# no R code


## 15.2.2 ##

X.outcomes <- c(-4,0,1,8)
X.prob <- c(0.32,0.48,0.15,0.05)
barplot(X.prob,ylim=c(0,0.5),names.arg=X.outcomes,space=0,xlab="x",ylab="Pr(X = x)")

#

X.cumul <- cumsum(X.prob)
X.cumul

#

barplot(X.cumul,names.arg=X.outcomes,space=0,xlab="x",ylab="Pr(X <= x)")

#

mu.X <- sum(X.outcomes*X.prob)
mu.X

#

var.X <- sum((X.outcomes-mu.X)^2*X.prob)
var.X

#

sd.X <- sqrt(var.X)
sd.X


## 15.2.3 ##

w <- seq(35,95,by=5)
w
lower.w <- w>=40 & w<=65
lower.w
upper.w <- w>65 & w<=90
upper.w

#

fw <- rep(0,length(w))
fw[lower.w] <- (w[lower.w]-40)/625
fw[upper.w] <- (90-w[upper.w])/625
fw

#

plot(w,fw,type="l",ylab="f(w)")
abline(h=0,col="gray",lty=2)

#

0.5*50*0.04

#

fw.specific <- (55.2-40)/625
fw.specific

#

fw.specific.area <- 0.5*15.2*fw.specific
fw.specific.area

#

fw.specific.vertices <- rbind(c(40,0),c(55.2,0),c(55.2,fw.specific))
fw.specific.vertices
plot(w,fw,type="l",ylab="f(w)")
abline(h=0,col="gray",lty=2)
polygon(fw.specific.vertices,col="gray",border=NA)
abline(v=55.2,lty=3)
text(50,0.005,labels=fw.specific.area)

#

(55.2^2-80*55.2-40^2+80*40)/1250

#

Fw <- rep(0,length(w))
Fw[lower.w] <- (w[lower.w]^2-80*w[lower.w]+1600)/1250
Fw[upper.w] <- (180*w[upper.w]-w[upper.w]^2-6850)/1250
Fw[w>90] <- 1
plot(w,Fw,type="l",ylab="F(w)")
abline(h=c(0,1),col="gray",lty=2)

#

abline(v=55.2,lty=3)
abline(h=fw.specific.area,lty=3)

#

plot(w,w*fw,type="l",ylab="wf(w)")
plot(w,(w-65)^2*fw,type="l",ylab="(w-65)^2 f(w)")

#

sqrt(104.1667)


## 15.2.4 ##

# no R code




##################
### CHAPTER 16 ###
##################

################
# Section 16.1 #
################

## 16.1.1 ##

# no R code


## 16.1.2 ##

dbinom(x=5,size=8,prob=1/6)

#

X.prob <- dbinom(x=0:8,size=8,prob=1/6)
X.prob

#

sum(X.prob)

#

round(X.prob,3)

#

8/6
8*(1/6)*(5/6)

#

barplot(X.prob,names.arg=0:8,space=0,xlab="x",ylab="Pr(X = x)")

#

sum(dbinom(x=0:3,size=8,prob=1/6))
pbinom(q=3,size=8,prob=1/6)

#

1-pbinom(q=2,size=8,prob=1/6)

#

qbinom(p=0.95,size=8,prob=1/6)

#

rbinom(n=1,size=8,prob=1/6)
rbinom(n=1,size=8,prob=1/6)
rbinom(n=1,size=8,prob=1/6)
rbinom(n=3,size=8,prob=1/6)


## 16.1.3 ##

dpois(x=3,lambda=3.22)
dpois(x=0,lambda=3.22)
round(dpois(0:10,3.22),3)

#

(3.22^3*exp(-3.22))/prod(3:1)

#

barplot(dpois(x=0:10,lambda=3.22),ylim=c(0,0.25),space=0,names.arg=0:10,ylab="Pr(X=x)",xlab="x")

#

ppois(q=2,lambda=3.22)
1-ppois(q=5,lambda=3.22)

#

barplot(ppois(q=0:10,lambda=3.22),ylim=0:1,space=0,names.arg=0:10,ylab="Pr(X<=x)",xlab="x")

#

rpois(n=15,lambda=3.22)


## 16.1.4 ##

# no R code



################
# Section 16.2 #
################

## 16.2.1 ##

1/(0.41-0.223)

#

dunif(x=c(-2,-0.33,0,0.5,1.05,1.2),min=-0.4,max=1.1)

#

dunif(x=c(0.3,0,0.41),min=0.223,max=0.41)

#

a1 <- -4/10
b1 <- 11/10
unif1 <- 1/(b1-a1)
plot(c(a1,b1),rep(unif1,2),type="o",pch=19,xlim=c(a1-1/10,b1+1/10),ylim=c(0,0.75),ylab="f(x)",xlab="x")
abline(h=0,lty=2)
segments(c(a1-2,b1+2,a1,b1),rep(0,4),rep(c(a1,b1),2),rep(c(0,unif1),each=2),lty=rep(1:2,each=2))
points(c(a1,b1),c(0,0))

#

segments(c(-0.21,0.6),c(0,0),c(-0.21,0.6),rep(unif1,2),lty=3)

#

polygon(rbind(c(a1,0),c(a1,unif1),c(-0.21,unif1),c(-0.21,0)),col="gray",border=NA)

#

punif(q=-0.21,min=a1,max=b1)

#

1-punif(q=0.6,min=a1,max=b1)

#

punif(q=0.6,min=a1,max=b1) - punif(q=-0.21,min=a1,max=b1)

#

qunif(p=0.1266667,min=a1,max=b1)
qunif(p=1-1/3,min=a1,max=b1)

#

runif(n=10,min=a1,max=b1)


## 16.2.2 ##

xvals <- seq(-4,4,length=50)
fx <- dnorm(xvals,mean=0,sd=1)
fx

#

pnorm(q=1)-pnorm(q=-1)

#

mu <- -3.42
sigma <- 0.2
mu.minus.1sig <- mu-sigma
mu.minus.1sig
mu.plus.1sig <- mu+sigma
mu.plus.1sig
pnorm(q=mu.plus.1sig,mean=mu,sd=sigma) - pnorm(q=mu.minus.1sig,mean=mu,sd=sigma)

#

1-pnorm(mu.plus.1sig,mu,sigma)
pnorm(mu.minus.1sig,mu,sigma)

#

1-0.6826895

#

0.3173105/2

#

xvals <- seq(-5,-2,length=300)
fx <- dnorm(xvals,mean=mu,sd=sigma)
plot(xvals,fx,type="l",xlim=c(-4.4,-2.5),main="N(-3.42,0.2) distribution",xlab="x",ylab="f(x)")
abline(h=0,col="gray")
abline(v=c(mu.plus.1sig,mu.minus.1sig),lty=3:2)
legend("topleft",legend=c("-3.62\n(mean - 1 sd)","\n-3.22\n(mean + 1 sd)"),lty=2:3,bty="n")

#

xvals.sub <- xvals[xvals>=mu.minus.1sig & xvals<=mu.plus.1sig]
fx.sub <- fx[xvals>=mu.minus.1sig & xvals<=mu.plus.1sig]

#

polygon(rbind(c(mu.minus.1sig,0),cbind(xvals.sub,fx.sub),c(mu.plus.1sig,0)),border=NA,col="gray")

#

arrows(c(-4.2,-2.7,-2.9),c(0.5,0.5,1.2),c(-3.7,-3.15,-3.4),c(0.2,0.2,1))
text(c(-4.2,-2.7,-2.9),c(0.5,0.5,1.2)+0.05,labels=c("0.159","0.159","0.682"))

#

qnorm(p=0.159,mean=mu,sd=sigma)

#

qnorm(p=1-0.25,mean=mu,sd=sigma)

#

hist(chickwts$weight,main="",xlab="weight")
qqnorm(chickwts$weight,main="Normal QQ plot of weights")
qqline(chickwts$weight,col="gray")

#

rnorm(n=7,mu,sigma)

#

fakedata1 <- rnorm(n=71)
fakedata2 <- rnorm(n=710)
qqnorm(fakedata1,main="Normal QQ plot of generated N(0,1) data; n=710")
qqline(fakedata1,col="gray")
qqnorm(fakedata2,main="Normal QQ plot of generated N(0,1) data; n=710")
qqline(fakedata2,col="gray")

#

pnorm(78,80.2,1.1)

#

pnorm(81.5,80.2,1.1)-pnorm(80.5,80.2,1.1)

#

qnorm(0.2,80.2,1.1)

#

round(rnorm(5,80.2,1.1),1)


## 16.2.3 ##

qnorm(1-0.05)

#

qt(1-0.05,df=1)
qt(1-0.05,df=6)
qt(1-0.05,df=20)


## 16.2.4 ##

xvals <- seq(0,10,length=200)
plot(xvals,dexp(x=xvals,rate=1.65),xlim=c(0,8),ylim=c(0,1.65),type="l",xlab="x",ylab="f(x)")
lines(xvals,dexp(x=xvals,rate=1),lty=2)
lines(xvals,dexp(x=xvals,rate=0.4),lty=3)
abline(v=0,col="gray")
abline(h=0,col="gray")
legend("topright",legend=c("EXP(1.65)","EXP(1)","EXP(0.4)"),lty=1:3)

#

lambda.e <- 107/120
lambda.e

#

1-pexp(q=2.5,rate=lambda.e)

#

pexp(25/60,lambda.e)

#

qexp(p=0.15,lambda.e)


## 16.2.5 ##

# no R code

