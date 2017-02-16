options(prompt="R> ")

##################
### CHAPTER 23 ###
##################

################
# Section 23.1 #
################

## 23.1.1 ##

plot(quakes$long,quakes$lat)

#

dev.new()

#

hist(quakes$stations)


## 23.1.2 ##

dev.set(2)
plot(quakes$long,quakes$lat,cex=0.02*quakes$stations,xlab="Longitude",ylab="Latitude")

#

dev.set(3)
abline(v=mean(quakes$stations),lty=2)


## 23.1.3 ##

dev.off(2)

#

dev.off()


## 23.1.4 ##

dev.new(width=8,height=4)
par(mfrow=c(1,2))
plot(quakes$long,quakes$lat,cex=0.02*quakes$stations,xlab="Longitude",ylab="Latitude")
hist(quakes$stations)
abline(v=mean(quakes$stations),lty=2)

#

lay.mat <- matrix(c(1,3,2,3),2,2)
lay.mat

#

layout(mat=lay.mat)

#

layout.show(n=max(lay.mat))

#

plot(survey$Wr.Hnd,survey$Height,xlab="Writing handspan",ylab="Height")
boxplot(survey$Height~survey$Smoke,xlab="Smoking frequency",ylab="Height")
barplot(table(survey$Exer),horiz=TRUE,main="Exercise")



################
# Section 23.2 #
################

## 23.2.1 ##

par()$oma
par()$mar

#

plot(1:10)
box(which="figure",lty=2)


## 23.2.2 ##

par(oma=c(1,4,3,2),mar=4:7)
plot(1:10)
box("figure",lty=2)
box("outer",lty=3)

#

mtext("Figure region margins\nmar[ . ]",line=2)
mtext("Outer region margins\noma[ . ]",line=0.5,outer=TRUE)


## 23.2.3 ##

dev.new()
par(oma=c(1,1,5,1),mar=c(2,4,5,4))
boxplot(mtcars$mpg~mtcars$cyl,xaxt="n",ylab="MPG")
box("figure",lty=2)
box("outer",lty=3)
arrows(x0=c(2,2.5,3),y0=c(44,37,27),x1=c(1.25,2.25,3),y1=c(31,22,20),xpd=FALSE)
text(x=c(2,2.5,3),y=c(45,38,28),c("V4 cars","V6 cars","V8 cars"),xpd=FALSE)

#

par(oma=c(1,1,5,1),mar=c(2,4,5,4))
boxplot(mtcars$mpg~mtcars$cyl,xaxt="n",ylab="MPG")
box("figure",lty=2)
box("outer",lty=3)
arrows(x0=c(2,2.5,3),y0=c(44,37,27),x1=c(1.25,2.25,3),y1=c(31,22,20),xpd=TRUE)
text(x=c(2,2.5,3),y=c(45,38,28),c("V4 cars","V6 cars","V8 cars"),xpd=TRUE)

#

par(oma=c(1,1,5,1),mar=c(2,4,5,4))
boxplot(mtcars$mpg~mtcars$cyl,xaxt="n",ylab="MPG")
box("figure",lty=2)
box("outer",lty=3)
arrows(x0=c(2,2.5,3),y0=c(44,37,27),x1=c(1.25,2.25,3),y1=c(31,22,20),xpd=NA)
text(x=c(2,2.5,3),y=c(45,38,28),c("V4 cars","V6 cars","V8 cars"),xpd=NA)



################
# Section 23.3 #
################

## 23.3.1 ##

plot(1,1)
locator()


## 23.3.2 ##

plot(1,1)
Rtist <- locator(type="o",pch=4,lty=2,lwd=3,col="red",xpd=TRUE)
Rtist


## 23.3.3 ##

library("MASS")
plot(survey$Height~survey$Wr.Hnd,pch=16,col=c("gray","black")[as.numeric(survey$Sex)],xlab="Writing handspan",ylab="Height")

#

legend(locator(n=1),legend=levels(survey$Sex),pch=16,col=c("gray","black"))



################
# Section 23.4 #
################

## 23.4.1 ##

hp <- mtcars$hp
mpg <- mtcars$mpg
wtcex <- mtcars$wt/mean(mtcars$wt)

#

plot(hp,mpg,cex=wtcex)

#

plot(hp,mpg,cex=wtcex,xaxs="i",yaxs="i")

#

plot(hp,mpg,cex=wtcex,xaxt="n",yaxt="n",bty="n",xlab="",ylab="")

#

plot(hp,mpg,cex=wtcex,axes=FALSE,ann=FALSE)


## 23.4.2 ##

plot(hp,mpg,cex=wtcex,axes=FALSE,ann=FALSE)
box(bty="u")

#

plot(hp,mpg,cex=wtcex,axes=FALSE,ann=FALSE)
box(bty="l",lty=3,lwd=2)

#

plot(hp,mpg,cex=wtcex,axes=FALSE,ann=FALSE)
box(bty="]",lty=2,col="gray")


## 23.4.3 ##

hpseq <- seq(min(hp),max(hp),length=10)
plot(hp,mpg,cex=wtcex,xaxt="n",bty="n",ann=FALSE)
axis(side=1,at=hpseq)
axis(side=3,at=round(hpseq))

#

hpseq2 <- seq(50,325,by=25)
plot(hp,mpg,cex=wtcex,axes=FALSE)
box(bty="l")
axis(side=2,tcl=-2,las=1,mgp=c(3,2.5,0))
axis(side=1,at=hpseq2,tcl=1.5,mgp=c(3,1.5,1))



################
# Section 23.5 #
################

## 23.5.1 ##

par(mar=c(3,3,3,3))
plot(1,1,type="n",xlim=c(-1,1),ylim=c(0,7),xaxt="n",yaxt="n",ann=FALSE)

#

text(0,6,label="sans text (default)\nfamily=\"sans\", font=1")
text(0,5,label="serif text\nfamily=\"serif\", font=1",family="serif",font=1)
text(0,4,label="mono text\nfamily=\"mono\", font=1",family="mono",font=1,cex=1)
text(0,3,label="mono text (bold, italic)\nfamily=\"mono\", font=4",family="mono",font=4,cex=1)
text(0,2,label="sans text (italic)\nfamily=\"sans\", font=3",family="sans",font=3,cex=1)
text(0,1,label="serif text (bold)\nfamily=\"serif\", font=2",family="serif",font=2,cex=1)
mtext("some",line=1,at=-0.5,cex=2,family="sans")
mtext("different",line=1,at=0,cex=2,family="serif")
mtext("fonts",line=1,at=0.5,cex=2,family="mono")


## 23.5.2 ##

par(mar=c(3,3,3,3))
plot(1,1,type="n",xlim=c(-1,1),ylim=c(0.5,4.5),xaxt="n",yaxt="n",ann=FALSE)
text(0,4,label=expression(alpha),cex=1.5)
text(0,3,label=expression(paste("sigma: ",sigma,"    Sigma: ",Sigma)),family="mono",cex=1.5)
text(0,2,label=expression(paste(beta," ",gamma," ",Phi)),cex=1.5)
text(0,1,label=expression(paste(Gamma,"(",tau,") = 24 when ",tau," = 5")),family="serif",cex=1.5)
title(main=expression(paste("Gr",epsilon,epsilon,"k")),cex.main=2)


## 23.5.3 ##

expr1 <- expression(c^2==a[1]^2+b[1]^2)
expr2 <- expression(paste(pi^{x[i]},(1-pi)^(n-x[i])))
expr3 <- expression(paste("Sample mean:    ",italic(n)^{-1},sum(italic(x)[italic(i)],italic(i)==1,italic(n))==frac(italic(x)[1]+...+italic(x)[italic(n)],italic(n))))
expr4 <- expression(paste("f(x","|",alpha,",",beta,")"==frac(x^{alpha-1}~(1-x)^{beta-1},B(alpha,beta))))

#

par(mar=c(3,3,3,3))
plot(1,1,type="n",xlim=c(-1,1),ylim=c(0.5,4.5),xaxt="n",yaxt="n",ann=FALSE)
text(0,4:1,labels=c(expr1,expr2,expr3,expr4),cex=1.5)
title(main="Math",cex.main=2)



################
# Section 23.6 #
################

hp <- mtcars$hp
mpg <- mtcars$mpg
wtcex <- mtcars$wt/mean(mtcars$wt)
hpseq2 <- seq(50,325,by=25)

#

dev.new()
par(mar=c(5,4,4,4))
plot(hp,mpg,cex=wtcex,axes=FALSE,ann=FALSE)
box(bty="u")

#

axis(2,las=1,tcl=-0.8,family="mono")
axis(1,at=hpseq2,labels=FALSE,tcl=-1)

#

L100 <- seq(22,7,by=-3)
L100

#

MPG.L100 <- (100/L100*3.78541)/1.609
MPG.L100

#

axis(4,at=MPG.L100,labels=L100,las=1,tcl=0.3,mgp=c(3,0.3,0),family="mono")

#

express.L100 <- expression(paste(L/100,"km"%~~%frac(378.541,1.609%*%MPG)))

#

title(main="MPG by Horsepower",xlab="Horsepower",ylab="MPG",family="serif")
mtext(express.L100,side=4,line=3,family="serif")
text(hpseq2,rep(7.5,length(hpseq2)),labels=hpseq2,srt=45,xpd=TRUE,family="mono")

#

grid(col="darkgray")

#

legend(250,30,legend=rep("          ",3),pch=rep(1,3),pt.cex=c(1.5,1,0.5))
arrows(265,27,265,29,length=0.05)
text(locator(1),labels="Weight",cex=0.8,family="serif")



##################
### CHAPTER 24 ###
##################

library("ggplot2")

################
# Section 24.1 #
################

gg.static <- ggplot(data=mtcars,mapping=aes(x=hp)) + ggtitle("Horsepower") + labs(x="HP")
mtcars.mm <- data.frame(mm=c(mean(mtcars$hp),median(mtcars$hp)),stats=factor(c("mean","median")))
gg.lines <- geom_vline(mapping=aes(xintercept=mm,linetype=stats),show.legend=TRUE,data=mtcars.mm)

#

gg.static + geom_histogram(color="black",fill="white",breaks=seq(0,400,25),closed="right") + gg.lines + scale_linetype_manual(values=c(2,3)) + labs(linetype="")



################
# Section 24.2 #
################

## 24.2.1 ##

library("MASS")
surv <- na.omit(survey[,c("Sex","Wr.Hnd","Height")])

#

ggplot(surv,aes(x=Wr.Hnd,y=Height)) + geom_point(aes(col=Sex,shape=Sex)) + geom_smooth(method="loess")

#

plot(surv$Wr.Hnd,surv$Height,col=surv$Sex,pch=c(16,17)[surv$Sex])
smoother <- loess(Height~Wr.Hnd,data=surv)
handseq <- seq(min(surv$Wr.Hnd),max(surv$Wr.Hnd),length=100)
sm <- predict(smoother,newdata=data.frame(Wr.Hnd=handseq),se=TRUE)
lines(handseq,sm$fit)
polygon(x=c(handseq,rev(handseq)),y=c(sm$fit+2*sm$se,rev(sm$fit-2*sm$se)),col=adjustcolor("gray",alpha.f=0.5),border=NA)

#

ggplot(surv,aes(x=Wr.Hnd,y=Height,col=Sex,shape=Sex)) + geom_point() + geom_smooth(method="loess")


## 24.2.2 ##

ggplot(data=airquality,aes(x=Temp)) + geom_density()

#

air <- airquality
air$Month <- factor(air$Month,labels=c("May","June","July","August","September"))

#

ggplot(data=air,aes(x=Temp,fill=Month)) + geom_density(alpha=0.4) + ggtitle("Monthly temperature probability densities") + labs(x="Temp (F)",y="Kernel estimate")



################
# Section 24.3 #
################

## 24.3.1 ##

gg1 <- ggplot(air,aes(x=1:nrow(air),y=Temp)) + geom_line(aes(col=Month)) + geom_point(aes(col=Month,size=Wind)) + geom_smooth(method="loess",col="black") + labs(x="Time (days)",y="Temperature (F)")
gg2 <- ggplot(air,aes(x=Solar.R,fill=Month)) + geom_density(alpha=0.4) + labs(x=expression(paste("Solar radiation (",ring(A),")")),y="Kernel estimate")
gg3 <- ggplot(air,aes(x=Wind,y=Temp,color=Month)) + geom_point(aes(size=Ozone)) + geom_smooth(method="lm",level=0.9,fullrange=FALSE,alpha=0.2) + labs(x="Wind speed (MPH)",y="Temperature (F)")

#

library("gridExtra")
grid.arrange(gg1,gg2,gg3)


## 24.3.2 ##

ggp <- ggplot(data=air,aes(x=Temp,fill=Month)) + geom_density(alpha=0.4) + ggtitle("Monthly temperature probability densities") + labs(x="Temp (F)",y="Kernel estimate")

#

ggp + facet_wrap(~Month)
ggp + facet_wrap(~Month,scales="free")
ggp + facet_wrap(~Month,nrow=1) 

#

ggp + facet_grid(.~Month)

#

library("faraway")
diab <- na.omit(diabetes[,c("chol","weight","gender","frame","age","height","location")])
ggplot(diab,aes(x=age,y=chol)) + geom_point(aes(shape=location,size=weight,col=height)) + facet_grid(gender~frame) + geom_smooth(method="lm") + labs(y="cholesterol")



################
# Section 24.4 #
################

library("ggvis")
library("MASS")
surv <- na.omit(survey[,c("Sex","Wr.Hnd","Height","Smoke","Exer")])

#

surv %>% ggvis(x=~Height) %>% layer_histograms()

#

surv %>% ggvis(x=~Height) %>% layer_histograms(width=input_slider(1,15,label="Binwidth:"),fill:="gray")

#

surv %>% ggvis(x=~Wr.Hnd,y=~Height,size:=200,opacity:=0.3) %>% layer_points()

#

filler <- input_radiobuttons(c("Sex"="Sex","Smoking status"="Smoke","Exercise frequency"="Exer"),map=as.name,label="Color points by...")
sizer <- input_slider(10,300,label="Point size:")
opacityer <- input_slider(0.1,1,label="Opacity:")
surv %>% ggvis(x=~Wr.Hnd,y=~Height,fill=filler,size:=sizer,opacity:=opacityer) %>% layer_points() %>% add_axis("x",title="Handspan") %>% add_legend("fill",title="")

#

surv %>% ggvis(x=~Wr.Hnd,y=~Height,fill=~Sex) %>% group_by(Sex) %>% layer_smooths(span=input_slider(0.3,1,value=0.75,label="Smoothing span:"),se=TRUE) %>% layer_points() %>% add_axis("x",title="Handspan")




##################
### CHAPTER 25 ###
##################

################
# Section 25.1 #
################

## 25.1.1 ##

palette()

#

col2rgb(c("black","green3","pink"))

#

rgb(t(col2rgb(c("black","green3","pink"))),maxColorValue=255)

#

pcol <- function(cols){
	n <- length(cols)
	dev.new(width=7,height=7)
	par(mar=rep(1,4))
	plot(1:5,1:5,type="n",xaxt="n",yaxt="n",ann=FALSE)
	for(i in 1:n){
		pt <- locator(1)
		rgbval <- col2rgb(cols[i])
		points(pt,cex=4,pch=19,col=cols[i])
		text(pt$x+1,pt$y,family="mono",label=paste("\"",cols[i],"\"","\nR: ",rgbval[1],"  G: ",rgbval[2],"  B: ",rgbval[3],"\nhex: ",rgb(t(rgbval),maxColorValue=255),sep=""))
	}
}

#

mycols <- c("black","blue","royalblue2","pink","magenta","purple","violet","coral","lightgray","seagreen4","red","red2","yellow","lemonchiffon3")
pcol(mycols)


## 25.1.2 ##

N <- 600
rbow <- rainbow(N)
heat <- heat.colors(N)
terr <- terrain.colors(N)
topo <- topo.colors(N)
cm <- cm.colors(N)
gry1 <- gray.colors(N)
gry2 <- gray(level=seq(0,1,length=N))

#

dev.new(width=8,height=3)
par(mar=c(1,8,1,1))
plot(1,1,xlim=c(1,N),ylim=c(0.5,7.5),type="n",xaxt="n",yaxt="n",ann=FALSE)
points(rep(1:N,7),rep(7:1,each=N),pch=19,cex=3,col=c(rbow,heat,terr,topo,cm,gry1,gry2))
axis(2,at=7:1,labels=c("rainbow","heat.colors","terrain.colors","topo.colors","cm.colors","gray.colors","gray"),family="mono",las=1)


## 25.1.3 ##

puryel.colors <- colorRampPalette(colors=c("purple","yellow"))

#

blues <- colorRampPalette(colors=c("navyblue","lightblue"))

#

fours <- colorRampPalette(colors=c("black","hotpink","seagreen4","tomato"))
patriot.colors <- colorRampPalette(colors=c("red","white","blue"))

#

py <- puryel.colors(N)
bls <- blues(N)
frs <- fours(N)
pat <- patriot.colors(N)
dev.new(width=8,height=2)
par(mar=c(1,8,1,1))
plot(1,1,xlim=c(1,N),ylim=c(0.5,4.5),type="n",xaxt="n",yaxt="n",ann=FALSE)
points(rep(1:N,4),rep(4:1,each=N),pch=19,cex=3,col=c(py,bls,frs,pat))
axis(2,at=4:1,labels=c("peryel.colors","blues","fours","patriot.colors"),family="mono",las=1)


## 25.1.4 ##

library("MASS")
surv <- na.omit(survey[,c("Wr.Hnd","NW.Hnd","Height")])

#

NW.pal <- colorRampPalette(colors=c("red4","yellow2"))

#

k <- 5
ryc <- NW.pal(k)
ryc

#

NW.breaks <- seq(min(surv$NW.Hnd),max(surv$NW.Hnd),length=k+1)
NW.breaks

#

NW.fac <- cut(surv$NW.Hnd,breaks=NW.breaks,include.lowest=TRUE)
as.numeric(NW.fac)
NW.cols <- ryc[as.numeric(NW.fac)]
NW.cols

#

plot(surv$Wr.Hnd,surv$Height,col=NW.cols,pch=19)

#

normalize <- function(datavec){
  lo <- min(datavec,na.rm=TRUE)
  up <- max(datavec,na.rm=TRUE)
  datanorm <- (datavec-lo)/(up-lo)
  return(datanorm)
}

#

surv$NW.Hnd
normalize(surv$NW.Hnd)

#

NW.pal2 <- colorRamp(colors=c("red4","yellow2"))

#

ryc2 <- NW.pal2(normalize(surv$NW.Hnd))

#

NW.cols2 <- rgb(ryc2,maxColorValue=255)
NW.cols2

#

plot(surv$Wr.Hnd,surv$Height,col=NW.cols2,pch=19)


## 25.1.5 ##

library("shape")
plot(surv$Wr.Hnd,surv$Height,col=NW.cols2,pch=19,xlab="Writing handspan (cm)",ylab="Height (cm)")
colorlegend(NW.pal(200),zlim=range(surv$NW.Hnd),zval=seq(13,23,by=2),posx=c(0.3,0.33),posy=c(0.5,0.9),main="Nonwriting handspan")

#

par(mar=c(5,4,4,6))
plot(surv$Wr.Hnd,surv$Height,col=NW.cols2,pch=19,xlab="Writing handspan (cm)",ylab="Height (cm)")
colorlegend(NW.pal(200),zlim=range(surv$NW.Hnd),zval=13.5:22.5,digit=1,posx=c(0.89,0.91),main="Nonwriting\nhandspan")


## 25.1.6 ##

rgb(cbind(255,0,0),maxColorValue=255)
rgb(cbind(255,0,0),maxColorValue=255,alpha=0)
rgb(cbind(255,0,0),maxColorValue=255,alpha=102)
rgb(cbind(255,0,0),maxColorValue=255,alpha=255)

#

adjustcolor(rgb(cbind(255,0,0),maxColorValue=255),alpha.f=0.4)

#

keycols <- c("blue","red","yellow")
depth.pal <- colorRampPalette(keycols)
depth.pal2 <- colorRamp(keycols)

#

depth.cols <- rgb(depth.pal2(normalize(quakes$depth)),maxColorValue=255,alpha=0.6*255)

#

plot(quakes$mag,quakes$stations,pch=19,cex=2,col=depth.cols,xlab="Magnitude",ylab="No. of stations")

#

colorlegend(adjustcolor(depth.pal(20),alpha=0.6),zlim=range(quakes$depth),zval=seq(100,600,100),posx=c(0.3,0.32),posy=c(0.5,0.9),left=TRUE,main="Depth")


## 25.1.7 ##

# no R code



################
# Section 25.2 #
################

## 25.2.1 ##

pwid <- iris$Petal.Width
plen <- iris$Petal.Length
swid <- iris$Sepal.Width
slen <- iris$Sepal.Length

#

library("scatterplot3d") 
scatterplot3d(x=pwid,y=plen,z=swid)


## 25.2.2 ##

scatterplot3d(x=pwid,y=plen,z=swid,highlight.3d=TRUE,type="h",lty.hplot=2,lty.hide=3,xlab="Petal width",ylab="Petal length",zlab="Sepal width",main="Iris Flower Measurements")

#

keycols <- c("purple","yellow2","blue")
slen.pal <- colorRampPalette(keycols)
slen.pal2 <- colorRamp(keycols)
slen.cols <- rgb(slen.pal2(normalize(slen)),maxColorValue=255)

#

scatterplot3d(x=pwid,y=plen,z=swid,color=slen.cols,pch=c(19,17,15)[as.numeric(iris$Species)],type="h",lty.hplot=2,lty.hide=3,xlab="Petal width",ylab="Petal length",zlab="Sepal width",main="Iris Flower Measurements")

#

legend("bottomright",legend=levels(iris$Species),pch=c(19,17,15))

#

colorlegend(slen.pal(200),zlim=range(slen),zval=5:7,digit=1,posx=c(0.1,0.13),posy=c(0.7,0.9),left=TRUE,main="Sepal length")



################
# Section 25.3 #
################

## 25.3.1 ##

xcoords <- 1:6
xcoords
ycoords <- 1:4
ycoords

#

xycoords <- expand.grid(x=xcoords,y=ycoords)
xycoords

#

z <- letters[1:24]
cbind(xycoords,z)


## 25.3.2 ##

nx <- length(xcoords)
ny <- length(ycoords)
zmat <- matrix(z,nrow=nx,ncol=ny)
zmat


## 25.3.3 ##

dev.new(width=7,height=5.5)
par(mar=c(4,4,6,6))
plot(1,1,type="n",xlim=c(0.5,nx+0.5),ylim=c(0.5,ny+0.5),xaxs="i",yaxs="i",xlab="x",ylab="y")
grid(nx,ny,col="darkgray",lty=2,lwd=2)
arrows(c(rep(0.75,ny),xcoords),c(ycoords,rep(0.75,nx)),c(rep(nx+0.75,ny),xcoords),c(ycoords,rep(ny+0.75,nx)),length=0.1,xpd=TRUE)#,col="red")
text(rep(nx+1.4,ny),ycoords,labels=c("zmat[,1]","zmat[,2]","zmat[,3]","zmat[,4]"),family="mono",xpd=TRUE)
text(xcoords,rep(ny+1.4,nx),labels=c("zmat[1,]","zmat[2,]","zmat[3,]","zmat[4,]","zmat[5,]","zmat[6,]"),family="mono",srt=90,xpd=TRUE)
text(xycoords$x,xycoords$y,labels=zmat,cex=2,family="mono")



################
# Section 25.4 #
################

## 25.4.1 ##

dim(volcano)
contour(x=1:nrow(volcano),y=1:ncol(volcano),z=volcano,asp=1)

#

car.fit <- lm(mpg~hp*wt,data=mtcars)
car.fit

#

len <- 20
hp.seq <- seq(min(mtcars$hp),max(mtcars$hp),length=len)
wt.seq <- seq(min(mtcars$wt),max(mtcars$wt),length=len)
hp.wt <- expand.grid(hp=hp.seq,wt=wt.seq)
nrow(hp.wt)
hp.wt[1:5,]

#

car.pred <- predict(car.fit,newdata=hp.wt)

#

car.pred.mat <- matrix(car.pred,nrow=len,ncol=len)

#

contour(x=hp.seq,y=wt.seq,z=car.pred.mat,levels=32:8,lty=2,lwd=1.5,xaxs="i",yaxs="i",xlab="Horsepower",ylab="Weight",main="Mean MPG model")

#

library("MASS")
quak.dens <- kde2d(x=quakes$long,y=quakes$lat,n=100)

#

dim(quak.dens$z)

#

contour(quak.dens$x,quak.dens$y,quak.dens$z)

#

contour(quak.dens$x,quak.dens$y,quak.dens$z,nlevels=50,drawlabels=FALSE,xaxs="i",yaxs="i",xlab="Longitude",ylab="Latitude")
points(quakes$long,quakes$lat,cex=0.7)

#

plot(quakes$long,quakes$lat,cex=0.5,col="gray",xaxs="i",yaxs="i",xlab="Longitude",ylab="Latitude")

#

quak.levs <- c(0.001,0.005,0.01,0.015)

#

contour(quak.dens$x,quak.dens$y,quak.dens$z,add=TRUE,levels=quak.levs,drawlabels=FALSE,lty=4:1,lwd=2)

#

legend("bottomleft",legend=quak.levs,lty=4:1,lwd=2,title="Kernel estimate (contours)")


## 25.4.2 ##

filled.contour(x=hp.seq,y=wt.seq,z=car.pred.mat,color.palette=colorRampPalette(c("white","red4")),xlab="Horsepower",ylab="Weight",key.title=title(main="Mean MPG",cex.main=0.8))

#

filled.contour(x=quak.dens$x,y=quak.dens$y,z=quak.dens$z,color.palette=topo.colors,nlevels=30,xlab="Longitude",ylab="Latitude",key.title=title(main="KDE",cex.main=0.8),plot.axes={axis(1);axis(2);points(quakes$long,quakes$lat,cex=0.5,col=adjustcolor("black",alpha=0.3))})



################
# Section 25.5 #
################

## 25.5.1 ##

image(x=1:nrow(volcano),y=1:ncol(volcano),z=volcano,asp=1)

#

car.fit <- lm(mpg~hp*wt,data=mtcars)
len <- 20
hp.seq <- seq(min(mtcars$hp),max(mtcars$hp),length=len)
wt.seq <- seq(min(mtcars$wt),max(mtcars$wt),length=len)
hp.wt <- expand.grid(hp=hp.seq,wt=wt.seq)
car.pred.mat <- matrix(predict(car.fit,newdata=hp.wt),nrow=len,ncol=len)

#

library("shape")
blues <- colorRampPalette(c("cyan","navyblue"))
par(mar=c(5,4,4,5))
image(hp.seq,wt.seq,car.pred.mat,col=blues(10),xlab="Horsepower",ylab="Weight")
colorlegend(col=blues(10),zlim=range(car.pred.mat),zval=seq(10,30,5),main="Mean\nMPG")

#

car.fit <- lm(mpg~hp*wt,data=mtcars)
len <- 50
hp.seq <- seq(min(mtcars$hp),max(mtcars$hp),length=len)
wt.seq <- seq(min(mtcars$wt),max(mtcars$wt),length=len)
hp.wt <- expand.grid(hp=hp.seq,wt=wt.seq)
car.pred.mat <- matrix(predict(car.fit,newdata=hp.wt),nrow=len,ncol=len)

#

par(mar=c(5,4,4,5))
image(hp.seq,wt.seq,car.pred.mat,col=blues(100),xlab="Horsepower",ylab="Weight")
contour(hp.seq,wt.seq,car.pred.mat,add=TRUE,lty=2)
colorlegend(col=blues(100),zlim=range(car.pred.mat),zval=seq(10,30,5),main="Mean\nMPG")


## 25.5.2 ##

library("spatstat")
library("MASS")
plot(chorley$x,chorley$y,xlab="Eastings (km)",ylab="Northings (km)")

#

chor.dens <- kde2d(x=chorley$x,y=chorley$y,n=256)

#

rbow <- rainbow(200,start=0,end=5/6)

#

image(x=chor.dens$x,y=chor.dens$y,z=chor.dens$z,col=rbow)

#

plot(chorley$window,add=TRUE)

#

chor.WIN <- chorley$window
range(chorley$x)
WIN.xr <- chor.WIN$xrange
WIN.xr
range(chorley$y)
WIN.yr <- chor.WIN$yrange
WIN.yr

#

image(chor.dens$x,chor.dens$y,chor.dens$z,col=rbow,xlim=WIN.xr,ylim=WIN.yr)
plot(chor.WIN,add=TRUE)

#

chor.dens.WIN <- kde2d(chorley$x,chorley$y,n=256,lims=c(WIN.xr,WIN.yr))
image(chor.dens.WIN$x,chor.dens.WIN$y,chor.dens.WIN$z,col=rbow)
plot(chor.WIN,add=TRUE)

#

inside.owin(x=c(355,345),y=c(420,415),w=chor.WIN)

#

chor.xy <- expand.grid(chor.dens.WIN$x,chor.dens.WIN$y)
nrow(chor.xy)

#

chor.outside <- !inside.owin(x=chor.xy[,1],y=chor.xy[,2],w=chor.WIN)

#

chor.out.mat <- matrix(chor.outside,nrow=256,ncol=256)
chor.dens.WIN$z[chor.out.mat] <- NA

#

dev.new(width=7.5,height=7)
par(mar=c(5,4,4,7))
image(chor.dens.WIN$x,chor.dens.WIN$y,chor.dens.WIN$z,col=rbow,xlab="Eastings",ylab="Northings",bty="l",asp=1)
plot(chor.WIN,lwd=2,add=TRUE)
colorlegend(col=rbow,zlim=range(chor.dens.WIN$z,na.rm=TRUE),zval=seq(0,0.02,0.0025),main="KDE",digit=4,posx=c(0.85,0.87))



################
# Section 25.6 #
################

## 25.6.1 ##

car.fit <- lm(mpg~hp*wt,data=mtcars)
len <- 20
hp.seq <- seq(min(mtcars$hp),max(mtcars$hp),length=len)
wt.seq <- seq(min(mtcars$wt),max(mtcars$wt),length=len)
hp.wt <- expand.grid(hp=hp.seq,wt=wt.seq)
car.pred.mat <- matrix(predict(car.fit,newdata=hp.wt),nrow=len,ncol=len)

#

persp(x=hp.seq,y=wt.seq,z=car.pred.mat)

#

persp(x=hp.seq,y=wt.seq,z=car.pred.mat,theta=-30,phi=23,xlab="Horsepower",ylab="Weight",zlab="mean MPG")

#

persp(x=hp.seq,y=wt.seq,z=car.pred.mat,theta=40,phi=30,ticktype="detailed",xlab="Horsepower",ylab="Weight",zlab="mean MPG")

#

persp(x=hp.seq,y=wt.seq,z=car.pred.mat,theta=40,phi=30,shade=0.6,expand=0.8,border=NA,ticktype="detailed",xlab="Horsepower",ylab="Weight",zlab="mean MPG")


## 25.6.2 ##

library("spatstat")
library("MASS")
chor.WIN <- chorley$window
chor.dens.WIN <- kde2d(chorley$x,chorley$y,n=256,lims=c(chor.WIN$xrange,chor.WIN$yrange))
chor.xy <- expand.grid(chor.dens.WIN$x,chor.dens.WIN$y)
chor.out.mat <- matrix(!inside.owin(x=chor.xy[,1],y=chor.xy[,2],w=chor.WIN),256,256)
chor.dens.WIN$z[chor.out.mat] <- NA

#

zm <- chor.dens.WIN$z
nr <- nrow(zm)
nc <- ncol(zm)
zf <- (zm[-1,-1]+zm[-1,-nc]+zm[-nr,-1]+zm[-nr,-nc])/4
dim(zf)

#

rbow <- rainbow(200,start=0,end=5/6)
zf.breaks <- seq(min(zf,na.rm=TRUE),max(zf,na.rm=TRUE),length=201)
zf.colors <- cut(zf,breaks=zf.breaks,include.lowest=TRUE)

#

library("shape")
par(mar=c(0,1,0,7))
persp(chor.dens.WIN$x,chor.dens.WIN$y,chor.dens.WIN$z,border=NA,col=rbow[zf.colors],theta=-30,phi=30,scale=FALSE,expand=750,xlab="Eastings (km)",ylab="Northings (km)",zlab="Kernel estimate")
colorlegend(col=rbow,zlim=range(chor.dens.WIN$z,na.rm=TRUE),zval=seq(0,0.02,0.0025),main="KDE",digit=4,posx=c(0.85,0.87),posy=c(0.2,0.8))


## 25.6.3 ##

persprot <- function(skip=1,...){
  for(i in seq(90,20,by=-skip)){
    persp(phi=i,theta=0,...)
  }
  for(i in seq(0,360,by=skip)){
    persp(phi=20,theta=i,...)
  }
}

#

quak.dens <- kde2d(x=quakes$long,y=quakes$lat,n=50)

#

persprot(x=quak.dens$x,y=quak.dens$y,z=quak.dens$z,border="red3",shade=0.4,ticktype="detailed",xlab="Longitude",ylab="Latitude",zlab="Kernel estimate")




##################
### CHAPTER 26 ###
##################

################
# Section 26.1 #
################

library("rgl")


## 26.1.1 ##

pwid <- iris$Petal.Width
plen <- iris$Petal.Length
swid <- iris$Sepal.Width
slen <- iris$Sepal.Length

#

plot3d(x=pwid,y=plen,z=swid)


## 26.1.2 ##

plot3d(x=pwid,y=plen,z=swid,size=1.5,type="s",col=c(1,2,3)[as.numeric(iris$Species)])

#

legend3d("topright",col=1:3,legend=levels(iris$Species),pch=16,cex=2)

#

bg3d(color="white")


## 26.1.3 ##

slen.pal <- colorRampPalette(c("purple","yellow2","blue"))
cols <- slen.pal(50)
slen.cols <- cut(slen,breaks=seq(min(slen),max(slen),length=51),include.lowest=TRUE)

#

plot3d(x=pwid,y=plen,z=swid,type="s",size=1.5,col=cols[slen.cols],aspect=c(1,1.75,1),xlab="Petal width",ylab="Petal length",zlab="Sepal width")

#

xfromto <- rep(pwid,each=2)
yfromto <- rep(plen,each=2)
zfromto <- rep(min(swid),times=2*nrow(iris))
zfromto[seq(2,length(zfromto),2)] <- swid

#

segments3d(x=xfromto,y=yfromto,z=zfromto,col=rep(cols[slen.cols],each=2))

#

grid3d(side="z-")

#

library("shape")
bgplot3d({plot.new();colorlegend(slen.pal(50),zlim=range(slen),zval=seq(4.5,7.5,0.5),digit=1,posx=c(0.91,0.93),posy=c(0.1,0.9),main="Sepal length")})



################
# Section 26.2 #
################

library("rgl")


## 26.2.1 ##

car.fit <- lm(mpg~hp*wt,data=mtcars)
len <- 20
hp.seq <- seq(min(mtcars$hp),max(mtcars$hp),length=len)
wt.seq <- seq(min(mtcars$wt),max(mtcars$wt),length=len)
hp.wt <- expand.grid(hp=hp.seq,wt=wt.seq)

#

car.pred <- predict(car.fit,newdata=hp.wt,interval="prediction",level=0.99)

#

car.pred.mat <- matrix(car.pred[,1],nrow=len,ncol=len)
persp3d(x=hp.seq,y=wt.seq,z=car.pred.mat,col="green")


## 26.2.2 ##

persp3d(x=hp.seq,y=wt.seq,z=car.pred.mat,col="red",alpha=0.7,xlab="Horsepower",ylab="Weight",zlab="mean MPG")
points3d(mtcars$hp,mtcars$wt,mtcars$mpg,col="green3",size=10)

#

car.pred.lo <- matrix(car.pred[,2],nrow=len,ncol=len)
car.pred.up <- matrix(car.pred[,3],nrow=len,ncol=len)

#

persp3d(x=hp.seq,y=wt.seq,z=car.pred.up,col="cyan",add=TRUE,alpha=0.5)
persp3d(x=hp.seq,y=wt.seq,z=car.pred.lo,col="cyan",add=TRUE,alpha=0.5)

#

persp3d(x=hp.seq,y=wt.seq,z=car.pred.mat,col="red",alpha=0.7,xlab="Horsepower",ylab="Weight",zlab="mean MPG")
text3d(x=mtcars$hp,y=mtcars$wt,z=mtcars$mpg,texts=rownames(mtcars),cex=0.75)
persp3d(x=hp.seq,y=wt.seq,z=car.pred.up,col="cyan",add=TRUE,alpha=0.5)
persp3d(x=hp.seq,y=wt.seq,z=car.pred.lo,col="cyan",add=TRUE,alpha=0.5)

#

xfromto <- rep(mtcars$hp,each=2)
yfromto <- rep(mtcars$wt,each=2)
zfromto <- rep(car.fit$fitted.values,each=2)
zfromto[seq(2,2*nrow(mtcars),2)] <- mtcars$mpg

#

segments3d(x=xfromto,y=yfromto,z=zfromto)


## 26.2.3 ##

blues <- colorRampPalette(c("cyan","navyblue"))
blues200 <- blues(200)
zm <- car.pred.mat
zm.breaks <- seq(min(zm),max(zm),length=201)
zm.colors <- cut(zm,breaks=zm.breaks,include.lowest=TRUE)

#

persp3d(x=hp.seq,y=wt.seq,z=car.pred.mat,col=blues200[zm.colors],alpha=0.6,xlab="Horsepower",ylab="Weight",zlab="mean MPG")


## 26.2.4 ##

library("spatstat")
library("MASS")
chor.WIN <- chorley$window
chor.dens.WIN <- kde2d(chorley$x,chorley$y,n=256,lims=c(chor.WIN$xrange,chor.WIN$yrange))
chor.xy <- expand.grid(chor.dens.WIN$x,chor.dens.WIN$y)
chor.out.mat <- matrix(!inside.owin(x=chor.xy[,1],y=chor.xy[,2],w=chor.WIN),256,256)
chor.dens.WIN$z[chor.out.mat] <- NA

#

zm <- chor.dens.WIN$z
rbow <- rainbow(200,start=0,end=5/6)
zm.breaks <- seq(min(zm,na.rm=TRUE),max(zm,na.rm=TRUE),length=201)
zm.colors <- cut(zm,breaks=zm.breaks,include.lowest=TRUE)

#

xd <- chor.WIN$xrange[2]-chor.WIN$xrange[1]
xd
yd <- chor.WIN$yrange[2]-chor.WIN$yrange[1]
yd
xd/yd

#

persp3d(chor.dens.WIN$x,chor.dens.WIN$y,chor.dens.WIN$z,col=rbow[zm.colors],aspect=c(xd/yd,1,0.75),xlab="Eastings (km)",ylab="Northings (km)",zlab="Kernel estimate")

#

bgplot3d({plot.new();colorlegend(col=rbow,zlim=range(chor.dens.WIN$z,na.rm=TRUE),zval=seq(0,0.02,0.0025),main="KDE",digit=4,posx=c(0.87,0.9),posy=c(0.2,0.8))})



################
# Section 26.3 #
################

library("rgl")


## 26.3.1 ##

reds <- seq(0,255,25)
reds
greens <- seq(0,255,25)
blues <- seq(0,255,25)
full.rgb <- expand.grid(reds,greens,blues)
nrow(full.rgb)

#

plot3d(x=full.rgb[,1],y=full.rgb[,2],z=full.rgb[,3],col=rgb(full.rgb,maxColorValue=255),type="s",size=1.5,xlab="Red",ylab="Green",zlab="Blue")


## 26.3.2 ##

library("mvtnorm")
rand2d.norm <- rmvnorm(n=500,mean=c(0,0))
plot(rand2d.norm,xlab="x",ylab="y")

#

vals <- seq(-3,3,length=50)
xy <- expand.grid(vals,vals)
z <- matrix(dmvnorm(xy),50,50)

#

contour(vals,vals,z,xlab="x",ylab="y")

#

rand3d.norm <- rmvnorm(500,c(0,0,0))
plot3d(rand3d.norm,xlab="x",ylab="y",zlab="z")

#

res <- 11
ran <- c(-3,3)
rs <- seq(ran[1],ran[2],length=res)
plot3d(rand3d.norm,xlab="x",ylab="y",zlab="z",type="n",xlim=c(-3,3),ylim=c(-3,3),zlim=c(-3,3))
segments3d(x=c(rep(rs,each=2,times=res),rep(c(ran[1],ran[2]),times=res^2),rep(rs,each=2,times=res)),
           y=c(rep(c(ran[1],ran[2]),times=res^2),rep(rs,each=2,times=res),rep(rs,each=2*res)),
           z=c(rep(rs,each=2*res),rep(rs,each=2*res),rep(c(ran[1],ran[2]),res^2)),col=2,alpha=0.5)

#

xyz <- expand.grid(vals,vals,vals)
nrow(xyz)

#

w <- array(dmvnorm(xyz),c(50,50,50))

#

max3d.norm <- dmvnorm(c(0,0,0),mean=c(0,0,0))
max3d.norm

#

library("misc3d")
contour3d(x=vals,y=vals,z=vals,f=w,level=0.05*max3d.norm)

#

plot3d(rand3d.norm,xlab="x",ylab="y",zlab="z")
contour3d(x=vals,y=vals,z=vals,f=w,level=0.05*max3d.norm,add=TRUE,alpha=0.5)

#

plot3d(rand3d.norm,xlab="x",ylab="y",zlab="z")
contour3d(x=vals,y=vals,z=vals,f=w,level=c(0.05,0.2,0.6,0.95)*max3d.norm,color=c("pink","green","blue","red"),alpha=c(0.1,0.2,0.4,0.9),add=TRUE)


## 26.3.3 ##

quak <- quakes[,c("long","lat","depth")]
quak$depth <- -quak$depth

#

plot3d(x=quak$long,y=quak$lat,z=quak$depth,xlab="Longitude",ylab="Latitude",zlab="Depth")

#

library("ks")
quak.dens3d <- kde(quak,gridsize=c(64,64,64),compute.cont=TRUE)

#

dim(quak.dens3d$estimate)

#

x.latt <- quak.dens3d$eval.points[[1]]
y.latt <- quak.dens3d$eval.points[[2]]
z.latt <- quak.dens3d$eval.points[[3]]

#

quak.dens3d$cont[75]

#

plot3d(x=quak$long,y=quak$lat,z=quak$depth,xlab="Longitude",ylab="Latitude",zlab="Depth")
contour3d(x=x.latt,y=y.latt,z=z.latt,f=quak.dens3d$estimate,color="blue",level=quak.dens3d$cont[75],add=TRUE)

#

quak.dens3d$cont[50]

#

plot3d(x=quak$long,y=quak$lat,z=quak$depth,xlab="Longitude",ylab="Latitude",zlab="Depth")
contour3d(x=x.latt,y=y.latt,z=z.latt,f=quak.dens3d$estimate,color="green",level=quak.dens3d$cont[50],add=TRUE,alpha=0.5)

#

qlevels <- quak.dens3d$cont[c(80,60,40,20)]
qlevels

#

qcols <- c("yellow","orange","red","red4")
qalpha <- c(0.2,0.3,0.4,0.5)

#

plot3d(x=quak$long,y=quak$lat,z=quak$depth,xlab="Longitude",ylab="Latitude",zlab="Depth")
contour3d(x=x.latt,y=y.latt,z=z.latt,f=quak.dens3d$estimate,color=qcols,level=qlevels,add=TRUE,alpha=qalpha)



################
# Section 26.4 #
################

library("rgl")


## 26.4.1 ##

radius <- 3
a <- 1
b <- -4.4
angle <- 0:360*(pi/180)
x <- a+radius*cos(angle)
y <- b+radius*sin(angle)
plot(x,y,ann=FALSE)
abline(v=a)
abline(h=b)

#

r <- 3
h <- 10
zseq <- 0:h
theta <- 0:360*(pi/180)

#

ztheta <- expand.grid(zseq,theta)
nrow(ztheta)

#

x <- apply(ztheta,1,function(vec) r*cos(vec[2]))
y <- apply(ztheta,1,function(vec) r*sin(vec[2]))
z <- apply(ztheta,1,function(vec) vec[1])

#

xm <- matrix(x,length(zseq),length(theta))
ym <- matrix(y,length(zseq),length(theta))
zm <- matrix(z,length(zseq),length(theta))

#

xm <- outer(zseq,theta,function(z,t) r*cos(t))
ym <- outer(zseq,theta,function(z,t) r*sin(t))
zm <- outer(zseq,theta,function(z,t) z)

#

persp3d(x=xm,y=ym,z=zm,col="red")
points3d(x=xm,y=ym,z=zm)

#

xm <- outer(zseq,theta,function(z,t) (h-z)/h*r*cos(t))
ym <- outer(zseq,theta,function(z,t) (h-z)/h*r*sin(t))
zm <- outer(zseq,theta,function(z,t) z)
persp3d(x=xm,y=ym,z=zm,col="green")


## 26.4.2 ##

res <- 200
vseq <- seq(-1,1,length=res)
theta <- seq(0,2*pi,length=res)

#

xm <- outer(vseq,theta,function(v,t) (1+v/2*cos(t/2))*cos(t))
ym <- outer(vseq,theta,function(v,t) (1+v/2*cos(t/2))*sin(t))
zm <- outer(vseq,theta,function(v,t) v/2*sin(t/2))

#

plot3d(x=xm,y=ym,z=zm)

#

persp3d(x=xm,y=ym,z=zm,col="orange",axes=FALSE,ann=F,xlab="",ylab="",zlab="")

#

patriot.colors <- colorRampPalette(c("red4","red","white","blue","white","red","red4"))

#

patcols <- patriot.colors(2*res-1)
stripcols <- rep(NA,res^2)
for(i in 0:(res-1)){
	stripcols[1:res+res*i] <- patcols[1:res+i]
}

#

persp3d(x=xm,y=ym,z=zm,col=stripcols,aspect=c(2,2.5,1.5),axes=FALSE,xlab="",ylab="",zlab="")

#

alpha <- 1
beta <- 2
xm <- outer(theta,theta,function(t1,t2) (beta+alpha*cos(t2))*cos(t1))
ym <- outer(theta,theta,function(t1,t2) (beta+alpha*cos(t2))*sin(t1))
zm <- outer(theta,theta,function(t1,t2) alpha*sin(t2))

#

plot3d(x=xm,y=ym,z=zm)

#

persp3d(x=xm,y=ym,z=zm,col="seagreen4",axes=FALSE,xlab="",ylab="",zlab="")

#

donutcols <- rep("tan",res^2)

#

donutcols[as.vector(zm)>0] <- "pink"

#

sample(1:10,4)

#

sprinkles <- c("blue","green","red","violet","yellow")
donutcols[sample(which(as.vector(zm)>0),300)] <- sprinkles

#

persp3d(xm,ym,zm,col=donutcols,aspect=c(1,1,0.4),axes=FALSE,xlab="",ylab="",zlab="")

