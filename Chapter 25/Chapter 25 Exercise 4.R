#  Book --------------------

dev.new()
image(x=1:nrow(volcano),
      y=1:ncol(volcano),
      z=volcano,
      asp=1)

filled.contour(x=1:nrow(volcano),
               y=1:ncol(volcano),
               z=volcano)

install.packages("shape")
library("shape")

car.fit<-lm(mpg~hp*wt,data=mtcars)
len<-20
hp.seq<-seq(min(mtcars$hp),
            max(mtcars$hp),
            length=len)
wt.seq<-seq(min(mtcars$wt),
            max(mtcars$wt),
            length=len)
hp.wt<-expand.grid(hp=hp.seq,
                   wt=wt.seq)
car.pred.mat<-matrix(predict(car.fit,
                             newdata=hp.wt),
                     nrow=len,
                     ncol=len)
blues<-colorRampPalette(c("cyan","navyblue"))
dev.new()
par(mar=c(5,4,4,5))                    
image(hp.seq,
      wt.seq,
      car.pred.mat,
      col=blues(10),
      xlab="Horsepower",
      ylab="Weight")
colorlegend(col=blues(10),
            zlim=range(car.pred.mat),
            zval=seq(10,30,5),
            main="Mean\nMPG",
            main.cex = 0.5,
            cex=0.5)


car.fit<-lm(mpg~hp*wt,data=mtcars)
len<-50
hp.seq<-seq(min(mtcars$hp),max(mtcars$hp),length.out = len)
wt.seq<-seq(min(mtcars$wt),max(mtcars$wt),length.out = len)
hp.wt<-expand.grid(hp=hp.seq,wt=wt.seq)
car.pred.mat<-matrix(predict(car.fit,newdata = hp.wt),
                     nrow=len,
                     ncol=len)


dev.new()
par(mar=c(5,4,4,8))
image(hp.seq,
      wt.seq,
      car.pred.mat,
      col=blues(100),
      xlab="Horsepower",
      ylab="Weight")
contour(x=hp.seq,wt.seq,car.pred.mat,
        add=T,lty=2)
colorlegend(col=blues(100),
            zlim=range(car.pred.mat),
            zval=seq(10,30,5),
            main="Mean\nMPG")

install.packages("spatstat")
library("spatstat")
?chorley
plot(chorley$x,chorley$y,xlab="Eastings (Km)",ylab="Northings (Km)")
library("MASS")

chor.dens<-kde2d(x=chorley$x,
                 y=chorley$y,
                 n=256)
rbow=rainbow(200,start=0,end=5/6)
image(x=chor.dens$x,
      y=chor.dens$y,
      z=chor.dens$z)
plot(chorley$window,add=T)

chor.WIN<-chorley$window
range(chorley$x)
WIN.xr<-chor.WIN$xrange
WIN.xr
range(chorley$y)
WIN.yr<-chor.WIN$yrange


image(chor.dens$x,chor.dens$y,
      chor.dens$z,
      col=rbow,
      xlim=WIN.xr,
      ylim=WIN.yr)
plot(chor.WIN,add=T)

chor.dens.WIN<-kde2d(chorley$x,
                     chorley$y,
                     n=256,
                     lims=c(WIN.xr,
                            WIN.yr))
image(chor.dens.win$x,
      chor.dens.win$y,
      chor.dens.win$z,
      col = rbow)
plot(chor.WIN,add = T)

inside.owin(x=c(355,345),y=c(420,415),w=chor.WIN)

chor.xy<-expand.grid(chor.dens.WIN$x,
                     chor.dens.WIN$y)
nrow(chor.xy)

chor.outside<-!inside.owin(x=chor.xy[,1],y=chor.xy[,2],w=chor.WIN)

chor.out.mat<-matrix(chor.outside,
                     nrow=256,
                     ncol=256)

chor.dens.WIN$z[chor.out.mat]<-NA

dev.new(width=7.5,height=7)
par(mar=c(5,4,4,7))
image(chor.dens.WIN$x,
      chor.dens.WIN$y,
      chor.dens.WIN$z,
      col=rbow,
      xlab="Eastings",
      ylab="Northings",
      bty="l",
      asp=1)
plot(chorley$window,lwd=4,add=T)
colorlegend(col=rbow,
            zlim=range(chor.dens.WIN$z,
                       na.rm=T),
            zval=seq(0,0.02,0.0025),
            main="KDE",
            digit=4,
            posx=c(0.85,0.87),
            xpd=NA)

#  Exercises --------------------

?airquality
airqual<-na.omit(airquality[,c(4,3,1)])
rownames(airqual)<-NULL #To remove rownames

#a
air.fit<-lm(Temp~Wind*Ozone,data=airqual)
summary(air.fit)

#b
Wind.vec<-seq(min(airqual$Wind),max(airqual$Wind),length.out=50)
Ozone.vec<-seq(min(airqual$Ozone),max(airqual$Ozone),length.out = 50)

Wind.Ozone<-expand.grid(Wind=Wind.vec,Ozone=Ozone.vec)
zvalues<-predict(air.fit,newdata = Wind.Ozone)
zmat<-matrix(zvalues,nrow=length(Wind.vec),ncol=length(Ozone.vec))

#c
#
dev.new()
par(mar=c(5,4,4,6))
#
windozonecol<-topo.colors(20)
image(x=Wind.vec,y=Ozone.vec,z=zmat,
      col=windozonecol,
      xlab="Wind",ylab="Ozone")
      #
normalize<-function(datavec){
      lo<-min(datavec,na.rm=TRUE)
      up<-max(datavec,na.rm=TRUE)
      datanorm<-(datavec-lo)/(up-lo)
      return(datanorm)
}
#
temp.norm<-normalize(airqual$Temp)
temp.col<-gray(temp.norm)
points(airqual$Wind,airqual$Ozone,
       col=temp.col,
       pch=16)
#
colorlegend(windozonecol,
            zlim=c(min(zmat),
                   max(zmat)),
            zval = seq(min(zmat),max(zmat),length.out = 5),
            zlevels = 10,
            posx=c(0.95,0.96),
            posy=c(0.08,0.9),
            main="Temp\nPrediction")

colorlegend(gray(seq(0,1,length.out=116)),
            zlim=c(min(airqual$Temp),
                  max(airqual$Temp)),
            zval=seq(min(airqual$Temp),
                        max(airqual$Temp),
                        length.out = 8),
            posx=c(0.8,0.81),
            posy=c(0.3,0.8),
            main="Temp\nobserved")

#d
library("MASS")
library("spatstat")
fire<-split(clmfires)$intentional
firewin<-clmfires$window

int.fire.den<-kde2d(x=fire$x,
                    y=fire$y,
                    n=c(256,256),
                    lims=c(firewin$xrange,firewin$y))
#e
xy.grid<-expand.grid(x=int.fire.den$x,
                     y=int.fire.den$y)
outside.vec<-!inside.owin(xy.grid,w=fire$window)
outside.mat<-matrix(outside.vec,nrow=256,ncol=256)
int.fire.den$z[outside.mat]<-NA
#f
#
dev.new()
par(mar=c(3,3,3,7))
#
image(x=int.fire.den$x,
      y=int.fire.den$y,
      z=int.fire.den$z,
      col=heat.colors(50),
      bty="l",
      xaxs="i",yaxs="i",
      asp=1,
      xlab="",ylab="",
      xlim=)
#
plot(x=firewin,lwd=2,add=T)

#
colorlegend(col=heat.colors(50),
            zlim=c(min(na.omit(as.vector(int.fire.den$z))),
                   max(na.omit(as.vector(int.fire.den$z)))),
            zlevels=5,
            zval=seq(5e-6,35e-6,by=5e-6),
            digit = 6,
            posx=c(0.9,0.92),posy=c(0.1,0.9))