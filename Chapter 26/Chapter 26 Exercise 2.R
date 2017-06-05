#----- TEXT -------------------
library("rgl")
car.fit<-lm(mpg~hp*wt,data=mtcars)
len<-20
hp.seq<-seq(min(mtcars$hp),max(mtcars$hp),length=len)
wt.seq<-seq(min(mtcars$wt),max(mtcars$wt),length=len)
hp.wt<-expand.grid(hp=hp.seq,wt=wt.seq)

car.pred<-predict(car.fit,newdata=hp.wt,interval="prediction",level=0.99)
car.pred.mat<-matrix(car.pred[,1],nrow=len,ncol=len)
persp3d(x=hp.seq,
        y=wt.seq,
        z=car.pred.mat,
        col="green")
points3d(x=mtcars$hp,y=mtcars$wt,z=mtcars$mpg)
car.pred.mat.lo<-matrix(car.pred[,2],nrow=len,ncol=len)
car.pred.mat.up<-matrix(car.pred[,3],nrow=len,ncol=len)
text3d(x=mtcars$hp,
       y=mtcars$wt,z=mtcars$mpg,texts=rownames(mtcars),cex=0.75)
persp3d(x=hp.seq,
        y=wt.seq,
        z=car.pred.mat.lo,
        col="cyan",
        alpha=0.5,
        add = T)
persp3d(x=hp.seq,
        y=wt.seq,
        z=car.pred.mat.up,
        col="cyan",
        alpha=0.5,
        add = T)


xfromto<-rep(mtcars$hp,each=2)
yfromto<-rep(mtcars$wt,each=2)
zfromto<-rep(car.fit$fitted.values,each=2)
zfromto[seq(2,2*nrow(mtcars),2)] <-mtcars$mpg
segments3d(x=xfromto,
           y=yfromto,z=zfromto,add=T)

blues<-colorRampPalette(c("cyan","navyblue","red"))
blues200<-blues(200)
zm<-car.pred.mat
zm.breaks<-seq(min(zm),max(zm),length=201)
zm.colors<-cut(zm,breaks=zm.breaks,include.lowest = T)

persp3d(x=hp.seq,y=wt.seq,z=car.pred.mat,col=blues200[zm.colors],
        alpha=0.6,xlab="Horsepower",ylab="Weight",zlab="mean MPG")


library("spatstat")
library("MASS")
chor.WIN <- chorley$window
chor.dens.WIN <- kde2d(
      chorley$x,
      chorley$y,
      n = 256,
      lims = c(chor.WIN$xrange, chor.WIN$yrange)
)
chor.xy <- expand.grid(chor.dens.WIN$x, chor.dens.WIN$y)
chor.out.mat <- matrix(!inside.owin(x = chor.xy$Var1, y = chor.xy$Var2,
                                    w = chor.WIN))
chor.dens.WIN$z[chor.out.mat] <- NA

zm<-chor.dens.WIN$z
rbow<-rainbow(200,start=0,end=5/6)
zm.breaks<-seq(min(zm,na.rm = TRUE),
              max(zm,na.rm=TRUE),
              length=201)
zm.colors<-cut(zm,breaks=zm.breaks,include.lowest=T)
persp3d(chor.dens.WIN$x,chor.dens.WIN$y,chor.dens.WIN$z,
        col=rbow[zm.colors],aspect=c(1.075772,1,0.75),
        xlab="Eastings (KM)",ylab="Northlings (KM)",
        zlab="Kernel estimate")
library("shape")

bgplot3d({plot.new();
      colorlegend(col=rbow,zlim=range(chor.dens.WIN$z,na.rm=T),
                  zval=seq(0,0.02,0.0025),main="KDE",digit=4,
                  posx=c(0.87,0.9),posy=c(0.2,0.8))})

# ------ EXERCISES ----
airqual<-airquality[,c(4,3,1,5)]
airqual<-na.omit(airqual)

#a
air.fit<-lm(Temp~Wind*Ozone,data=airqual)
wind.seq<-seq(min(airqual$Wind),
              max(airqual$Wind),
              length.out = 200)
ozone.seq<-seq(min(airqual$Ozone),
               max(airqual$Ozone),
               length.out = 200)
xy.grid<-expand.grid(Wind=wind.seq,
                   Ozone=ozone.seq)
temp.pred<-predict(air.fit,
                   newdata=xy.grid,
                   interval="prediction")
temp.pred.mat<-matrix(temp.pred,nrow=200,ncol=200)
temp.pred.mat.lo<-matrix(temp.pred[,2],nrow=200,ncol=200)
temp.pred.mat.up<-matrix(temp.pred[,3],nrow=200,ncol=200)

persp3d(x=wind.seq,
        y=ozone.seq,
        z=temp.pred.mat,
        col="green")
persp3d(x=wind.seq,
        y=ozone.seq,
        z=temp.pred.mat.lo,
        col="yellow",
        alpha=0.5,
        add=T)
persp3d(x=wind.seq,
        y=ozone.seq,
        z=temp.pred.mat.up,
        col="yellow",
        alpha=0.5,
        add=T)

#b
tcol<-topo.colors(120,alpha=0.8)
zm.breaks<-seq(min(temp.pred.mat),max(temp.pred.mat),length.out = 121)
zm.colors<-cut(temp.pred.mat,breaks=zm.breaks,include.lowest = T)
persp3d(x=wind.seq,
        y=ozone.seq,
        z=temp.pred.mat,
        col=tcol[zm.colors],
        xlab="Wind",
        ylab="Ozone",
        zlab="Temperature")

#c
#i
mont.col<-colorRampPalette(c("red4","pink"))(5)
points3d(x=airqual$Wind,
         y=airqual$Ozone,
         z=airqual$Temp,
         col=mont.col[as.factor(airqual$Month)],
         size=5)
#ii
xfromto<-rep(airqual$Wind,each=2)
yfromto<-rep(airqual$Ozone,each=2)
zfromto<-rep(air.fit$fitted.values,each=2)
zfromto[seq(2,2*nrow(airqual),2)]<-airqual$Temp
segments3d(x=xfromto,
           y=yfromto,
           z=zfromto,
           col=mont.col[rep(as.factor(airqual$Month),each=2)])

#iii
persp3d(x=wind.seq,
        y=ozone.seq,
        z=temp.pred.mat.lo,
        col="gray",
        alpha=.5,
        add=T)
persp3d(x=wind.seq,
        y=ozone.seq,
        z=temp.pred.mat.up,
        col="gray",
        alpha=.5,
        add=T)
bgplot3d({plot.new();
      legend("topright",
             legend=c("May","june","july","august","september"),
             col=mont.col,
             pch=19,
             cex=2)})

#d
library("spatstat")
library("MASS")
library("rgl")
?clmfires
fire<-split(clmfires)$intentional
firewin<-fire$window

fire.dens.win<-kde2d(fire$x,
                fire$y,
                n=200,
                lims=c(fire$window$xrange,
                       fire$window$yrange))
fire.xy<-expand.grid(fire.dens.win$x,
                     fire.dens.win$y)
fire.out.mat<-matrix(!inside.owin(x=fire.xy[,1],
                                  y=fire.xy[,2],
                                  w=firewin),
                     200,
                     200)
fire.dens.win$z[fire.out.mat]<-NA
firecols<-heat.colors(100,
                      alpha=0.5)
zm.breaks<-seq(min(fire.dens.win$z,na.rm = T),
                max(fire.dens.win$z,na.rm=T),
                length.out = 101)
zm.cols<-cut(fire.dens.win$z,
             breaks = zm.breaks,
             include.lowest = T)
xd<-range(fire.dens.win$x)[2]-range(fire.dens.win$x)[1]
yd<-range(fire.dens.win$y)[2]-range(fire.dens.win$y)[1]

persp3d(fire.dens.win$x,fire.dens.win$y,fire.dens.win$z,
        col=firecols[zm.cols],
        aspect=c(xd/yd,1,0.6),
        xlab="X",ylab="Y",zlab="",
        alpha=0.7)

#e
#i
points3d(x=fire$x,y=fire$y,z=min(fire.dens.win$z,na.rm = T))
#ii
firepoly<-vertices(firewin)
fwx<-firepoly$x
fwy<-firepoly$y
lines3d(x=fwx,
        y=fwy,
        z=min(fire.dens.win$z,na.rm=T),
        lwd=2)
