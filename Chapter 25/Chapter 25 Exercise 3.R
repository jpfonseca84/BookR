# Text ----
expand.grid((1:5),5:9)

dim(volcano)
?volcano
contour(x=1:nrow(volcano),y=1:ncol(volcano),z=volcano,asp=1)
contour(z=volcano,asp=1)

car.fit<-lm(mpg~hp*wt,data=mtcars)
car.fit
len<-20
hp.seq<-seq(min(mtcars$hp),max(mtcars$hp),length=len)
wt.seq<-seq(min(mtcars$wt),max(mtcars$wt),length=len)
hp.wt<-expand.grid(hp=hp.seq,wt=wt.seq)
nrow(hp.wt)
hp.wt[1:5,]
car.pred<-predict(car.fit,newdata=hp.wt)
car.pred.mat<-matrix(car.pred,nrow=len,ncol=len)
contour(x=hp.seq,y=wt.seq,
        z=car.pred.mat,
        levels=32:8,
        lty=2,lwd=1.5,
        xaxs="i",
        yaxs="i",
        xlab="Horsepower",
        ylab="Weight",
        main="Mean MPG model")

library("MASS")
quak.dens<-kde2d(x=quakes$long,y=quakes$lat,n=100)
contour(quak.dens$x,quak.dens$y,quak.dens$z,nlevels=50,drawlabels = FALSE,
        xaxs="i",yaxs="i",xlab="Longitude",ylab="Latitude")
points(quakes$long,quakes$lat,cex=0.7)

plot(quakes$long,quakes$lat,cex=0.5,col="gray",xaxs="i",yaxs="i",
     xlab="longittude",ylab="Latitude")
quak.levs<-c(0.001,0.005,0.01,0.015)
contour(quak.dens$x,quak.dens$y,quak.dens$z,levels=quak.levs,
        add=TRUE,xaxs="i",yaxs="i",drawlabels = F,
        lty=4:1,lwd=2)
legend("bottomleft",lty=4:1,lwd=2,
       legend = as.character(quak.levs),
       title="Kernel estimate (contours)")


names(mtcars)
filled.contour(x=hp.seq,
               y=wt.seq,
               z=car.pred.mat,
               color.palette=colorRampPalette(c("white","red4")),
               xlab="Horsepower",ylab="Weight",
               key.title=title(main="Mean MPG",cex.main=0.8))
               
filled.contour(
      x = quak.dens$x,
      y = quak.dens$y,
      z = quak.dens$z,
      color.palette = topo.colors,
      nlevels = 30,
      xlab = "Longitude",
      ylab = "Latitude",
      key.title = title(main = "KDE", cex.main = 0.8),
      plot.axes = {
            axis(1);
            axis(2);
            points(quakes$long,
                   quakes$lat,
                   cex = 0.5,
                   col = adjustcolor("black", alpha = 0.3))
      }
)

#Exercises ----
library("boot")
nuclear

#a
#i
nuc.fit.1<-lm(cost~date+cap,data=nuclear)
#i
nuc.fit.2<-lm(cost~date*cap,data=nuclear)
summary(nuc.fit.2)
#b
cap.vec <- seq(min(nuclear$cap), max(nuclear$cap), length.out = 50)
date.vec <- seq(min(nuclear$date), max(nuclear$date), length.out = 50)

grid <- expand.grid(cap = cap.vec, date = date.vec)
z.vec.1 <- predict(nuc.fit.1, newdata = grid)
zmat.1 <- matrix(z.vec.1, x=50, y=50)
z.vec.2 <- predict(nuc.fit.2, newdata = grid)
zmat.2 <- matrix(z.vec.2, length(cap.vec), length(date.vec))

#c
par(mfrow=c(2,1))
contour(x=cap.vec,
        y=date.vec,
        z=zmat.1,xaxs="i",yaxs="i",xlab="Capacity",ylab="Date")
contour(x=cap.vec,
        y=date.vec,
        z=zmat.2,xaxs="i",yaxs="i",xlab="Capacity",ylab="Date")
#Both charts appear to be very similar, therefore corroborating the 
#lack of statistical evidence of predictor relevance.

#d
filled.contour(x=cap.vec,
               y=date.vec,
               z=zmat.1,
               xaxs="i",
               yaxs="i",
               xlab="Capacity",
               ylab="Date",
               color.palette = colorRampPalette(c("blue","red","yellow")),
               plot.axes = {axis(1);
                     axis(2);
                     contour(x=cap.vec,
                             y=date.vec,
                             z=zmat.2,
                             xaxs="i",
                             yaxs="i",
                             add=T,
                             cex=2,
                             lty=2)})
legend(locator(1),
       legend = "Color Fill = main effects only\nLines = interactive effects",
       xpd=NA,
       adj=0.5,
       bty="n",
       cex=0.7)

#e
?faithful
plot(faithful$eruptions~faithful$waiting,
     xlab="Waiting Time",
     ylab="Eruption Time",
     pch=16)

#f
library("MASS")
erup.vec<-seq(min(faithful$eruptions),
              max(faithful$eruptions),
              length.out = 100)
wait.vec<-seq(min(faithful$eruptions),
              max(faithful$eruptions),
              length.out = 100)
faith.grid<-kde2d(x=faithful$waiting,
            y=faithful$eruptions,
            n=50)
contour(faith.grid$x,
        faith.grid$y,
        faith.grid$z,
        xaxs="i",
        yaxs="i",
        nlevels=7)

#g
filled.contour(x=faith.grid$x,
               y=faith.grid$y,
               z=faith.grid$z,
               xaxs="i",
               xlab="Waiting Time",
               yaxs="i",
               ylab="Eruptions Time",
               main="Eruption Times v Waiting Time",
               color.palette = colorRampPalette(c("darkblue","hotpink")),
               plot.axes = {axis(1);
                     axis(2);
                     points(faithful$waiting,
                            faithful$eruptions,
                            col="gray",
                            cex=0.5,
                            pch=16)},
               key.title = title("Density",cex.main=0.8))

#h
dev.new()+
plot(faithful$eruptions~faithful$waiting,
     pch=2,
     cex=0.75,
     col="gray",
     xaxs="i",
     yaxs="i",
     xlab="Waiting Time",
     ylab="Eruption Time",
     main="Eruption Time v Waiting Time\n Scatterplot")+
contour(x=faith.grid$x,
        y=faith.grid$y,
        z=faith.grid$z,
        levels = seq(0.002,0.014,by=0.004),
        drawlabels = F,
        col="darkred",
        lwd=1:4,
        add=T)+
legend("topleft",
       title="Kernel Density",
       lwd=1:4,
       legend=seq(0.002,0.014,by=0.004),
       col="darkred")
