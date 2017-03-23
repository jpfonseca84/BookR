#a

mpg.fit<-lm(mpg~cyl,data=mtcars)
dev.new()
dev.list()
par(mfrow=c(2,1))
boxplot(mtcars$mpg~mtcars$cyl,ylab="MPG",xlab="Cylinders",main="Boxplot",axis=F)
box("figure")
plot(mtcars$mpg~mtcars$cyl,ylab="MPG",xlab="Cylinders",main="Scatterplot")
Axis(side=2,at=c(2,4,6))
box("figure")
abline(mpg.fit)

#b
dev.mat1<-matrix(c(2,1,1,3),4,1)
dev.mat2<-matrix(c(1,1,1,1,2,4,3,5),2,4)
dev.mat3<-matrix(c(2,3,3,1,2,3,3,1,2,4,5,1),4,3)
dev.new(height=4,width=4)
layout(mat=dev.mat2)
plot(1,1)
plot(2,2)
plot(3,3)
plot(4,4)
plot(5,5)

#c
dev.new(height=4.5,width=9)
dev.matc<-matrix(c(1,1,1,1,2,3,4,4),2,4)
layout(mat=dev.matc)
par(mar=c(4,4,2,1))
plot(quakes$long,quakes$lat,cex=0.02*quakes$stations,xlab="Longitude",ylab="Latitude")
box("figure",lty=3,col="gray")
plot(quakes$stations~quakes$mag,xlab="Magnitude",ylab="Stations")
box("figure",lty=3,col="gray")
plot(quakes$stations~quakes$depth,xlab="Magnitude",ylab="Stations")
box("figure",lty=3,col="gray")
hist(quakes$stations)
box("figure",lty=3,col="gray")

#d
interactive.arrow<-function(...,label=NA){
      arrowpoints<-locator(n=2)
      arrows(x0=arrowpoints$x[1],
             y0=arrowpoints$y[1],
             x1=arrowpoints$x[2],
             y1=arrowpoints$y[2],
             ...)
      if(!is.na(label)){
            labelposition<-locator(n=1)
            text(x=labelposition$x[1],
                 y=labelposition$y[1],
                 labels=label,
                 ...)
      }
}

boxplot(quakes$mag)
interactive.arrow(xpd=NA,label="outliers")
interactive.arrow(xpd=F,label="maximum")
interactive.arrow(xpd=F,label="3rd quartile")
interactive.arrow(xpd=NA,label="median")
interactive.arrow(xpd=NA,label="1st quartile")
interactive.arrow(xpd=NA,label="minimum")
