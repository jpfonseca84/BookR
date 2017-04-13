library("shape")
normalize<-function(datavec){
      lo<-min(datavec,na.rm=TRUE)
      up<-max(datavec,na.rm=TRUE)
      datanorm<-(datavec-lo)/(up-lo)
      return(datanorm)
}

pwid<-iris$Petal.Width
plen<-iris$Petal.Length
swid<-iris$Sepal.Width
slen<-iris$Sepal.Length
scatterplot3d(pwid,plen,swid)

scatterplot3d(pwid,plen,swid,highlight.3d = T,type = "h",
              lty.hplot =6,lty.hide = 1,xlab="Petal Width",
              ylab="Petal length",zlab="Sepal width",
              main="Iris Flower Measurements")

keycols<-c("purple","yellow","blue")
slen.pal<-colorRampPalette(keycols)
slen.pal2<-colorRamp(keycols)
slen.cols<-rgb(slen.pal2(normalize(slen)),maxColorValue = 255)

scatterplot3d(x=pwid,
              y=plen,
              z=swid,
              color=slen.cols,
              pch=c(19,17,15)[as.numeric(iris$Species)],
              type="h",
              lty.hplot=2,
              lty.hide=3,
              xlab="Petal Width",
              ylab="Petal Length",
              zlab="Sepal Width",
              main="Iris Flower Measurements")
legend("bottomright",legend = levels(iris$Species),pch=c(19,17,15))
colorlegend(slen.pal(200),zlim=range(slen),zval=5:7,digit=1,
            posx=c(0.1,0.13),posy=c(0.7,0.9),left=TRUE,
            main="Sepal Length")

#---------------Exercise ---------------------------------

#a
library("faraway")
diabetes
scatterplot3d(x=diabetes$hip,    xlab = "Hip",
              y=diabetes$waist,  ylab= "Waist",
              z=diabetes$weight, zlab="Weight",
              highlight.3d = T,
              pch=c(15,16)[as.numeric(diabetes$gender)])
legend("topleft",
       legend = c("Male","Female"),
       pch=c(15,16))

#b
airquality
airqual<-na.omit(airquality)
aircol<-topo.colors(50)
cutozone<-cut(airqual$Ozone,
              breaks = 50)
dev.new(mar=c(5,5,5,6),xpd=NA)
scatterplot3d(x=airqual$Wind,    xlab= "Wind",
              y=airqual$Solar.R, ylab="Solar",
              z=airqual$Temp,    zlab="Temperature",
              type = "h",
              lty.hplot = 3,
              pch=(1:5)[as.numeric(as.factor(airqual$Month))],
              color = aircol[as.numeric(cutozone)],
              main= "Wind by Solar R, by Temperature"
              )
legend("topleft",
       legend=levels(as.factor(airqual$Month)),
       pch=(1:5),
       title="Month")
colorlegend(col=aircol,
            zlevels=5,
            zlim=c(1,170),
            zval=seq(0,170,20),
            posx = c(0.92,0.93),
            posy = c(0.5,0.94),
                  main="Ozone level")
              
              
              