#----Text --------------------
car.fit<-lm(mpg~hp*wt,data=mtcars)
len<-20
hp.seq<-seq(min(mtcars$hp),max(mtcars$hp),length=len)
wt.seq<-seq(min(mtcars$wt),max(mtcars$wt),length=len)
hp.wt<-expand.grid(hp=hp.seq,wt=wt.seq)
car.pred.mat<-matrix(predict(car.fit,newdata=hp.wt),nrow=len,ncol=len)
persp(x=hp.seq,y=wt.seq,z=car.pred.mat,theta=120,
      phi=50)

persp(x=hp.seq,y=wt.seq,z=car.pred.mat,theta=0,phi=23,
      xlab="Horsepower",ylab="Weight",zlab="mean MPG",
      shade=0.6,border = NA,expand = 0.2)

chor.WIN<-chorley$window
chor.dens.WIN<-kde2d(chorley$x,
                     chorley$y,
                     n=256,
                     lims=c(chor.WIN$xrange,
                            chor.WIN$yrange))
chor.xy<-expand.grid(chor.dens.WIN$x,
                     chor.dens.WIN$y)
chor.out.mat<-matrix(!inside.owin(x=chor.xy$Var1,
                                  y=chor.xy$Var2,
                                  w=chor.WIN))
chor.dens.WIN$z[chor.out.mat]<-NA

zm<-chor.dens.WIN$z
nr<-nrow(zm)
nc<-ncol(zm)
zf<-(zm[-1,-1]+zm[-1,-nc]+zm[-nr,-1]+zm[-nr,-nc])/4
zf
dim(zf)

#rbow<-rainbow(200,start=0,end=5/6)
rbow<-colorRampPalette(c("blue","Green"))
zf.breaks<-seq(min(zf,na.rm=TRUE),
               max(zf,na.rm=T),
               length=201)
zf.colors<-cut(zf,breaks=zf.breaks,include.lowest = T)
par(mar=c(0,1,0,7))
persp(chor.dens.WIN$x,
      chor.dens.WIN$y,
      chor.dens.WIN$z,
      border=NA,
      col=rbow(200)[zf.colors],theta=-30,phi=30,
      scale=F,expand=750,
      xlab="Eastings (km)",
      ylab="Northings (km)",
      zlab="Kernel estimate")
colorlegend(col=rbow,
            zlim=range(chor.dens.WIN$z,
                       na.rm=T),
            zval=seq(0,0.02,0.0025),
            main="KDE",digit=4,
            posx=c(0.85,0.87),
            posy=c(0.2,0.8))      

persprot<-function(skip=1,...){
      for(i in seq(90,20,by=-skip)){
            persp(phi=i,theta=0,...)
      }
      for(i in seq(0,360,by=skip)){
            persp(pho=20,theta=i,...)
      }
}
library("MASS")
quak.dens<-kde2d(x=quakes$long,y=quakes$lat,n=50)
persprot(x=quak.dens$x,
         y=quak.dens$y,
         z=quak.dens$z,
         border="red3",
         shade=0.4,
         ticktype="detailed",
         xlab="Longitude",
         ylab="Latitude",
         zlab="Kernel Estimate")


#----Exercise --------------------

library("boot")
nuclear
#a
nuc.fit.1<-lm(cost~cap+date,data=nuclear)
nuc.fit.2<-lm(cost~cap*date,data=nuclear)
#
dev.new()
par(mfrow=c(2,1),mar=c(1,1,1,1))

date.seq<-seq(min(nuclear$date),
              max(nuclear$date),
              length.out = 50)
cap.seq<-seq(min(nuclear$cap),
             max(nuclear$cap),
             length.out = 50)
predictors<-expand.grid(date=date.seq,
                        cap=cap.seq)
z.mat1<-matrix(predict(nuc.fit.1, #First linear model
                       newdata = predictors),
               nrow=50,
               ncol=50)
z.mat2<-matrix(predict(nuc.fit.2,
                       newdata = predictors),
               nrow=50,
               ncol=50)
persp(x=date.seq, #plot for the firs linear Model
      y=cap.seq,theta = 30,
      z=z.mat1,
      zlim=c(min(z.mat2),max(z.mat1)),
      xlab="Date",ylab="Capacity",zlab="Cost Prediction",
      shade=0.6,
      ticktype = "detailed",
      main="Linear regression only main effects")
persp(y=date.seq,
      x=cap.seq,
      z=z.mat2,
      theta=30,
      shade=0.4,
      xlab="Date",ylab="Capacity",zlab="Cost Prediction",
      zlim=c(min(z.mat2),max(z.mat1)),
      main="linear regression with effects")

#b
persp(x=date.seq,
     y=cap.seq,
     z=(z.mat1-z.mat2),
     xlab="Date",ylab="Capacity",zlab="Difference",
     ticktype = "detailed",
     shade=0.6,border = NA)

#c
?volcano
par(mar=c(6,6,6,6))
persp(x=1:nrow(volcano),xlab="Lat",
      y=1:ncol(volcano),ylab="Long",
      z=volcano,zlab="Topography")

#d
#
dev.new()
par(mar=c(1,1,1,6))
#Defining color
facetmat<-(volcano[-1,-1]+
      volcano[-1,-ncol(volcano)]+
      volcano[-nrow(volcano),-1]+
      volcano[-nrow(volcano),-ncol(volcano)])/4
tcols<-terrain.colors(50)
facetbreakes<-seq(min(volcano),max(volcano),length.out = 51)
sections<-cut(facetmat,breaks = facetbreakes,include.lowest = T)
#----
persp(x=1:nrow(volcano),xlab="Lat",
      y=1:ncol(volcano),ylab="Long",
      z=volcano,zlab="Topography",
      scale = FALSE,
      axes=F,
      expand=0.1,
      col=tcols[sections],
      border = NA,
      phi=30)
library("shape")
colorlegend(tcols,
            zlim=c(min(volcano),
                   max(volcano)),
            zval=seq(min(volcano),
                     max(volcano),
                     length.out = 4),
            main="Elevation (m)",
            main.cex = 0.8,
            posx=c(0.9,0.91),posy=c(0.1,0.9))

#(e)
library("spatstat")
library("MASS")
fire <- split(clmfires)$intentional #obtain the ppp object
firewin <- fire$window #obtain the Window info from PPP

#obtain the xrange and yrange of window in order to define the real 
#desired size of the plot.
firewin.xr <- firewin$xrange 
firewin.yr <- firewin$yrange
#'Define the KDE2d of the original ppp$x and ppp$y, use the limits 
#'from the window
fire.dens <- kde2d(x=fire$x, 
                   fire$y,n=256,
                   lims=c(firewin.xr,firewin.yr))
#'With the density matrix, use the expand.grid with density matrix from 
#'kde2d to have a table with all possible xy positions.
fire.xy <- expand.grid(fire.dens$x,
                       fire.dens$y)
#'Use !inside.owin() with expanded grid positions and the window polygon
#'to check if the position is inside the map or not. If is outside, make it 
#'an NA. 
fire.outside <- !inside.owin(x=fire.xy[,1],
                             y=fire.xy[,2],
                             w=firewin)
fire.dens$z[fire.outside] <- NA
#'Make the facet matrix by summing the 4 matrix and dived by 4
fm <- (fire.dens$z[-1,-1]+
             fire.dens$z[-1,-256]+
             fire.dens$z[-256,-1]+
             fire.dens$z[-256,-256])/4
#define the colors being it 50 in this case
hcols <- heat.colors(50)
#'Define the vector of colors for the data by selecting the colors palette
#'by a cutted version of the facet matrix
firecols <- hcols[cut(fm,
                      breaks=seq(min(fm,
                                     na.rm=TRUE),
                                 max(fm,na.rm=TRUE),
                                 length=51),
                      include.lowest=TRUE)]
#Create the Perspective plot with (Persp) by giving the kde2d to x,y and z
#sometimes, you'll need to expand= it.
persp(fire.dens$x,
      fire.dens$y,
      fire.dens$z,
      col=firecols,
      theta=10,
      phi=30,
      border=NA,
      scale=FALSE,
      expand=5e+6,
      ticktype="detailed",
      xlab="X",ylab="Y",zlab="Z")


#f
persprot<-function(skip=1,...){
      for(i in seq(90,20,by=-skip)){
            persp(phi=i,theta=0,...)
      }
      for(i in seq(0,360,by=skip)){
            persp(pho=20,theta=i,...)
      }
}
dev.new()
persp(skip=10,
      x=fire.dens$x,
      y=fire.dens$y,
      z=fire.dens$z,
      col=firecols,
      border=NA,
      scale=FALSE,
      expand=5e+6,
      ticktype="detailed",
      xlab="X",ylab="Y",zlab="Z")
