reds<-seq(0,255,25)
greens<-seq(0,255,25)
blues<-seq(0,255,25)
full.rgb<-expand.grid(reds,greens,blues)
nrow(full.rgb)
plot3d(x=full.rgb[,1],
       y=full.rgb[,2],
       z=full.rgb[,3],
       col=rgb(full.rgb,
               maxColorValue = 255),
       type="s",
       size=1.5,
       xlab="Red",
       ylab="Green",
       zlab="Blue")

install.packages("mvtnorm")
library("mvtnorm")

rand2d.norm<-rmvnorm(n=500,mean=c(0,0))
plot(rand2d.norm,xlab="x",ylab="y")

vals<-seq(-3,3,length=50)
xy<-expand.grid(vals,vals)
z<-matrix(dmvnorm(xy),50,50)

contour(vals,vals,z,xlab="x",ylab="y")

rand3d.norm<- rmvnorm(n=500,mean=c(0,0,0))
plot3d(rand3d.norm,xlab="x",ylab="y",zlab="z")

xyz<-expand.grid(vals,vals,vals)
w<-array(dmvnorm(xyz),c(50,50,50))

max3d.norm<-dmvnorm(c(0,0,0),mean=c(0,0,0))
contour3d(x=vals,y=vals,z=vals,f=w,level=0.05*max3d.norm)

plot3d(rand3d.norm,xlab="x",ylab="y",zlab="z")
contour3d(x=vals,
          y=vals,
          z=vals,
          f=w,
          level=0.9*max3d.norm,
          add=T,
          alpha=0.5)

plot3d(rand3d.norm,xlab="x",ylab="y",zlab="z")
contour3d(x=vals,y=vals,z=vals,f=w,
          level=c(0.05,0.2,0.6,0.95)*max3d.norm,
          color="blue",#c("pink","green","blue","red"),
          alpha=c(0.1,0.2,0.4,0.9),add=TRUE)

quak<-quakes[,c("long","lat","depth")]
quak$depth<- -quak$depth

plot3d(x=quak$long,y=quak$lat,z=quak$depth,
       xlab="Long",ylab="Lat",zlab="Depth")

quak.dens3d<-kde(x=qual,gridsize=c(64,64,64),compute.cont=true)
x.latt<-quak.dens3d$eval.points[[1]]
x.latt<-quak.dens3d$eval.points[[2]]
x.latt<-quak.dens3d$eval.points[[3]]
