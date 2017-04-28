# ------------- BOOK ------------
install.packages("rgl")
library("rgl")
iris
pwid<-iris$Petal.Width
plen<-iris$Petal.Length
swid<-iris$Sepal.Width
slen<-iris$Sepal.Length
color<-colorRampPalette(c("Blue","Red"))
swid.fac<-cut(swid,breaks = 50,include.lowest = T)
plot3d(x=pwid,y=plen,z=swid,
       col=c(1,2,3)[as.numeric(iris$Species)],size=1,type="s")
legend3d("topright",col=1:3,legend=levels(iris$Species),pch=15,cex=2)
bg3d(color="blue")

slen.pal<-colorRampPalette(c("purple","yellow2","blue"))
cols<-slen.pal(50)
slen.cols<-cut(slen,breaks=seq(min(slen),max(slen),length=51),
               include.lowest=TRUE)

plot3d(x=pwid,
       y=plen,swid,
       type="s",
       size=1.5,col=cols[slen.cols],
       aspect=c(1,1.75,1),
       xlab="Petal width",
       ylab="Petal length",
       zlab="Sepal Width")

xfromto<-rep(pwid,each=2)
yfromto<-rep(plen,each=2)
zfromto<-rep(min(swid),times=2*nrow(iris))
zfromto[seq(2,length(zfromto),2)]<-swid
segments3d(x=xfromto,
           y=yfromto,
           z=zfromto,
           col=cols[slen.cols])
           
segments3d(x=xfromto,
           y=yfromto,
           z=zfromto,
           col=rep(cols[slen.cols],
           each=2))
grid3d(side="z+")

bgplot3d({plot.new();colorlegend(slen.pal(50),
                                 zlim=range(slen),
                                 zval=seq(4.5,7.5,0.5),digit=1,
                                 posx=c(0.91,0.93),
                                 posy=c(0.1,0.9),
                                 main="Sepal Length");
      legend("topleft",legend="test")})
# ---------------- Exercises --------------