#The book ----
col2rgb("yellow")
rgb(0,3,12,maxColorValue = 255)
?rgb
pcol <- function(cols){
      n <- length(cols)
      dev.new(width=7,height=7)
      par(mar=rep(1,4))
      plot(1:5,1:5,type="n",xaxt="n",yaxt="n",ann=FALSE)
      for(i in 1:n){
            pt <- locator(1)
            rgbval <- col2rgb(cols[i])
            points(pt,cex=4,pch=19,col=cols[i])
            text(pt$x+1,
                 pt$y,
                 family="mono",
                 label=paste("\"",cols[i],
                             "\"","\nR: ",rgbval[1],"  G: ",
                             rgbval[2],"  B: ",rgbval[3],"\nhex: ",
                             rgb(t(rgbval),maxColorValue=255),sep=""))
      }
}

puryel.colors<-colorRampPalette(colors=c("purple","yellow"))
testcolors<-puryel.colors(500)

dev.new(height=3,whidth=8)
plot(1,1, type="n",xaxt="n",yaxt="n",ann=F,xlim=c(-10,510),ylim=c(0.5,2.5),
     ylab=c("testcolors","gray.colors"))
points(x=rep(1:500,2),y=rep(1:2,each=500),
       col=c(testcolors,gray.colors(500)),
             pch=16,
       cex=5)
axis(2,at=1:2,labels=c("testcolors","gray.colors"),las=1)

normalize<-function(datavec){
      lo<-min(datavec,na.rm=TRUE)
      up<-max(datavec,na.rm=TRUE)
      datanorm<-(datavec-lo)/(up-lo)
      return(datanorm)
}

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
NW.pal<-colorRampPalette(colors=c("red4","yellow2"))
NW.pal2 <- colorRamp(colors=c("red4","yellow2"))

#

ryc2 <- NW.pal2(normalize(surv$NW.Hnd))

#

NW.cols2 <- rgb(ryc2,maxColorValue=255)
NW.cols2

#

library("shape")
dev.new()
par(mar=c(10,10,10,10),xpd=NA)
plot(surv$Wr.Hnd,surv$Height,col=NW.cols2,pch=19,
     xlab="Writing handspan (cm)",ylab="Height (cm)")
colorlegend(NW.pal(200),zlim=range(surv$NW.Hnd),zval=seq(13,23,by=2),
            posx=c(0.75,0.755),posy=c(0,1),main="Nonwriting handspan",xpd)

keycols<-c("blue","red","yellow")
depth.pal<-colorRampPalette(keycols)
depth.pal2<-colorRamp(keycols)

depth.cols<-rgb(depth.pal2(normalize(quakes$depth)),maxColorValue = 255,
                alpha=0.6*255)
plot(quakes$mag,
     quakes$stations,
     pch=19,
     cex=2,
     col=depth.cols,
     xlab="Magnitude",
     ylab="No. of stations")
colorlegend(adjustcolor(depth.pal(20),
                        alpha.f = 0.6),
      zlim = range(quakes$depth),
      zval = seq(100, 600, 100),
      posx = c(0.3, 0.32),
      posy = c(0.5, 0.9),
      left = TRUE,
      main = "Depth")
#---------------------------------------- Exercises ----
library("car")
?Salaries

#a
colordef<-c("black","red","yellow2")
colpal<-colorRampPalette(colordef)
colpal2<-colorRamp(colordef)

#b
#i
pch.vec<-c(19,17,15)[as.numeric(Salaries$rank)]
#ii
cex.vec<-c(1,1.5)[as.numeric(Salaries$sex)]

#c
norm.ysphd<-normalize(Salaries$yrs.since.phd)
hex.col<-rgb(colpal2(norm.ysphd),maxColorValue = 255)

#d
hex.col.opa<-adjustcolor(hex.col,
                         alpha.f = 0.9)
hex.col.opa[Salaries$sex=="Male"]<-adjustcolor(hex.col.opa[Salaries$sex=="Male"],
                         alpha.f=0.3)

#e
dev.new()
par(mar=c(5,4,4,6),xpd=NA)
plot(Salaries$salary~Salaries$yrs.service,
     col=hex.col.opa,
     pch=pch.vec,
     cex=cex.vec,
     type="p",
     xlab="Years of Service",
     ylab="Salary")

#f
#i
legend(xpd=NA,
       x=-5,
       y=260000,
       legend = levels(Salaries$rank),
       pch=as.numeric(levels(as.factor(pch.vec))),
       ncol=3,
       title="Rank")
#ii
legend(xpd=NA,
       x=40,
       y=260000,
       legend=levels(Salaries$sex),
       pch=19,
       col=c(adjustcolor("red",alpha.f=0.9),
             adjustcolor("red",alpha.f=0.3)),
       pt.cex=as.numeric(levels(as.factor(cex.vec))),
       ncol=2,
       title="Sex")

#g
library("shape")
legendcolors<-rgb(colpal2(normalize(1:50)),maxColorValue = 255)
colorlegend(legendcolors,
            zlim=c(min(Salaries$yrs.service),
                   max(Salaries$yrs.service)),
            zval=seq(10,50,10),
            posx=c(0.95,0.955),
            posy=c(0.2,0.8),
            main="Years since \nPHD",
            xpd=NA)
#---------
#h
tcols<-terrain.colors(25)
rtcols<-tcols[25:1]
ftcols<-c(tcols,rtcols)

#i
vals<-seq(-3,3,length.out = 51)
normvals<-dnorm(vals)

#j
plot(vals,
     normvals,
     type="l",
     bty="l",
     xaxt="n",
     xaxs="i",
     xlab="",
     yaxs="i",
     ylab="",
     main="N(0,1) Density")

#k
for(i in 1:(length(vals)-1)){
      polygon(x=rep(vals[c(i,i+1)],each=2),
              y=c(0,normvals[c(i,i+1)],0),
              col=ftcols[i],
              border=NA)
}      

#l
colorlegend(col=ftcols,
            zlim=c(-3,3),
            zlevels = 6,
            posx = c(0.8,0.815),
            posy=c(0.1,0.9),
            main="SD from mean")
            