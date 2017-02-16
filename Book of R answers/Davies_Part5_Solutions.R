
######################################
#### SUGGESTED EXERCISE SOLUTIONS ####
######################################

##########
## 23.1 ##
##########
#(a)
par(mfrow=c(2,1))
boxplot(mtcars$mpg~mtcars$cyl,xlab="Cylinders",ylab="MPG")
carfit <- lm(mpg~cyl,data=mtcars)
plot(mtcars$mpg~mtcars$cyl,xlab="Cylinders",ylab="MPG")
abline(carfit,lwd=2)
#(b)
##(i)
lay.mat <- cbind(c(2,1,1,3),c(2,1,1,3))
lay.mat
layout(lay.mat)
layout.show(3)
##(ii)
lay.mat <- rbind(c(1,1,2,3),c(1,1,4,5))
lay.mat
layout(lay.mat)
layout.show(5)
##(iii)
lay.mat <- cbind(c(2,3,3,1),c(2,3,3,1),c(2,4,5,1))
lay.mat
layout(lay.mat)
layout.show(5)
#(c)
dev.new(width=9,height=4.5)
lay.mat <- rbind(c(1,1,2,4),c(1,1,3,4))
layout(lay.mat)
layout.show(4)
par(mar=c(4,4,2,1))
plot(quakes$long,quakes$lat,cex=0.02*quakes$stations,xlab="Longitude",ylab="Latitude")
box(which="figure",col="gray")
plot(quakes$mag,quakes$stations,xlab="Magnitude",ylab="Stations")
box(which="figure",col="gray")
plot(quakes$depth,quakes$stations,xlab="Depth",ylab="Stations")
box(which="figure",col="gray")
hist(quakes$stations,main="",xlab="Stations")
abline(v=mean(quakes$stations),lty=2)
box(which="figure",col="gray")
#(d)
interactive.arrow <- function(...,label=NA){
    arr.pts <- locator(2)
    arrows(x0=arr.pts$x[1],y0=arr.pts$y[1],x1=arr.pts$x[2],y1=arr.pts$y[2],...)
    if(!is.na(label)){
      lab.pt <- text(locator(1),label=label,xpd=NA)
    }
}
boxplot(quakes$mag)
interactive.arrow(xpd=TRUE,label="minumum")
interactive.arrow(xpd=TRUE,label="1st quartile")
interactive.arrow(xpd=TRUE,label="median")
interactive.arrow(xpd=TRUE,label="3rd quartile")
interactive.arrow(xpd=TRUE,label="maximum")
interactive.arrow(xpd=TRUE,label="outliers")

##########
## 23.2 ##
##########
dia.url <- "http://www.amstat.org/publications/jse/v9n2/4cdata.txt"
diamonds <- read.table(dia.url)
names(diamonds) <- c("Carat","Color","Clarity","Cert","Price")
#(a)
dev.new(width=6,height=6)
par(mar=c(0,4,2,0))
##(i)
boxplot(diamonds$Price~diamonds$Cert,axes=FALSE,frame=FALSE,main="Diamond Prices by Certification")
##(ii)
axis(2,at=seq(0,18000,2000),las=1,tcl=1,mgp=c(3,0.5,0))
##(iii)
text(locator(1),"SGD$",xpd=TRUE)
text(locator(1),"GIA",cex=1.5)
text(locator(1),"HRD",cex=1.5)
text(locator(1),"IGI",cex=1.5)
#(b)
dev.new(width=8,height=7)
par(mar=c(2,5,3,5),oma=c(2,rep(1,3)))
##(i)
plot(diamonds$Price~diamonds$Carat,col=c("red","green","blue")[as.numeric(diamonds$Cert)],axes=FALSE,ann=FALSE)
box(bty="u")
##(ii)
axis(1,at=seq(0.2,1.1,0.1),font=4,mgp=c(3,0.5,0))
axis(1,at=seq(0.15,1.05,0.1),tcl=-0.25,labels=FALSE)
##(iii)
axis(2,at=seq(1000,17000,2000),las=1,font=4)
axis(4,at=seq(1000,11000,1000)*1.37,labels=seq(1000,11000,1000),las=1,font=4)
##(iv)
dia.fit <- lm(Price~Carat+I(Carat^2),data=diamonds)
carat.seq <- seq(min(diamonds$Carat),max(diamonds$Carat),length=100)
dia.pred <- predict(dia.fit,newdata=data.frame(Carat=carat.seq),interval="prediction")
lines(carat.seq,dia.pred[,1],col="gray",lwd=2)
lines(carat.seq,dia.pred[,2],col="gray",lty=2)
lines(carat.seq,dia.pred[,3],col="gray",lty=2)
##(v)
expr1 <- expression("USD$"%~~%1.37%*%"SGD$")
expr2 <- expression(paste("Price"==beta[0]+beta[1],"Carat",+beta[2],"Carat"^2))
##(vi)
mtext("CARAT",side=1,line=0,outer=TRUE)
mtext("SGD$",side=2,line=4)
mtext("Scatterplot of Diamond Price by Carat and Certification",side=3,line=2,cex=1.5)
mtext(expr1,side=4,line=4)
##(vii)
interactive.arrow(label=expr2)
##(viii)
legend(locator(1),legend=levels(diamonds$Cert),col=c("red","green","blue"),pch=1)

##########
## 24.1 ##
##########
library("MASS")
?UScereal
library("ggplot2")
#(a)
cereal <- UScereal
new.mfr <- as.numeric(UScereal$mfr)
new.mfr[new.mfr>2] <- 3
cereal$mfr <- factor(new.mfr,labels=c("General Mills","Kelloggs","Other"))
cereal$shelf <- factor(cereal$shelf)
#(b)
##(i)
gg1 <- ggplot(cereal,aes(x=protein,y=calories,col=shelf)) + geom_point(aes(shape=mfr)) + geom_smooth(method="lm") + labs(x="Protein",y="Calories",col="Shelf",size="Carbs",shape="Manufacturer")
gg1
##(ii)
gg2 <- ggplot(cereal,aes(x=calories,fill=shelf)) + geom_density(alpha=0.5) + labs(x="Calories",y="Kernel estimate",fill="Shelf")
gg2
#(c)
library("gridExtra")
grid.arrange(gg1,gg2)
#(d)
ggplot(cereal,aes(x=protein,y=calories)) + geom_point(aes(col=sugars,size=sodium,shape=shelf)) + geom_smooth(method="loess",span=0.9) + facet_wrap(~mfr) + labs(x="Protein",y="Calories",col="Sugars",shape="Shelf",size="Sodium")
#(e)
library("car")
gg1 <- ggplot(Salaries,aes(x=yrs.service,y=salary,col=sex)) + geom_point() + geom_smooth(method="loess") + labs(x="Years of Service",y="Salary",col="Sex")
gg1
#(f)
##(i)
gg2 <- ggplot(Salaries,aes(x=rank,y=salary,col=sex)) + geom_boxplot() + labs(x="Rank",y="Salary",col="Sex")
gg2
##(ii)
gg3 <- ggplot(Salaries,aes(x=discipline,y=salary,fill=sex)) + geom_boxplot() + labs(x="Discipline",y="Salary",fill="Sex")
gg3
##(iii)
gg4 <- ggplot(Salaries,aes(x=salary,fill=rank)) + geom_density(alpha=0.3) + labs(x="Salary",y="Kernel estimate",fill="Rank")
gg4
#(g)
grid.arrange(gg1,gg2,gg3,gg4)
#(h)
##(i)
ggplot(Salaries,aes(x=salary,fill=sex)) + geom_density(alpha=0.7) + facet_wrap(~rank) + labs(x="Salary",y="Kernel estimate",fill="Sex")
##(ii)
ggplot(Salaries,aes(x=yrs.service,y=salary,col=sex)) + geom_point() + geom_smooth(method="lm") + facet_grid(discipline~rank,scales="free_x") + labs(x="Years of Service",y="Salary",col="Sex")

##########
## 24.2 ##
##########
library("ggvis")
library("car")
?Salaries
#(a)
salfill <- input_radiobuttons(c("Rank"="rank","Discipline"="discipline","Sex"="sex"),map=as.name,label="Color points by...")
Salaries %>% ggvis(x=~yrs.service,y=~salary,fill=salfill) %>% layer_points() %>% add_legend("fill",title="") %>% add_axis("x",title="Years of service") %>% add_axis("y",title="Salary")
#(b)
##(i)
Salaries %>% ggvis(x=~salary,fill=~rank) %>% group_by(rank) %>% layer_densities()
##(ii)
Salaries %>% ggvis(x=~salary,fill=~rank) %>% group_by(rank) %>% layer_densities(adjust=input_slider(0.1,2,label="Smoothness")) %>% add_axis("x",title="Salary") %>% add_axis("y",title="Kernel estimate") %>% add_legend("fill",title="Rank")
#(c)
library("MASS")
cereal <- UScereal
new.mfr <- as.numeric(UScereal$mfr)
new.mfr[new.mfr>2] <- 3
cereal$mfr <- factor(new.mfr,labels=c("General Mills","Kelloggs","Other"))
cereal$shelf <- factor(cereal$shelf)
filler <- input_radiobuttons(c("Manufacturer"="mfr","Shelf"="shelf","Vitamins"="vitamins"),map=as.name,label="Color points by...")
sizer <- input_slider(10,300,label="Point size:")
opacityer <- input_slider(0.1,1,label="Opacity:")
#(d)
cereal %>% ggvis(x=~protein,y=~calories,fill=filler,size:=sizer,opacity:=opacityer) %>% layer_points() %>% add_axis("x",title="Protein") %>% add_axis("y",title="Calories") %>% add_legend("fill",title="")
#(e)
shaper <- input_radiobuttons(c("Manufacturer"="mfr","Shelf"="shelf","Vitamins"="vitamins"),map=as.name,label="Shape points by...")
#(f)
cereal %>% ggvis(x=~protein,y=~calories,fill=filler,shape=shaper,opacity:=opacityer,size:=sizer) %>% layer_points() %>% add_axis("x",title="Protein") %>% add_axis("y",title="Calories") %>% add_legend("fill",title="") %>% add_legend("shape",title="",properties=legend_props(legend=list(y=100))) %>% set_options(duration=0)

##########
## 25.1 ##
##########
library("car")
#(a)
cols1 <- colorRampPalette(c("black","red","yellow2"))
cols2 <- colorRamp(c("black","red","yellow2"))
#(b)
##(i)
rank.pchs <- c(19,17,15)[as.numeric(Salaries$rank)]
##(ii)
sex.cexs <- c(1,1.5)[as.numeric(Salaries$sex)]
#(c)
normalize <- function(datavec){
  lo <- min(datavec,na.rm=TRUE)
  up <- max(datavec,na.rm=TRUE)
  datanorm <- (datavec-lo)/(up-lo)
  return(datanorm)
}
phd.norm <- normalize(Salaries$yrs.since.phd)
phd.cols <- rgb(cols2(phd.norm),maxColorValue=255)
#(d)
phd.cols[Salaries$sex=="Female"] <- adjustcolor(phd.cols[Salaries$sex=="Female"],alpha=0.9)
phd.cols[Salaries$sex=="Male"] <- adjustcolor(phd.cols[Salaries$sex=="Male"],alpha=0.3)
#(e)
par(mar=c(5,4,4,6))
plot(Salaries$salary~Salaries$yrs.service,col=phd.cols,pch=rank.pchs,cex=sex.cexs,xlab="Years of service",ylab="Salary")
#(f)
legend(-5,265000,legend=levels(Salaries$rank),title="Rank",pch=c(19,17,15),xpd=TRUE,horiz=TRUE)
legend(40,265000,legend=levels(Salaries$sex),title="Sex",pch=c(19,19),pt.cex=c(1,1.5),col=c(adjustcolor("red",0.9),adjustcolor("red",0.3)),xpd=TRUE,horiz=T)
#(g)
library("shape")
colorlegend(col=cols1(50),zlim=range(Salaries$yrs.since.phd),zval=seq(10,50,10),main="Years since\nPhD")
#(h)
tcols <- terrain.colors(25)
fillcols <- c(tcols,tcols[25:1])
#(i)
nvals <- 51
vals <- seq(-3,3,length=nvals)
normvals <- dnorm(vals)
#(j)
plot(vals,normvals,type="l",xaxs="i",yaxs="i",xlab="",ylab="",bty="L",xaxt="n",main="N(0,1) density")
#(k)
for(i in 1:(nvals-1)){
  polygon(x=rep(vals[c(i,i+1)],each=2),y=c(0,normvals[c(i,i+1)],0),border=NA,col=fillcols[i])
}
#(l)
library("shape")
colorlegend(fillcols,zlim=range(vals),zval=-3:3,main="SD from mean")

##########
## 25.2 ##
##########
library("scatterplot3d")
#(a)
library("faraway")
scatterplot3d(x=diabetes$hip,y=diabetes$waist,z=diabetes$weight,highlight.3d=TRUE,pch=c(1,2)[as.numeric(diabetes$gender)],xlab="Hip",ylab="Waist",zlab="Weight")
legend("topleft",legend=levels(diabetes$gender),pch=1:2)
#(b)
aq <- na.omit(airquality)
ozcols <- topo.colors(50)
colindex <- cut(aq$Ozone,breaks=seq(min(aq$Ozone),max(aq$Ozone),length=51),include.lowest=TRUE)
scatterplot3d(x=aq$Wind,y=aq$Solar.R,z=aq$Temp,type="h",lty.hplot=3,color=ozcols[colindex],pch=aq$Month-4,main="NY Air Quality",xlab="Wind (MPH)",ylab=expression(paste("Solar Radiation (",ring(A),")")),zlab="Temperature (F)")
legend(locator(1),legend=c("May","June","July","August","September"),pch=1:5,title="Month",xpd=TRUE)
library("shape")
colorlegend(ozcols,zlim=range(aq$Ozone),main="Ozone",zval=seq(0,160,40),posx=c(0.1,0.13),posy=c(0.7,0.9))

##########
## 25.3 ##
##########
library("boot")
?nuclear
#(a)
##(i)
nuc.fit1 <- lm(cost~cap+date,data=nuclear)
summary(nuc.fit1)
##(ii)
nuc.fit2 <- lm(cost~cap*date,data=nuclear)
summary(nuc.fit2)
#(b)
capseq <- seq(min(nuclear$cap),max(nuclear$cap),length=50)
datseq <- seq(min(nuclear$date),max(nuclear$date),length=50)
capdat <- expand.grid(cap=capseq,date=datseq)
nuc.pred1 <- matrix(predict(nuc.fit1,newdata=capdat),50,50)
nuc.pred2 <- matrix(predict(nuc.fit2,newdata=capdat),50,50)
#(c)
par(mfrow=c(1,2))
contour(x=capseq,y=datseq,z=nuc.pred1)
contour(x=capseq,y=datseq,z=nuc.pred2)
# The two response surfaces appear extremely similar, with only a slight curvature appearing in the surface associated with the interactive model. This similarity is to be expected based on the model summaries from (a), which don't provide any evidence supporting the need to include the interaction.
#(d)
filled.contour(x=capseq,y=datseq,z=nuc.pred1,xlab="Capacity",ylab="Permit date",color.palette=topo.colors,plot.axes={axis(1);axis(2);contour(capseq,datseq,nuc.pred2,add=TRUE,lwd=2,lty=2)})
text(locator(1),label=c("Color fill: Main effects only.\nLines: Interaction included."),xpd=NA)
#(e)
?faithful
plot(faithful$eruptions~faithful$waiting,xlab="Waiting time",ylab="Eruption duration",main="Old Faithful Geyser")
#(f)
library("MASS")
faith.dens <- kde2d(x=faithful$waiting,y=faithful$eruptions,n=100)
contour(faith.dens$x,faith.dens$y,faith.dens$z)
#(g)
faith.cols <- colorRampPalette(c("darkblue","hotpink"))
filled.contour(faith.dens$x,faith.dens$y,faith.dens$z,color.palette=faith.cols,xlab="Waiting time",ylab="Eruption duration",plot.axes={axis(1);axis(2);points(faithful$waiting,faithful$eruption,cex=0.5,col="gray")})
#(h)
plot(faithful$eruptions~faithful$waiting,col="gray",pch=2,cex=0.75,xaxs="i",yaxs="i",xlab="Waiting time",ylab="Eruption duration",main="Old Faithful Geyser")
contour(faith.dens$x,faith.dens$y,faith.dens$z,levels=seq(0.002,0.014,0.004),col="red4",add=TRUE,drawlabels=FALSE,lwd=1:4)
legend("topleft",legend=seq(0.002,0.014,0.004),lwd=1:4,col="red4",title="Kernel estimate")

##########
## 25.4 ##
##########
aq <- na.omit(airquality[,c("Temp","Wind","Ozone")])
#(a)
aq.fit <- lm(Temp~Wind*Ozone,aq)
summary(aq.fit)
#(b)
res <- 50
winseq <- seq(min(aq$Wind),max(aq$Wind),length=res)
ozoseq <- seq(min(aq$Ozone),max(aq$Ozone),length=res)
winozo <- expand.grid(Wind=winseq,Ozone=ozoseq)
aq.pred <- matrix(predict(aq.fit,newdata=winozo),res,res)
#(c)
normalize <- function(datavec){
  lo <- min(datavec,na.rm=TRUE)
  up <- max(datavec,na.rm=TRUE)
  datanorm <- (datavec-lo)/(up-lo)
  return(datanorm)
}
library("shape")
par(mar=c(5,4,4,6))
image(x=winseq,y=ozoseq,z=aq.pred,col=topo.colors(20),xlab="Wind",ylab="Ozone")
points(aq$Wind,aq$Ozone,col=gray(normalize(aq$Temp)),pch=19)
colorlegend(col=topo.colors(20),zlim=range(aq.pred),zval=seq(60,140,10),posx=c(0.88,0.91),main="Pred. temp")
colorlegend(col=gray.colors(10,start=0,end=1),zlim=range(aq$Temp),zval=seq(60,95,5),posx=c(0.67,0.7),posy=c(0.4,0.8),main="Obs. temp")
#(d)
library("spatstat")
?clmfires
fire <- split(clmfires)$intentional
firewin <- clmfires$window
firewin.xr <- firewin$xrange
firewin.yr <- firewin$yrange
fire.dens <- kde2d(x=fire$x,fire$y,n=256,lims=c(firewin.xr,firewin.yr))
#(e)
fire.xy <- expand.grid(fire.dens$x,fire.dens$y)
fire.outside <- !inside.owin(x=fire.xy[,1],y=fire.xy[,2],w=firewin)
fire.dens$z[fire.outside] <- NA
#(f)
library("shape")
par(mar=c(3,3,3,7))
image(fire.dens$x,fire.dens$y,fire.dens$z,col=heat.colors(50),asp=1,bty="l",ann=FALSE)
plot(firewin,lwd=2,add=TRUE)
colorlegend(col=heat.colors(50),posx=c(0.83,0.86),zlim=range(fire.dens$z,na.rm=TRUE),zval=seq(5e-6,35e-6,5e-6),digit=6)

##########
## 25.5 ##
##########
library("boot")
#(a)
nuc.fit1 <- lm(cost~cap+date,data=nuclear)
nuc.fit2 <- lm(cost~cap*date,data=nuclear)
capseq <- seq(min(nuclear$cap),max(nuclear$cap),length=50)
datseq <- seq(min(nuclear$date),max(nuclear$date),length=50)
capdat <- expand.grid(cap=capseq,date=datseq)
nuc.pred1 <- matrix(predict(nuc.fit1,newdata=capdat),50,50)
nuc.pred2 <- matrix(predict(nuc.fit2,newdata=capdat),50,50)
par(mfrow=c(2,1),mar=rep(1,4))
persp(x=capseq,y=datseq,z=nuc.pred1,theta=25,zlim=range(c(nuc.pred1,nuc.pred2)),ticktype="detailed",xlab="Capacity",ylab="Permit issue date",zlab="Predicted cost")
persp(x=capseq,y=datseq,z=nuc.pred2,theta=25,zlim=range(c(nuc.pred1,nuc.pred2)),ticktype="detailed",xlab="Capacity",ylab="Permit issue date",zlab="Predicted cost")
# As expected based on earlier results, there's barely any visual differences between the two surfaces. Inclusion of the interaction term, as evidenced by extremely large p-values, has no tangible effect on modeling the response.
#(b)
par(mfrow=c(1,1),mar=rep(1,4)) #(reset to default)
persp(x=capseq,y=datseq,z=nuc.pred2-nuc.pred1,theta=75,ticktype="detailed")
# The main impact of including the interaction term seems to be a discrepancy (when compared with the main-effect-only model) at the later dates (approx. 1970-71) as you increase plant capacity.
#(c)
vr <- nrow(volcano)
vc <- ncol(volcano)
persp(x=1:vr,y=1:vc,z=volcano)
#(d)
dev.new()
par(mar=c(1,1,1,5))
vm <- (volcano[-1,-1]+volcano[-1,-vc]+volcano[-vr,-1]+volcano[-vr,-vc])/4
tcols <- terrain.colors(50)
volcol <- tcols[cut(vm,breaks=seq(min(volcano),max(volcano),length=51),include.lowest=TRUE)]
persp(1:vr,1:vc,volcano,col=volcol,border=NA,theta=-30,phi=15,scale=FALSE,expand=0.1,axes=FALSE)
library("shape")
colorlegend(tcols,zlim=range(volcano),zval=seq(100,180,20),posx=c(0.83,0.86),posy=c(0.4,0.7),main="Elevation (m)")
#(e)
library("spatstat")
library("MASS")
fire <- split(clmfires)$intentional
firewin <- clmfires$window
firewin.xr <- firewin$xrange
firewin.yr <- firewin$yrange
fire.dens <- kde2d(x=fire$x,fire$y,n=256,lims=c(firewin.xr,firewin.yr))
fire.xy <- expand.grid(fire.dens$x,fire.dens$y)
fire.outside <- !inside.owin(x=fire.xy[,1],y=fire.xy[,2],w=firewin)
fire.dens$z[fire.outside] <- NA
fm <- (fire.dens$z[-1,-1]+fire.dens$z[-1,-256]+fire.dens$z[-256,-1]+fire.dens$z[-256,-256])/4
hcols <- heat.colors(50)
firecols <- hcols[cut(fm,breaks=seq(min(fm,na.rm=TRUE),max(fm,na.rm=TRUE),length=51),include.lowest=TRUE)]
persp(fire.dens$x,fire.dens$y,fire.dens$z,col=firecols,theta=10,phi=30,border=NA,scale=FALSE,expand=5e+6,ticktype="detailed",xlab="X",ylab="Y",zlab="Z")
#(f)
persprot <- function(...,skip=1){
  for(i in seq(90,20,by=-skip)){
    persp(phi=i,theta=0,...)
  }
  for(i in seq(0,360,by=skip)){
    persp(phi=20,theta=i,...)
  }
}
persprot(skip=10,fire.dens$x,fire.dens$y,fire.dens$z,col=firecols,border=NA,scale=FALSE,expand=5e+6,ticktype="detailed",xlab="X",ylab="Y",zlab="Z")

##########
## 26.1 ##
##########
library("rgl")
library("MASS")
#(a)
surv <- na.omit(survey[,c("Wr.Hnd","NW.Hnd","W.Hnd","Sex","Height")])
wrh <- surv$Wr.Hnd
nwh <- surv$NW.Hnd
wh <- surv$W.Hnd
hei <- surv$Height
sex <- surv$Sex
plot3d(x=wrh,y=nwh,z=hei)
#(b)
plot3d(x=wrh[wh=="Right"],y=nwh[wh=="Right"],z=hei[wh=="Right"],col=c("black","red")[as.numeric(sex[wh=="Right"])],size=4,xlab="Writing hand",ylab="Non-writing hand",zlab="Height")
points3d(x=wrh[wh=="Left"],y=nwh[wh=="Left"],z=hei[wh=="Left"],col=c("black","red")[as.numeric(sex[wh=="Left"])],size=10)
legend3d("topleft",legend=c("Male LH","Female RH","Male LH","Female LH"),pch=19,pt.cex=c(0.8,0.8,1.5,1.5),col=c("black","red","black","red"))
#(c)
aq <- na.omit(airquality)
ozcols <- topo.colors(50)
colindex <- cut(aq$Ozone,breaks=seq(min(aq$Ozone),max(aq$Ozone),length=51),include.lowest=TRUE)
plot3d(aq$Wind,aq$Solar.R,aq$Temp,type="s",col=ozcols[colindex],size=1,aspect=c(1,1.5,1),xlab="Wind",ylab="Solar radiation",zlab="Temperature")
xfromto <- rep(aq$Wind,each=2)
yfromto <- rep(aq$Solar.R,each=2)
zfromto <- rep(min(aq$Temp),times=2*nrow(aq))
zfromto[seq(2,length(zfromto),2)] <- aq$Temp
segments3d(x=xfromto,y=yfromto,z=zfromto,col=rep(ozcols[colindex],each=2))
grid3d(side="z-")
library("shape")
bgplot3d({plot.new();colorlegend(ozcols,zlim=range(aq$Temp),zval=seq(60,95,5),posx=c(0.91,0.93),posy=c(0.1,0.9))})

##########
## 26.2 ##
##########
library("rgl")
#(a)
aq <- na.omit(airquality[,c("Temp","Wind","Ozone","Month")])
aq.fit <- lm(Temp~Wind*Ozone,aq)
summary(aq.fit)
res <- 50
winseq <- seq(min(aq$Wind),max(aq$Wind),length=res)
ozoseq <- seq(min(aq$Ozone),max(aq$Ozone),length=res)
winozo <- expand.grid(Wind=winseq,Ozone=ozoseq)
aq.pred <- predict(aq.fit,newdata=winozo,interval="confidence",level=0.95)
aq.zmat <- matrix(aq.pred[,1],res,res)
persp3d(x=winseq,y=ozoseq,z=aq.zmat,col="yellow")
#(b)
tcols <- topo.colors(50)
persp3d(x=winseq,y=ozoseq,z=aq.zmat,col=tcols[cut(aq.pred,breaks=seq(min(aq.pred),max(aq.pred),length=51),include.lowest=TRUE)],alpha=0.8,xlab="Wind",ylab="Ozone",zlab="Temp.")
#(c)
##(i)
reds <- colorRampPalette(c("red4","pink"))
rcols <- reds(5)
points3d(aq$Wind,aq$Ozone,aq$Temp,col=rcols[aq$Month-4],size=10)
##(ii)
xfromto <- rep(aq$Wind,each=2)
yfromto <- rep(aq$Ozone,each=2)
zfromto <- rep(aq.fit$fitted.values,each=2)
zfromto[seq(2,2*nrow(aq),2)] <- aq$Temp
segments3d(x=xfromto,y=yfromto,z=zfromto,col=rep(rcols[aq$Month-4],each=2))
##(iii)
persp3d(x=winseq,y=ozoseq,z=matrix(aq.pred[,2],res,res),col="gray",alpha=0.5,add=TRUE)
persp3d(x=winseq,y=ozoseq,z=matrix(aq.pred[,3],res,res),col="gray",alpha=0.5,add=TRUE)
##(iv)
legend3d("topright",legend=c("May","June","July","August","September"),pch=19,cex=2,col=rcols)
#(d)
library("spatstat")
library("MASS")
fire <- split(clmfires)$intentional
firewin <- clmfires$window
firewin.xr <- firewin$xrange
firewin.yr <- firewin$yrange
fire.dens <- kde2d(x=fire$x,fire$y,n=256,lims=c(firewin.xr,firewin.yr))
fire.xy <- expand.grid(fire.dens$x,fire.dens$y)
fire.outside <- !inside.owin(x=fire.xy[,1],y=fire.xy[,2],w=firewin)
fire.dens$z[fire.outside] <- NA
hcols <- heat.colors(50)
firecols <- hcols[cut(fire.dens$z,breaks=seq(min(fire.dens$z,na.rm=TRUE),max(fire.dens$z,na.rm=TRUE),length=51),include.lowest=TRUE)]
xr <- firewin$xrange[2]-firewin$xrange[1]
yr <- firewin$yrange[2]-firewin$yrange[1]
persp3d(x=fire.dens$x,y=fire.dens$y,z=fire.dens$z,col=firecols,aspect=c(xr/yr,1,0.6),alpha=0.7,xlab="X",ylab="Y",zlab="")
#(e)
##(i)
points3d(x=fire$x,y=fire$y,z=min(fire.dens$z,na.rm=TRUE))
##(ii)
firepoly <- vertices(firewin)
fwx <- firepoly$x
fwy <- firepoly$y
lines3d(x=fwx,y=fwy,z=min(fire.dens$z,na.rm=TRUE),lwd=2)

##########
## 26.3 ##
##########
library("mvtnorm")
library("rgl")
library("misc3d")
library("ks")
covmat <- matrix(c(1,0.8,0.4,0.8,1,0.6,0.4,0.6,1),3,3)
rand3d.norm <- rmvnorm(1000,mean=c(0,0,0),sigma=covmat)
#(a)
plot3d(rand3d.norm,xlab="x",ylab="y",zlab="z")
#(b)
vals <- seq(-3,3,length=50)
xyz <- expand.grid(vals,vals,vals)
w <- array(dmvnorm(xyz,mean=c(0,0,0),sigma=covmat),c(50,50,50))
max3d.norm <- dmvnorm(c(0,0,0),mean=c(0,0,0),sigma=covmat)
contour3d(x=vals,y=vals,z=vals,f=w,level=c(0.1,0.5,0.9)*max3d.norm,color=c("yellow","seagreen4","navyblue"),alpha=c(0.2,0.4,0.6),add=TRUE)
#(c)
kde3d.norm <- kde(rand3d.norm,compute.cont=TRUE)
plot3d(rand3d.norm,xlab="x",ylab="y",zlab="z")
##(i)
contour3d(x=vals,y=vals,z=vals,f=w,level=0.5*max3d.norm,color="seagreen4",alpha=0.4,add=TRUE)
##(ii)
contour3d(x=kde3d.norm$eval.points[[1]],y=kde3d.norm$eval.points[[2]],z=kde3d.norm$eval.points[[3]],f=kde3d.norm$estimate,level=kde3d.norm$cont[50],color="red",alpha=0.2,add=TRUE)
#(d)
library("MASS")
?Boston
bos <- Boston[,c("rm","lstat","medv")]
##(i)
plot3d(bos,col="gray",type="s",size=0.5,xlab="Avg. rooms",ylab="Lower status %",zlab="Median value")
##(ii)
bos.dens <- kde(bos,gridsize=c(64,64,64),compute.cont=TRUE)
contour3d(x=bos.dens$eval.points[[1]],y=bos.dens$eval.points[[2]],z=bos.dens$eval.points[[3]],f=bos.dens$estimate,level=bos.dens$cont[c(75,50,10)],color=c("green","yellow","blue"),alpha=c(0.1,0.4,0.5),add=TRUE)
##(iii)
grid3d(side=c("z-","x+","y+"))
#(e)
# The most common houses tend to have an average of around 6 rooms, are associated with lower-status areas of around 5 to 15%, and have median values of around $20,000 to $30,000. There seems to be an additional pocket of observations that represent relatively high-value suburb homes with a large average number of rooms in areas with a very small lower-status percentage.
#(f)
vals <- seq(-pi,pi,length=1000)
theta.phi <- expand.grid(vals,vals)
xv <- outer(vals,vals,function(theta,phi) sin(theta)*(7+cos(theta/3-2*phi)+2*cos(theta/3+phi)))
yv <- outer(vals,vals,function(theta,phi) cos(theta)*(7+cos(theta/3-2*phi)+2*cos(theta/3+phi)))
zv <- outer(vals,vals,function(theta,phi) sin(theta/3-2*phi)+2*sin(theta/3+phi))
persp3d(x=xv,y=yv,z=zv,col=rainbow(1000),axes=FALSE,xlab="",ylab="",zlab="")

