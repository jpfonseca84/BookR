dia.url<-"http://www.amstat.org/publications/jse/v9n2/4cdata.txt"
diamonds<-read.table(dia.url)
names(diamonds)<-c("Cara","Color","Clarity","Cert","Price")

#a
dev.new(height=6,width=6)
par(mar=c(0,4,2,0))

#i
boxplot(diamonds$Price~diamonds$Cert,
        xaxt="n",
        yaxt="n",
        frame=F)
title(main="Price by certificate", family="sans")

#ii
axis(side=2,
     at=seq(0,18000,2000),
     tcl=1,
     las=1,
     mgp=c(3,0.5,0))

text(locator(1),labels="SGD$",xpd=NA)
text(locator(1),labels="GIA",xpd=NA,cex=2)
text(locator(1),labels="HRD",xpd=NA,cex=2)
text(locator(1),labels="IGI",xpd=NA,cex=2)

#b
dev.new(height=8,width=7)
par(mar=c(2,5,3,5),oma=c(0,1,1,1))
#i
plot(diamonds$Price~diamonds$Cara,
     col=c("red","green","blue")[as.numeric(diamonds$Cert)],
     xaxt="n",
     yaxt="n",
     ann=F,
     bty="n")
box(bty="u")

#ii
axis(1,
     at=seq(0.2,1.1,0.1),
     family="sans",
     font=3,
     mgp=c(3,0.5,0))
axis(1,
     at=seq(0.15,1.05,0.1),
     mgp=c(3,0.75,0.25),
     tcl=-0.5,
     family="sans",
     font=3,
     ann=F,
     xaxs="i")

#iii
axis(2,
     at=seq(1000,17000,2000),
     las=1,
     family="sans",
     font=3)
USDticks<-seq(1000,11000,1000)
mpgdollar<-USDticks*1.37
axis(4,
     at=mpgdollar,
     labels=USDticks,
     las=1,
     family="sans",
     font=3)

#iv
price.fit<-lm(Price~Cara+I(Cara^2),data=diamonds)
#provide a prediction of the model for a sequence of carat values 
#spanning the range of the observed values
cara.line<-predict(price.fit,
        data.frame(Cara=seq(min(diamonds$Cara),max(diamonds$Cara),by=0.02)),
        interval="prediction")
lines(seq(min(diamonds$Cara),max(diamonds$Cara),by=0.02),
     cara.line[,1],
     type="l",
     col="gray")
lines(seq(min(diamonds$Cara),max(diamonds$Cara),by=0.02),
      cara.line[,2],
      col="black",
      lty=3)
lines(seq(min(diamonds$Cara),max(diamonds$Cara),by=0.02),
      cara.line[,3],
      col="black",
      lty=3)

#v
expr1<-expression(paste("USD$"%~~%1.37%*%"SGD","$"))
expr2<-expression(paste(beta[0]+beta[1],"Carat",+beta[2],"Carat"^2))
mtext(expr1,side=4,line=4)
mtext("SGD$",side=2,line=4)
#vi
interactive.arrow(label=expr2)

#vii
legend(locator(1),
       legend=c("GIA","HRD","IGI"),
       col=c("red","green","blue"),
       pch=1)

legend()
