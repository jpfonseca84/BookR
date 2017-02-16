#a ----
weight<-c(55,85,75,42,93,63,58,75,89,67)
height<-c(161,185,174,154,188,178,170,167,181,178)
Sex<-c("f","m","m","f","m","m","f","m","m","f")

cor(weight,height)

#b ----
mtcars[1:5,]
#i 
?mtcars

#ii
thecor<-cor(mtcars[,4],mtcars[,7])
plot(mtcars[,4],mtcars[,7],xlab="Horsepower",ylab="1/4 mile time")
text(300,20,labels=c("correlation is\n\n", round(thecor,2)))

#iii
tranfac<-factor(mtcars[,9],labels=c("auto","manual"))

#iv
theplot<-qplot(mtcars[,4],mtcars[,7],
               main="The Plot",
               xlab="Horsepower",
               ylab="1/4 mile time",
               color=tranfac,
               shape=tranfac)

#v
autoflag<-mtcars[,9]==0
manualcor<-round(cor(mtcars[,4][autoflag],mtcars[,7][autoflag]),4)
autocor<-round(cor(mtcars[,4][!autoflag],mtcars[,7][!autoflag]),4)
#Separeted by transmission, the negative correlation gets stronger

#c ---- 


#i 
sunchicks<-chickwts$weight[chickwts$feed == "sunflower"]
plot(
      x = sunchicks,
      y = rep(0, length(sunchicks)),
      xlab = "weight",
      xlim=c(min(sunchicks),
             max(sunchicks)),
      ylab = "sunflower chick weights",
      yaxt = "n",
      bty = "n",
      cex.axis=1.5,
      cex.lab=1.5)
abline(h=0,lty=2)

#ii
sd(sunchicks)
#[1] 48.83638

IQR(sunchicks)
#[1] 27.5

#iii
sunchicks2<-sunchicks[-6]

plot(
      x = sunchicks2,
      y = rep(0, length(sunchicks2)),
      xlab = "weight",
      xlim=c(min(sunchicks2),
             max(sunchicks2)),
      ylab = "",
      yaxt = "n",
      bty = "n",
      cex.axis=1.5,
      cex.lab=1.5)
abline(h=0,lty=2)

sd(sunchicks2)
#[1] 38.31473

IQR(sunchicks2)
#[1] 21.5
