#a
quantile(chickwts$weight,c(0.1,0.3,0.9))

chicksvar<-tapply(chickwts$weight,chickwts$feed,var)
chicksvar[chicksvar==max(chicksvar)]

#b
#iIQR(quakes$depth)

#ii
summary(quakes$mag[quakes$depth<=400])
#comparing it with the summary of quakes with depths> 400. the deeper, the 
#weaker it gets

#iii
depthcat<-cut(quakes$depth,
              breaks=(seq(min(quakes$depth),max(quakes$depth),length=5)),
              right=T,
              include.lowest = T)

#iv
#mean and standard deviation
tapply(quakes$mag,depthcat,mean)
tapply(quakes$mag,depthcat,sd)

#v
tapply(quakes$mag,depthcat,quantile,prob=0.8)

