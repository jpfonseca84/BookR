#a ----
hist(InsectSprays$count,xlab = "Count",right=T)

#b ----
sumofinsects<-tapply(InsectSprays$count,InsectSprays$spray,sum)

barplot(sumofinsects,
        names.arg = levels(InsectSprays$spray),
        xlab="Spray",
        ylab = "Total of insects")
pie(sumofinsects,
    labels = levels(InsectSprays$spray),
    main = "Total of mosquitoes per spray")

#c ----

sort.fac <- factor(InsectSprays$spray)

ggpairs(InsectSprays,
        mapping=aes(col=spray),
        xlab=("Spray"),
        ylab=("Frequency"),
        axisLabels = "internal")

#d ----
USArrests[1:5,]

qplot(USArrests$UrbanPop,
      geom = "blank",
      main="Urban Population",
      xlab="Urban Population")+
      geom_histogram(color="black",
                     fill="blue",
                     breaks=seq(0,100,10),
                     closed="right")+
      geom_vline(mapping=aes(xintercept=c(quantile(USArrests$UrbanPop,0.25),
                                          quantile(USArrests$UrbanPop,0.50),
                                          quantile(USArrests$UrbanPop,0.75))),
                 show.legend = T)+
      scale_linetype_manual(values=c(2,3))+
      labs(linetype="")

#e ----
crimesperstate<-t(as.matrix(USArrests[,-3]))

barplot(crimesperstate,
        names.arg = state.abb,
        main="Types of crimes per state",
        legend.text = rownames(crimesperstate))

#f ----
urbancat <- rep(NA, length(USArrests$UrbanPop))
urbanmedian <- median(USArrests$UrbanPop)
for (i in 1:length(USArrests$UrbanPop)) {
      if (USArrests$UrbanPop[i] > urbanmedian) {
            urbancat[i] <- 1
      } else{
            urbancat[i] <- 0
      }
}
urbancat <- factor(urbancat)

#g ----
myusarrests <- USArrests[-3]
myusarrests$urbancat <- urbancat

#h ----
ggpairs(myusarrests,
        mapping = aes(col = urbancat),
        axisLabels = "internal")

#i ----
qquantiles<-quantile(quakes$mag,c(1/3,2/3))

magvec<-cut(quakes$mag,
            breaks=c(min(quakes$mag),
                     qquantiles[1],
                     qquantiles[2],
                     max(quakes$mag)))

#j ----
plot(
      quakes$long,
      quakes$lat,
      pch = (1:3)[magvec],
      col = (1:3)[magvec],
      main = "Latitude and longitude",
      ylab = "Latitude",
      xlab = "Longitude"
)

#k ----
legend(
      "bottomleft",
      legend = c("0<x<1/3", "1/3<x<2/3", "2/3<x<1"),
      col = 1:3,
      pch = 1:3,
      title="magnitude quantiles"
)