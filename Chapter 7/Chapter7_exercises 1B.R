#B =================================
data<-data.frame("Weight (kg)"=c(55,85,75,42,93,63,58,75,89,67),
                 "Height (kg)"=c(161,185,174,154,188,178,170,167,181,178),
                 "Sex"=factor(c(1,2,2,1,2,2,1,2,2,1),
                              labels = c("female","male"))
)

#Creating the chart ====
#creating the empty chart
plot(data[,1],data[,2],type = "n",xlab="Weight (kg)",ylab="Height (cm)")
#plotting woman
points(data[,1][data[,3]=="female"],data[,2][data[,3]=="female"],
       pch=8,col="pink",lwd=4)
#plotting Man
points(data[,1][data[,3]=="male"],data[,2][data[,3]=="male"],
       pch=8,col="blue",lwd=4)
#Adding the legend
legend("bottomright",legend=c("female","male"),pch=c(8,8),col=c("pink","blue"),
       lwd=4,lty=c(NA,NA))
#Adding the Title
title(main="Weight per Height per sex")