#B =================================
data<-data.frame("Weight (kg)"=c(55,85,75,42,93,63,58,75,89,67),
                 "Height (kg)"=c(161,185,174,154,188,178,170,167,181,178),
                 "Sex"=factor(c(1,2,2,1,2,2,1,2,2,1),
                              labels = c("female","male"))
)
sex<-data[,3]
#Creating the chart with ggplot2 ====



chart<-qplot(data[,1],data[,2],color=sex,xlab = "Wheight",
             ylab="Height")+
      geom_point(size=4)

print(chart)
