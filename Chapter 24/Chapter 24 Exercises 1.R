# Chapter examples: ----
      library("ggplot2")
gg.static <- ggplot(data = mtcars,
                    mapping = aes(x = hp)) +
      ggtitle("horsepower") +
      labs(x = "Hp")

mtcars.mm <- data.frame(mm = c(mean(mtcars$hp),
                               median(mtcars$hp)),
                        stats = factor(c("mean",
                                         "Median")))

gg.lines <- geom_vline(
      mapping = aes(xintercept = mm,
                    linetype = stats),
      show.legend = T,
      data = mtcars.mm
)

gg.static +
      geom_histogram(
            color = "black",
            fill = "white",
            breaks = seq(0, 400, 25),
            closed = "right"
      ) +
      gg.lines +
      scale_linetype_manual(values = c(2, 3)) +
      labs(linetype = "")


#----------------------page 611
library("MASS")
surv<-na.omit(survey[,c("Sex","Wr.Hnd","Height")])

ggplot(surv,aes(x=Wr.Hnd,
                y=Height,
                col=Sex,
                pch=Sex))+
      geom_point()+
      geom_smooth(method = "loess",
                  span=0.75)

#----------------------page 614
?airquality
ggplot(data=airquality,aes(x=Temp))+
      geom_density()
air<-airquality
air$Month<-factor(air$Month,labels = c("May","June","July","August",
                                       "September"))
ggplot(data = air,
       aes(x = Temp, fill = Month)) +
      geom_density(alpha = 0.5) +
      ggtitle("Monthly temperature probability densities") +
      labs(x = "Temp (F)", y = "Kernel estimae")

#-------------------page 616
install.packages("gridExtra")
library("gridExtra")

gg1 <- ggplot(air, aes(x = 1:nrow(air), y = Temp)) +
      geom_line(aes(col = Month)) +
      geom_point(aes(col = Month, size = Wind)) +
      geom_smooth(method = "loess", col = "Black") +
      labs(x = "Time (days)", y = "Temperature(F)")

gg2 <- ggplot(air, aes(x = Solar.R, fill = Month)) +
      geom_density(alpha = 0.4) +
      labs(x = expression(paste("Solar radiation (", ring(A), ")")),
           y = "Kernel estimate")

gg3 <- ggplot(air, aes(x = Wind, y = Temp, color = Month)) +
      geom_point(aes(size = Ozone)) +
      geom_smooth(
            method = "lm",
            level = 0.9,
            fullrange = FALSE,
            alpha = 0.2) +
      labs(x = "Wind speed (MPH)",
           y = "Temperature (F)")

grid.arrange(gg1, gg2, gg3)

#--------------page 619

ggp<-ggplot(data=air,aes(x=Temp,fill=Month))+
      geom_density(alpha=0.4)+
      ggtitle("Monthly temperature probability densities")+
      labs(x="Temp (F)",y="Kernel estimate")

ggp+facet_wrap(~Month)
#--------------page 620
library("faraway")

diab<-na.omit(diabetes[,c("chol",
                           "weight",
                           "gender",
                           "frame",
                           "age",
                           "height",
                           "location")])

ggplot(diab,aes(x=age,y=chol))+
      geom_point(aes(shape=location,size=weight,col=height))+
      facet_grid(gender~frame)+
      geom_smooth(method="lm")+
      labs(y="cholesterol")
                  
# EXERCISES ----
library("MASS")
#a
cereal<-UScereal
cereal$mfr
levels(cereal$mfr)<-factor(c("General Mills","Kellogs",rep("Other",4)))
cereal$shelf<-factor(cereal$shelf)

#b
#i
gg1 <-ggplot(data = cereal,
              aes(x = protein,
                  y = calories,
                  col = shelf))+
      geom_point(aes(shape=mfr))+
      geom_smooth(method="lm",
                  aes(col=shelf))+
      labs(x="Protein",
           y="Calories",
           col="Shelf",
           shape="manufacturer")+
      ggtitle("Calories x Protein scatterplot")
gg1
#ii
gg2<-ggplot(data=cereal,
            aes(x=calories,
                fill=shelf))+
      geom_density(alpha=0.5)+
      labs(x="Calories",
           y="Kernel estimate",
           fill="Shelf")+
      ggtitle("Kernel estimates of Calories")

#c
grid.arrange(gg1,gg2)

#d
ggplot(data = cereal,
       aes(x = protein,
           y = calories)) +
      geom_point(aes(col = sugars,
                     cex = sodium,
                     shape = shelf)) +
      geom_smooth(method="loess") +
      facet_wrap( ~ mfr)+
      ggtitle("Calories x Protein")


#e
library("car")
Salaries
gg1<-ggplot(Salaries,
            aes(x=yrs.service,
                y=salary,
                col=sex,
                pch=sex))+
      geom_point()+
      geom_smooth(method="loess")+
      labs(x="Years of Service",y="Salary")+
      ggtitle("Smooth Salary by years of service by sex")
gg1

#f
#i
gg2<-ggplot(Salaries,aes(y=salary,
                    x=rank,
                    fill=sex))+
      geom_boxplot()+
      ggtitle("Salary by rank by Sex")+
labs(x="Rank",y="Salary")
gg2
#ii
gg3<-ggplot(Salaries,aes(y=salary,x=discipline,fill=sex))+
      geom_boxplot()+
      ggtitle("Salary by discipline by sex")+
      labs(x="Discipline",y="Salary")
gg3
#iii
gg4<-ggplot(Salaries,aes(x=salary,fill=rank))+
      geom_density(alpha=0.3)+
      facet_wrap(~sex)
gg4

#g
grid.arrange(gg1,gg2,gg3,gg4)

#h
#i
ggplot(Salaries,aes(x=salary,fill=sex))+
      geom_density(alpha=.7)+
      facet_wrap(~rank)
#ii 
ggplot(Salaries,aes(y=salary,
                    x=yrs.service,
                    fill=sex))+
      geom_point(mapping=aes(col=sex))+
      facet_grid(discipline~rank,scales="free_x")+
      geom_smooth(method="lm")+
      labs(x="Years of Service",y="Salary")+
      ggtitle("Scatterplots of Salary on years of Service")
