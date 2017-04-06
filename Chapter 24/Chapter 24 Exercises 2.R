#--------------text----
install.packages("ggvis")
library("ggvis")
surv<-na.omit(survey[,c("Sex","Wr.Hnd","Height","Smoke","Exer")])
head(survey)
surv%>%ggvis(x=~Height)%>%layer_histograms()
surv%>%
      ggvis(x=~Height)%>%
      layer_histograms(width=input_slider(1,15,
                                          label="Binwidth:"),
                       fill:="gray")

surv%>%
      ggvis(x=~Wr.Hnd,y=~Height,size:=200,opacity:=0.3)%>%
      layer_points()

filler<-input_radiobuttons(c("Sex"="Sex",
                             "Smoking status"="Smoke",
                             "Exercice frequency"="Exer"),
                           map=as.name,
                           label="Color points by...")

sizer<-input_slider(10,300,label="Point size:")

opacityer<-input_slider(0.1,1,label="Opacty:")

surv %>%
      ggvis(x =  ~ Wr.Hnd,
            y =  ~ Height,
            fill = filler,
            size := sizer,
            opacity := opacityer) %>%
      layer_points() %>%
      add_axis("x",
               title="Handspawn") %>%
      add_legend("fill",title="")

surv%>%
      ggvis(x=~Wr.Hnd,y=~Height,fill=~Sex)%>%
      group_by(Sex)%>%
      layer_smooths(span = input_slider(0.3, 1,
                                        value = 0.75,
                                        label = "Smoothing span:"),
                    se = TRUE) %>%
      layer_points()%>%
      add_axis("x",title="Handspan")

#---------------------Exercises----

library("car")
library("ggvis")

?Salaries

#a
salfill<-input_radiobuttons(c("Rank"="rank",
                                  "Discipline"="discipline",
                                  "Sex"="sex"),
                                map=as.name,
                                label="Color points by ...")
Salaries %>%
      ggvis(x =  ~ yrs.service,
            y =  ~ salary,
            fill = salfill) %>%
      layer_points() %>%
      add_legend("fill", title = "") %>%
      add_axis("x", title = "Years of Service") %>%
      add_axis("y", title = "Salary")

#b
#i
Salaries %>%
      ggvis(x=~salary,
            fill=~rank) %>%
      group_by(rank) %>%
      layer_densities()
#ii
Salaries %>%
      ggvis(x=~salary,
            fill=~rank) %>%
      group_by(rank) %>%
      layer_densities(adjust=input_slider(0.2,
                                          2,
                                          label="Smoothnes")) %>%
      add_axis("x",title="Salary",ticks=5) %>%
      add_axis("y",title="Kernel Density",ticks=5,layer="front")

#C
?UScereal
filler <- input_radiobuttons(
      c("Manufacturer" = "mfr",
            "Shelf" = "shelf",
            "Vitamins" = "vitamins"),
      map = as.name,
      label = "Color options")

#d
sizer<-input_slider(10,300,label="Point size:")
opacityer<-input_slider(0.1,1,label="Opacty:")

UScereal %>% ggvis(x=~calories,
                   y=~protein,
                   fill=filler,
                   size:=sizer,
                   opacity:=opacityer) %>%
      add_legend("fill",title="")%>%
      layer_points()

#e
shaper <- input_radiobuttons(
      c("Manufacturer" = "mfr",
        "Shelf" = "shelf",
        "Vitamins" = "vitamins"),
      map = as.name,
      label = "shape options")

#f
UScereal %>% ggvis(
      x =  ~ calories,
      y =  ~ protein,
      fill = filler,
      size := sizer,
      opacity := opacityer,
      shape = shaper) %>%
      layer_points() %>%
      add_legend("fill", title = "") %>%
      add_legend("shape",
                 title = "",
                 properties = legend_props(legend = list(y = 100))) %>%
      set_options(duration=0)
