#hight of a certain Tree is uniformly distributed between 3 and 70 feet
a<-3
b<-70
#a tree shorter than 5.5 feet
punif(5.5, min=a,max=b)

#b Cutoff points of tallest 15 percent of trees.
qunif((1-0.15),min=a,max=b)

#c
mean<- (a+b)/2
sd<- sqrt(((b-a)^2)/12) 

#d
mark1<-mean-sd/2
mark2<-mean+sd/2
sum(punif(mark2,a,b)-punif(mark1,a,b))

#e
barplot(dunif(3:70,min=a,max=b),
        space=0)
max(dunif(3:70,min=a,max=b))

#f
simulation<-runif(10,a,b)
quantile(simulation,0.85)

Simulation<-runif(1000,a,b)
quantile(Simulation,0.85)
#as we have more variables, the test value aproximates itself 
#from the true value
