#a ====
data("quakes")

#ai
write.table(quakes[quakes$mag>=5,1:5],
            "C:\\Users\\jpfon\\Google Drive\\Data Analysis R\\R Book\\Exercises\\Chapter 8\\q5.txt",
            sep = "!",row.names =F)
#aii
q5.dframe<-read.table("C:\\Users\\jpfon\\Google Drive\\Data Analysis R\\R Book\\Exercises\\Chapter 8\\q5.txt")

#b ====
install.packages("car")
library(car)
data("Duncan")

#bi
png("R Book\\Exercises\\Chapter 8\\Graph.png",width = 500,height = 500)

plot(Duncan$education[Duncan$prestige<=80],Duncan$income[Duncan$prestige<=80],xlim = c(0,100),ylim=c(0,100),xlab = "Education",ylab="Income")
points(Duncan$education[Duncan$prestige>80],Duncan$income[Duncan$prestige>80],pch=19,col="blue")
#bii
legend("topleft",legend=c("Prestige<=80","Prestige>80"),col = c("black","blue"),pch = c(1,19))

dev.off()

#c ====
exer<-list(quakes,q5.dframe,Duncan)
#i
dput(exer,file="R Book\\Exercises\\Chapter 8\\Exercise8-1.txt")
#ii
list.of.dataframes<-dget(file="R Book\\Exercises\\Chapter 8\\Exercise8-1.txt")

#d ====
#exercise from section 7.4.3 ====
x<- 1:20
y<- c(-1.49,3.37,2.59,-2.78,-3.94,-0.92,6.43,8.51,3.41,-8.23,-12.01,-6.58,2.87,
      14.12,9.63,-4.58,-14.78,-11.67,1.17,15.62)
ptype<-rep(NA,length(x))
ptype[y>=5]<-"too_big"
ptype[y<=-5]<-"too_small"
ptype[(x>=5&x<=15)&(y>-5&y<5)]<-"sweet"
ptype[(x<5|x>15)&(y>-5&y<5)]<-"standard"
ptype<-factor(x=ptype)
qplot(x,y,shape=ptype,color=ptype)