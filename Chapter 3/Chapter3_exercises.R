

#Exercise 3.3
a<-array(data= seq(from=4.8,to=0.1,length.out = 4*2*6),dim = c(4,2,6))

b<-a[c(4,1),2,]

c<-array(rep(b[2,],times=4),dim=c(2,2,2,3))

d<-a[,,-6]

e<-d

e[c(2,4),2,c(1,3,5)]<--99
