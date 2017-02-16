#4.1
#a
a<- c(6,9,7,3,6,7,9,6,3,6,6,7,1,9,1)
#i
a==6
#ii
a>=6
#iii
a<6+2
#iv
a!=6

#b
b<-array(b,c(2,2,3))
#i
b1<-b==6/2+4
#ii
b2<-b+2<=6/2+4

#c
c<-diag(,10)==0
c==0

#d
d1<-any(b1)
d2<-all(b1)
d3<-any(b2)
d4<-all(b2)
c(d1,d2,d3,d4)

#e
e<-any(diag(c))
