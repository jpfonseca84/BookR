#a

#i
foo<-array(data=1:36,dim=c(3,3,4))
#ii
bar<-as.vector(foo)
#iii
baz<-as.character(bar)
#iv
qux<-as.factor(baz)
#v
quux<-bar+c(-0.1,0.1)

#b
bfoo<-is.numeric(foo)+is.integer(foo)
bbar<-is.numeric(bar)+is.integer(bar)
bbaz<-is.numeric(baz)+is.integer(baz)
bqux<-is.numeric(qux)+is.integer(qux)
bquux<-is.numeric(quux)+is.integer(quux)

b<-factor(c(bfoo,bbar,bbaz,bqux,bquux),levels = c(0,1,2))

#c
c<-matrix(c(2,3,4,5,6,7,8,9,10,11,12,13),3,4)
c1<-as.character(as.vector(t(c)))

#d
d<-matrix(c(34,23,33,42,41,0,1,1,0,0,1,2,1,1,2),5,3)
di<-as.data.frame(d)
di[,2]<-as.logical(di[,2])
di[,3]<-as.factor(di[,3])