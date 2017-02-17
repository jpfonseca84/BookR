#a
foo<-c(7,5,6,1,2,10,8,3,8,2)
#i
bar<-foo[foo>=5]
#ii
foo[foo<5]

#b
baz<-matrix(bar,2,3,byrow = T)
#i
baz[baz==8]<-baz[1,2]^2
#ii
baz<=25&baz>4

#c
qux<-array(c(10,5,1,4,7,4,3,3,1,3,4,3,1,7,8,3,7,3),c(3,2,3))
#i
which(qux==3|qux==4,arr.ind = T)
#ii
qux[qux<3|qux>=7]<-100

#d
