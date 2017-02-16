#a ----
foo <- matrix(1:12, 4, 3)

apply(apply(foo, 1, sort, decreasing = T), 2, prod)
#b ----
matlist <-
      list(matrix(c(T, F, T, T), 2, 2),
           matrix(c("a", "c", "b", "z", "p", "q"), 3, 2),
           matrix(1:8, 2, 4))
lapply(matlist,t)
#c ----
qux<-array(96:1,dim=c(4,4,2,3))

#i
apply(qux[,,2,],3,diag)

#ii
#return the dimensions of each of the three matrices formed by accessing 
#the fourth column of every matrix in qux
qux[, 4, , ]
apply(apply(qux[, 4, , ],3,dim),1,sum)


