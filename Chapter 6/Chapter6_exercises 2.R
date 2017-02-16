#a
foo<-c(4.3,2.2,NULL,2.4,NaN,3.3,3.1,NULL,3.4,NA)
# i is true
length(foo)
#ii is False
which(is.na(foo))
#iii is false
is.null(foo)
#iv is false
is.na(foo[8])+4/NULL

#b
b<-list(c(7,7,NA,3,NA,1,1,5,NA))
#i
names(b)<-"alpha"
#ii
is.null(b$beta)
#iii
b$beta<-which(is.na(b$alpha))