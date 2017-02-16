#a
seq(-4,4,2)

#########
## 9.2 ##
#########
#(a)
seq(-4,4,0.2)
#(b)
##(i)
array(8:1,dim=c(2,2,2)) # MIXED: 'data' positional, 'dim' exact.
##(ii)
rep(1:2,3) # POSITIONAL
##(iii)
seq(from=10,to=8,length=5) # MIXED: 'from' and 'to' exact, 'length.out' partial.
##(iv)
sort(decreasing=T,x=c(2,1,1,2,0.3,3,1.3)) # EXACT
##(v)
which(matrix(c(T,F,T,T),2,2)) # POSITIONAL
##(vi)
which(matrix(c(T,F,T,T),2,2),a=T) # MIXED: 'x', 'data', 'nrow', 'ncol' positional, 'arr.ind' partial
#(c) used args(plot.default) the args below were not listed.
# 'pch', 'lwd', 'lty' and 'col' are part of the ellipsis.
