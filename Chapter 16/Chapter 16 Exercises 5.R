#a ----
lambda<-3500

#i
lambda.day<-lambda/365.25

#ii
xval<-seq(0,1,length.out=100)
plot(xval,dexp(xval,lambda.day),main="Density function",xlab="x",ylab="f(x)",type = "l")
abline(h=0,lty=3)
abline(v=0,lty=3)

#iii
pexp(30,lambda.day/24/60)

#iv

qexp(.9,lambda.day)*24
.
#b ----
# average life 11 years
#X is the time before maintenance is needed
mu<-11
lambda <- 1/mu

#i
pexp(5,lambda)

#ii

c.mu<-9
c.lambda<-1/c.mu

pexp(6,c.lambda)

#iii
#our product
1-pexp(15,lambda)
#their product
1-pexp(15,c.lambda)
