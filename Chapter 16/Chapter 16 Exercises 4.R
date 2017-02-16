#a ----
mu<-17
sd<-4.5

#i
1-pnorm(20,mu,sd)

#ii
a<-5
b<-10
pnorm(b,mu,sd)-pnorm(a,mu,sd)

#iii
qnorm(.9,mu,sd)


#iv
#values
xval <- seq(mu - 4 * sd, mu + 4 * sd, length.out = 500)
fx <- dnorm(xval, mu, sd)
#plotting
plot(
      xval,
      fx,
      typ = "l",
      ylab = "f(x)",
      xlab = "x",
      main = "Normal distribution of x"
)
abline(h=0,lty=3)

#painting

fxpoints <- fx[(xval >= qnorm(.9, mu, sd))]
xvalpoints <- xval[(xval >= qnorm(.9, mu, sd))]

polygon(rbind(
      cbind(xvalpoints[1], 0),
      cbind(xvalpoints, fxpoints),
      cbind(xvalpoints[length(xvalpoints)], 0)),
      col = "gray"
)

#v
hist(rnorm(10,mu,sd))

#b ----
mu=10
var=2
sigma=sqrt(var)

#i
pnorm(11,10,2)-pnorm(9.5,10,2)

#ii
dnorm(c(9.5,11),10,2)
stan9.5 <- (9.5 - mu) / sigma
stan11 <- (11 - mu) / sigma

pnorm(c(stan9.5,stan11),0,1)
pnorm(c(9.5,11),mu,sigma)

#iii
shortest<-qnorm(0.025,mu,sigma)
standshort<-(shortest-mu)/sigma
pnorm(standshort)
