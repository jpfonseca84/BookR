#a ----
#Before comencing, Load exercise 5 typeII.mean

power.mean<-function(nvec,...){
      nlen<-length(nvec)
      result<-rep(NA,nlen)
      pbar<-txtProgressBar(min=0,max=nlen,style=3)
      for(i in 1:nlen){
            result[i]<-1-typeII.mean(n=nvec[i],...)
            setTxtProgressBar(pbar,i)
      }
      close(pbar)
      return(result)
}

#i
sample.sizes=5:100
power.mean(nvec=50,mu0=10,muA=10.5,sigma=0.9,alpha=0.01,test="two.sided")

#ii
power.mean(nvec=44,mu0=80,muA=78.5,sigma=3.1,alpha=0.05,test="two.sided")

power.mean(nvec=44,mu0=80,muA=78.5,sigma=3.1,alpha=0.01,test="two.sided")
#b ----

#Functions to use ----
snacks <- c(87.7,80.01,77.28,78.76,81.52,74.2,80.71,79.5,77.87,81.94,80.7,82.32,
            75.78,80.19,83.91,79.4,77.52,77.62,81.4,74.89,82.95,73.59,77.92,
            77.18,79.83,81.23,79.28,78.44,79.01,80.47,76.23,78.89,77.14,69.94,
            78.54,79.7,82.45,77.29,75.52,77.21,75.99,81.94,80.41,77.7)

sample.sizes<- 5:100

pow1 <- power.mean(
      nvec = sample.sizes,
      mu0 = 80,
      muA = 78.5,
      sigma = 3.1,
      alpha = 0.05,
      ITERATIONS = 5000,
      test = "less"
)

pow2 <- power.mean(
      nvec = sample.sizes,
      mu0 = 80,
      muA = 78.5,
      sigma = 3.1,
      alpha = 0.01,
      ITERATIONS = 5000,
      test = "less"
)
min.1<-sample.sizes[min(which(pow1>0.8))]
min.2<-sample.sizes[min(which(pow2>0.8))]

plot(sample.sizes,pow1,main="minimum sample sizes per alpha",xlab="Sample Size",
     ylab="Simulated Power")+
      points(pow2,col="lightblue")+
      abline(h=0.8,lty=3)+
      abline(v=min.1,lty=4)+
      abline(v=min.2,lty=4,col="lightblue")+
      legend("bottomright",
             legend=c("alpha=0.05","alpha=0.01"),
             fill=c("black","lightblue"))
