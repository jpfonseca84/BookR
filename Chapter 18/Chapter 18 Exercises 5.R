#a ----
typeI.mean<-function(mu0,sigma,n,alpha,ITERATIONS=10000,test="less"){
      
      t.stat<-rep(NA,ITERATIONS)
      pvals<-rep(NA,ITERATIONS)
      for(i in 1:ITERATIONS){
            temporary.sample<-rnorm(n=n,mean=mu0,sd=sigma)
            temporary.mean<-mean(temporary.sample)
            temporary.sd<-sd(temporary.sample)
            t.stat[i]<-(temporary.mean-mu0)/(temporary.sd/sqrt(n))
      }
      if(test=="less"){
        pvals<-pt(t.stat,df=n-1)    
      }else 
            if(test=="greater"){
            pvals<-1-pt(t.stat,df=n-1)
      }else 
            if(test=="two.sided"){
            pvals[t.stat>=0]<-2*(1-pt(t.stat[t.stat>=0],df=n-1))
            pvals[t.stat<0]<-2*pt(t.stat[t.stat<0],df=n-1)
            
            }else {
            stop("Not a valid test input, please use \"less\",\"greater\", or 
\"two.sided\"")
      }
      return(mean(pvals<alpha))
}

# i
typeI.mean(0,1,40,0.05,test="less")
typeI.mean(0,1,40,0.05,test="greater")
typeI.mean(0,1,40,0.05,test="two.sided")

#ii
typeI.mean(-4,0.3,60,0.01,test="less")
typeI.mean(-4,0.3,60,0.01,test="greater")
typeI.mean(-4,0.3,60,0.01,test="two.sided")
#b ----

typeII.mean<-function(mu0,muA,sigma,n,alpha,test="two.sided",ITERATIONS=10000){
      test.t<-rep(NA,ITERATIONS)
      for(i in 1:ITERATIONS){
            temporary.sample<-rnorm(n=n,mean=muA,sd=sigma)
            temporary.mean<-mean(temporary.sample)
            temporary.sd<-sd(temporary.sample)
            test.t[i]<-(temporary.mean-mu0)/(temporary.sd/sqrt(n))
      }
      pvals<-pt(test.t,df=n-1)
      if(test=="less"){
            return(mean(pvals>=alpha))
      }else if(test=="greater"){
            return(mean(1-pvals>=alpha))
      }else if(test=="two.sided"){
            result <- pvals
            result[test.t>0] <- 1-pvals[test.t>0]
            return(mean(result>=alpha/2))
      }else {
            stop("argument \"test\" is not valid. Please use \"less\", 
\"greater\" or \"two.sided\"")
      }
}

#i
typeII.mean(-3.2,-3.3,0.1,25,0.05,"two.sided")
#ii
typeII.mean(8994,5600,3888,9,0.01,"less")
#iii
typeII.mean(0.44,0.4,2.4,68,0.05,"greater")
