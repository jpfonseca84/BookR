z.test <- function(p1,
                   n1,
                   p2 = NULL,
                   n2 = NULL,
                   p0,
                   alternative = "two.sided",
                   conf.level = 0.95) {
      
      if(is.null(p2)|is.null(n2)){
            cat("One sample Z-Test\n")
            if(p1*n1||n1*(1-p1)){
                  warning("normal distribution may not be valid")
            }
            z<-(p1-p0)/sqrt(p0*(1-p0)/n1)
            CI<-(p1)+c(-1,1)*qnorm(1-conf.level/2)*sqrt(p0*(1-p0)/n1)
      }else{
            cat("two-sample test")
            if(p1*n1<5||n1*(1-p1)<5||n2*p2<5||n2*(1-p2)<5){
                  warning("Normal distribution might not be accurate as rule 
                          of thumb")
            }
            p.star(((n1*p1)+(n2*p2))/n1+n2)
            z<-(p1-p2-p0)/sqrt(p.star*(1-p.star)*(1/n1+1/n2))
            CI<-(p1-p1)+c(-1,1)*qnorm(1-conf.level/2)*
                  sqrt(p.star*(1-p.star)*(1/n1+1/n2))
      }
      #defining p-value
      p<-pnorm(z)
      if(alternative=="greater"){
            p<-1-p
      }
      if(alternative=="two.sided"){
            if(z>0){
                  p<-2*(1-p)
            }else{
                  p<-2*p
            }
      }
      return(list(Z=z,P=p,CI=CI))
}

sick <- c(0,0,1,1,0,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,1,1,0,0,0,1)
n.sick<-length(sick)
p.sick<-sum(sick)/n.sick
