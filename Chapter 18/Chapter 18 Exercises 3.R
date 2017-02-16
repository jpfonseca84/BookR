#First part ####

#a
#Hypotheses: H0: p=0.9 // HA: p<0.9
p<-0.9
n<-89
x<-71
p.hat<-x/n
n*p.hat>5&n*(1-p.hat)>5
#as both are true, we can accept the to carry out with a normal distribution

#b
z.test<-(p.hat-p)/(sqrt(p*(1-p)/n))
pnorm(z.test)
#the P-value is too small. with 99% of confidence, there is  enought statistical
#evidenve to reject H0 in favor of HA.

#c CI
p.hat+c(-1,1)*qnorm(0.99)*sqrt(p*(1-p)/n)
#this result corroborates the answer in "B" as it excludes the claimmed mean of 
#0.9
#Second Part ####

x1<-97
n1<-445
x2<-90
n2<-419

p.hat1<-x1/n1
p.hat2<-x2/n2

#d
alpha<-0.05
#Hypotheses:: H0: p2-p1 =0 // HA:p2-p1 != 0
prop.test(c(x2,x1),
          c(n2,n1),
          alternative = "two.sided",
          conf.level = 1-alpha,
          correct = FALSE)
#the P-value is a big one. There is not enought statistical evidence that 
#the probability is different in each country. Pooled Variance
p<-(x1+x2)/(n1+n2)
#testing with the manual calculation
z<-(p.hat2-p.hat1)/(sqrt(p*(1-p)*(1/n1+1/n2)))
pval<-2*pnorm(z)

#e to create a Confidence Interval
# statistic+critical Value*SE
se<-sqrt(p*(1-p)*(1/n1+1/n2))
(p.hat2-p.hat1)+c(-1,1)*qnorm(1-alpha/2)*se
#the CI includes the value 0 reassuring that both states might have the same 
# proportion.
#Third Part ####
#f
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

