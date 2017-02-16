#First Part ----
n=34
x.bar<-14.22
sigma<-2.9

#a construct and interpret a 90 percent confidence interval for the true mean time
alpha<-1-.9

x.bar+c(-1,1)*qnorm(1-alpha/2)*(sigma/sqrt(n))

#b Repeat A, but no Sigma, only s=2.9
s<-2.9
x.bar+c(-1,1)*qt(1-alpha/2,df=n-1)*(s/sqrt(n))

# Second Part ----
n<-400
# Option A is right-handed
# Option B is left-handed
# Option C is ambidextrous

p.b<-37/n
p.c<-11/n
p.a<-1-p.b-p.c

#c Calculate a 99% CI for the true proportion of citzend who are B
alpha<-1-0.99
p.b+
      (c(-1,1)*qnorm(1-alpha/2))*
      sqrt(p.b*(1-p.b)/n)

#d 99% confidence in CI of true proportion citzens left or ambidextrous

(p.b + p.c)+
      c(-1, 1) * qnorm(1 - alpha / 2) *
      (sqrt((p.b + p.c) * (1 - (p.b + p.c)) / n))

#Third Part ----
#e

x.mat<-matrix(NA,5000,3)
n<-300
lambda<-0.1
mu<-1/lambda

for(i in 1:nrow(x.mat)) {
      
      sample <- rexp(n, lambda)

      limits <- mean(sample) +
            c(-1, 1) * qt(1-0.05/2, df= n-1) *
            (sd(sample) / sqrt(n))
      x.mat[i, 1:2] <- limits
      x.mat[i,3]<- mu>=limits[1]&mu<=limits[2] #This returns a T F vector

}

mean(x.mat[,3])
cat(tru.prop)

#Forth Part ----
#plot the 100 first CI with the average in the middle

xaxis<-seq(mu-3,mu+3,length=100)
yaxis<-1:100

plot(xaxis, yaxis, type = "n") +
      abline(v = 10, lty = 2) +
      for (i in 1:length(xaxis)) {
            lines(c(x.mat[i, 1], x.mat[i, 2]),
                  c(i, i),
                  col = if (x.mat[i, 1] < 10 & x.mat[i, 2] > 10) {
                        "blue"
                  } else{
                        "red"
                  })
            
            
            
      }
dev.off()            


            
            
            
            
            
            
            
            