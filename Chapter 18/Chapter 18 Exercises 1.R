#a ----
#Cat Breed

mu<-3.5

n<-73
x.bar<-3.97
x.sd<-2.21
#significance = 0.05
#H0<- 3.35
#ha != h0

#solution
t<-(x.bar-mu)/(x.sd/sqrt(n))
p<-pt(-t,df=n-1)+(1-pt(t,df=n-1))
# the probability is greater than the alpha (significance value). For this 
# reason, we don't have reasons to believe the H0 is not true.

CI<- x.bar+c(-1,1)*qt(0.975,n-1)*x.sd/sqrt(n)
#the CI contains the H0. reassuring the t-test

#b ----
#mean magnitude of sismic events on fiji
mu<-4.3
# Hypothesis Test is: ha>h0
alpha<-0.01
remove(x.bar)

t.test(quakes$mag,mu=mu,alternative="greater",conf.level=1-alpha)
#p.value is too small. A strong evidence the true magnitude is greater than 4.3

#c ----
n<-length(quakes$mag)
CI<-mean(quakes$mag)+
      c(-1,1)*qt(1-0.01/2,n-1)*
      sd(quakes$mag)/sqrt(n)
#the CI interval does not contain the h0 mean for the true mean value.
