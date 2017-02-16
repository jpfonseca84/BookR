#a ----
data(HairEyeColor)
total.hec<-HairEyeColor[,,1]+HairEyeColor[,,2]

chisq.test(total.hec)
#H0:: Variables A and B are Independent // Ha: Variables are not independent
# We have a very small p.number. A strong statistical evidence to reject H0

#b ----
library("car")
Duncan[,1]

c.prof<-length(Duncan[Duncan[,1]=="prof",1])
c.bc<-length(Duncan[Duncan[,1]=="bc",1])
c.wc<-length(Duncan[Duncan[,1]=="wc",1])


Duncan.tab<-table(Duncan[,1])

#Hypotheses: H0: p1=p2=p3=1 //Ha: h0 is incorrect

chisq.test(Duncan.tab)

#the P-value is lesser than the alpha=0.05, there is enough evidence to believe 
# the Variables are dependent, following Ha

#B
#with alpha=0.01, the p.value becomes greater than it, we tend with H0, the 
#variables are independent. 