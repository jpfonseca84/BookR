#First Exercise ----
#max points is 65
mean <- 41.1
sd <- 11.3
n <- 6
#a
se <- sd / sqrt(n)

#b
pnorm(55, mean, se) - pnorm(45, mean, se)

#c
pnorm(65 / 2, mean, se)

#Second Exercise ----
n <- 140
p <- 0.35 #probability of A being choosed

#d
p * n >= 5 & n * (1 - p) >= 5

#e
se <- sqrt(p * (1 - p) / n)   # to be able to use it in the normal formula for
                              # sample below
1 - pnorm(0.4, p, se)

#f
upper<-qnorm(0.9,p,se)
lower<-qnorm(0.1,p,se)

#Third Exercise ----
n<-63
xmean<-37.8
s<-34.51


#g
# This is a normal curve as it is regarding a mean of samples.
se <- s/sqrt(n)
df <- n - 1

#i but as it is regarding a sample, it works as a normal.
#all 

newx <- (40 - xmean) / se
1 - pt(newx, df)

#ii
newx <- (30 - xmean) / se
pt(newx, df)

#iii
newx <- (40 - xmean) / se
pt(newx,df)-0.5
