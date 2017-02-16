# 13 bird-viewing plataforms
# 75% chance of seing birds in each platform
# a ----
# Probability mass function of the binomial distribution
# X~BIN(13,3/4)
# f(x)= (13!/(13!*(13-13)!))*(0.75^13*(1-0.75)^(13-13)
#or
value<-1
platform<-13
prob<-3/4
#a
dbinom(x=value,size=platform,prob=prob)

#b
value=13
dbinom(x=value,size=platform,prob=prob)

#c
value=9
1-pbinom(q=value,size=platform,prob=prob)

#d
sum(dbinom(x=1:3,size=platform,prob=prob))
pbinom(q=3,size=platform,prob=prob)

#e
pbinom(q=8,13,3/4)

#f
xrealizations<-rbinom(10,13,3/4)
mean(xrealizations)
sd(xrealizations)
