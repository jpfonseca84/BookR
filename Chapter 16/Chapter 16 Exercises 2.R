#mean number of cars passing in 120 min is 107

#a ----
1-ppois(100,107)

#b
dpois(0,107)

#c
barplot(
      dpois(60:150, 107),
      main = "Number of cars distribution",
      xlab = "x cars",
      ylab = "Pr(X = x)",
      names.arg = seq(60,150,1),
      space=0
)
#d

hist(rpois(260,107),
     xlim = c(60,150))
