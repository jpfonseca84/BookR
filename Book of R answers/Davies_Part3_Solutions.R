

######################################
#### SUGGESTED EXERCISE SOLUTIONS ####
######################################

##########
## 13.1 ##
##########
#(a)
##(i) Numeric-discrete
##(ii) Categorical-ordinal
##(iii) Numeric-continuous
##(iv) Categorical-ordinal
##(v) Categorical-nominal
##(vi) Numeric-continuous
#(b)
##(i) Sample statistic. Population parameter is the proportion of NZers who own a gaming console.
##(ii) Sample statistic. Population parameter is the average number of blemishes on the hoods of all cars at No Dodgy Carz.
##(iii) Population parameter.
##(iv) Population parameter.
##(v) Sample statistic. Population parameter is (iv).

##########
## 13.2 ##
##########
#(a)
round(mean(quakes$depth >= 300), 2)
#(b)
mean(quakes$mag[quakes$depth >= 300])
median(quakes$mag[quakes$depth >= 300])
#(c)
for (i in levels(chickwts$feed)) {
      cat(i, ": ", round(mean(chickwts$weight[chickwts$feed == i]), 1), " grams\n", sep =
                "")
}
#(d) count: numeric-discrete; spray: categorical-nominal
#(e)
Ctab <- table(InsectSprays$count)
Ctab[Ctab == max(Ctab)]
#(f)
tapply(InsectSprays$count, INDEX = InsectSprays$spray, FUN = sum)
#(g)
for (i in levels(InsectSprays$spray)) {
      cat("Spray ",
          i,
          "; at least 5 bugs: ",
          round(mean(InsectSprays$count[InsectSprays$spray == i] >= 5) * 100, 0),
          "% \n",
          sep = "")
}
#(h)
tapply(
      InsectSprays$count,
      INDEX = InsectSprays$spray,
      FUN = function(x)
            round(mean(x >= 5) * 100, 0)
)


##########
## 13.3 ##
##########
#(a)
quantile(chickwts$weight, c(0.1, 0.3, 0.9))
chickvars <- tapply(chickwts$weight, INDEX = chickwts$feed, FUN = var)
chickvars[chickvars == max(chickvars)]
#(b)
##(i)
IQR(quakes$depth)
##(ii)
summary(quakes$mag[quakes$depth >= 400]) ### Magnitudes at <400 have a similar amount of spread as those at >=400 (and an equivalent IQR of 0.5), but are centered at slightly higher values.
##(iii)
dmin <- min(quakes$depth)
dmax <- max(quakes$depth)
depthcat <-
      cut(
            quakes$depth,
            breaks = seq(dmin, dmax, length = 5),
            include.lowest = TRUE,
            right = FALSE
      )
levels(depthcat)
##(iv)
tapply(quakes$mag, INDEX = depthcat, FUN = mean)
tapply(quakes$mag, INDEX = depthcat, FUN = sd)
##(v)
tapply(quakes$mag,
       INDEX = depthcat,
       FUN = quantile,
       prob = 0.8)


##########
## 13.4 ##
##########
#(a)
w <- c(55, 85, 75, 42, 93, 63, 58, 75, 89, 67)
h <- c(161, 185, 174, 154, 188, 178, 170, 167, 181, 178)
cor(w, h)
#(b)
##(i)
mtcars
?mtcars
##(ii)
plot(mtcars$hp, mtcars$qsec, xlab = "horsepower", ylab = "1/4 mile time")
cor(mtcars$hp, mtcars$qsec)
##(iii)
tran <- factor(mtcars$am, labels = c("auto", "manual"))
##(iv)
library(ggplot2)
qplot(mtcars$hp,
      mtcars$qsec,
      col = tran,
      xlab = "horsepower",
      ylab = "1/4 mile time")
##(v)
cor(mtcars$hp[tran == "manual"], mtcars$qsec[tran == "manual"])
cor(mtcars$hp[tran == "auto"], mtcars$qsec[tran == "auto"]) ### Separated by transmission type, correlations are more extreme than when pooled. Manual cars have a slightly more extreme negative correlation between the two variables than automatic cars.
#(c)
##(i)
sunchicks <- chickwts$weight[chickwts$feed == "sunflower"]
plot(
      sunchicks,
      rep(0, length(sunchicks)),
      yaxt = "n",
      bty = "n",
      xlab = "sunflower chick weights",
      ylab = ""
)
abline(h = 0, col = "gray", lty = 2)
##(ii)
sd(sunchicks)
IQR(sunchicks)
##(iii)
sd(sunchicks[-which(sunchicks == min(sunchicks))])
IQR(sunchicks[-which(sunchicks == min(sunchicks))]) ### Both measures of spread decrease with the deletion of the lowest weight; though the sd is affected more than the IQR...

##########
## 14.1 ##
##########
library("ggplot2")
library("GGally")
#(a)
hist(InsectSprays$count)
#(b)
inspray <- levels(InsectSprays$spray)
incount <-
      tapply(InsectSprays$count, INDEX = InsectSprays$spray, FUN = sum)
barplot(
      incount,
      names.arg = inspray,
      xlab = "Spray",
      ylab = "Total insects",
      main = "Barplot of number of insects\naccording to spray type",
      col = "pink"
)
pie(incount, 
    labels = inspray, 
    main = "Abundance of insects\naccording to spray type")
#(c)
qplot(
      InsectSprays$spray,
      InsectSprays$count,
      geom = "boxplot",
      xlab = "Spray",
      ylab = "# of insects",
      main = "Boxplots of insect counts\naccording to spray type"
)
#(d)
USArrests
?USArrests
qplot(USArrests[, 3],
      geom = "blank",
      main = "US states urban population",
      xlab = "proportion urban") + 
      geom_histogram(
            color = "blue",
            fill = "white",
            breaks = seq(0, 100, 10),
            closed = "right"
      ) + geom_vline(mapping = aes(
            xintercept = c(
                  quantile(USArrests[, 3], 0.25),
                  median(USArrests[, 3]),
                  quantile(USArrests[, 3], 0.75)
            ),
            linetype = factor(c("1st Q", "median", "3rd Q"))
      ),
      show.legend = TRUE) + 
      scale_linetype_manual(values = 3:5) + 
      labs(linetype = "")
                                                                             
#(e)
ustab <- t(as.matrix(USArrests[, -3]))
barplot(
      ustab,
      names.arg = state.abb,
      horiz = T,
      las = 1,
      legend.text = c("murder", "rape", "assault"),
      main = "USA arrests per 100000\nby state and serious crime"
)
#(f)
urbancat <- rep(1, 50)
urbancat[USArrests$UrbanPop <= median(USArrests$UrbanPop)] <- 0
urbancat <- factor(urbancat)
#(g)
myUSArrests <- USArrests[, -3]
myUSArrests$urbancat <- urbancat
#(h)
ggpairs(myUSArrests, aes(col = urbancat), axisLabels = "internal")
#(i)
magquan <- quantile(quakes$mag, c(1 / 3, 2 / 3))

magfac <-cut(quakes$mag,
            breaks = c(min(quakes$mag), 
                       magquan[1], 
                       magquan[2], 
                       max(quakes$mag)),
            include.lowest = TRUE
      )
#(j)
plot(quakes[, 2],
     quakes[, 1],
     pch = (1:3)[magfac],
     xlab = "Longitude",
     ylab = "Latitude")
#(k)
legend("bottomleft", legend = levels(magfac), pch = 1:3)

##########
## 15.1 ##
##########
#(a)
4 / 52 # Pr(Ace)
1 / 52 # Pr(4 of spades)
#(b)
13 / 52 # Pr(A|B)==Pr(A), so the two events are independent
#(c)
13 / 52 # Pr(A)
13 / 51 # Pr(A|B)  Pr(A|B)!=Pr(A), so the two events are no longer independent
#(d)
12 / 52 # Pr(C)
26 / 52 # Pr(D)
6 / 52 # Pr(C and D) = Pr(C|D)*Pr(D) = (6/26)*(26/52) = 6/52 != 0 therefore C and D are not mutually exclusive

##########
## 15.2 ##
##########
#(a)
##(i) Realization of discrete random variable
##(ii) Discrete random variable
##(iii) Discrete random variable
##(iv) Continuous random variable
##(v) Realization of discrete random variable
##(vi) Continous random variable
#(b)
S.outcomes <- 1:5
##(i)
1 - 0.1 - 0.13 - 0.21 - 0.15 # Pr(S=4)
S.prob <- c(0.1, 0.13, 0.21, 0.41, 0.15)
##(ii)
cumsum(S.prob)
##(iii)
mu.S <- sum(S.prob * S.outcomes)
mu.S
##(iv)
sqrt(sum(S.prob * (S.outcomes - mu.S) ^ 2))
##(v)
sum(S.prob[3:5])
##(vi)
barplot(
      S.prob,
      ylim = c(0, 0.5),
      names.arg = S.outcomes,
      space = 0,
      xlab = "s",
      ylab = "Pr(S = s)"
) ### Unimodal, asymmetric -- slight left skew
#(c)
##(i)
fw <- function(w) {
      w.upper <- w > 65 & w <= 90
      w.lower <- w >= 40 & w <= 65
      
      result <- rep(0, length(w))
      result[w.upper] <- (90 - w[w.upper]) / 625
      result[w.lower] <- (w[w.lower] - 40) / 625
      return(result)
}
##(ii)
Fw <- function(w) {
      w.upper <- w > 65 & w <= 90
      w.lower <- w >= 40 & w <= 65
      
      result <- rep(0, length(w))
      result[w.upper] <- (180 * w[w.upper] - w[w.upper] ^ 2 - 6850) / 1250
      result[w.lower] <- (w[w.lower] ^ 2 - 80 * w[w.lower] + 1600) / 1250
      result[w > 90] <- 1
      
      return(result)
}
##(iii)
fw(55.2)
Fw(55.2)
##(iv)
1 - Fw(60)
##(v)
Fw(76.89) - Fw(60.3)
#(d)
##(i) Bimodal, symmetric
##(ii) Trimodal, asymmetric -- right skew
##(iii) Unimodal, symmetric
##(iv) Unimodal, asymmetric -- right skew

##########
## 16.1 ##
##########
#(a)
barplot(
      dbinom(x = 0:13, size = 13, prob = 0.75),
      names.arg = 0:13,
      space = 0,
      xlab = "x",
      ylab = "Pr(X = x)"
)
#(b)
dbinom(13, 13, 0.75)
#(c)
1 - pbinom(q = 9, 13, 0.75)
#(d)
sum(dbinom(8:11, 13, 0.75))
pbinom(11, 13, 0.75) - pbinom(7, 13, 0.75)
#(e)
pbinom(8, 13, 0.75)
#(f)
visits <- rbinom(10, 13, 0.75)
visits
#(g)
mu.X <- 13 * 0.75
mu.X
sigma.X <- sqrt(mu.X * 0.25)
sigma.X

##########
## 16.2 ##
##########
#(a)
1 - ppois(100, 107)
#(b)
dpois(0, 107)
#(c)
barplot(
      dpois(x = 60:150, lambda = 107),
      names.arg = 60:150,
      space = 0,
      xlab = "x",
      ylab = "Pr(X = x)"
)
#(d)
traffic <- rpois(n = 260, 107)
hist(traffic, xlim = c(60, 150))

##########
## 16.3 ##
##########
a <- 3
b <- 70
#(a)
punif(q = 5.5, min = a, max = b)
#(b)
qunif(p = 1 - 0.15, min = a, max = b)
#(c)
mu.X <- (a + b) / 2 #mean
mu.X
sigma.X <- sqrt((b - a) ^ 2 / 12) #sd
sigma.X
#(d)
punif(mu.X + 0.5 * sigma.X, a, b) - punif(mu.X - 0.5 * sigma.X, a, b)
#(e)
X.dens <- dunif(mu.X, a, b)
X.dens
plot(
      c(a, b),
      rep(X.dens, 2),
      type = "o",
      pch = 19,
      xlim = c(a - 1, b + 1),
      ylim = c(0, X.dens),
      ylab = "f(x)",
      xlab = "x"
)
abline(h = 0, lty = 2)
segments(c(a - 5, b + 5, a, b),
         rep(0, 4),
         rep(c(a, b), 2),
         rep(c(0, X.dens), each = 2),
         lty = rep(1:2, each = 2))
points(c(a, b), c(0, 0))
#(f)
sim1 <- runif(10, a, b)
sim1
quan1 <- quantile(sim1, prob = 1 - 0.15)
quan1
sim2 <- runif(1000, a, b)
quan2 <- quantile(sim2, prob = 1 - 0.15)
quan2      ### Overall, both estimates seem to be centered on the 'true' value from (b), but those based on sim1 (the smaller samples) are more variable.

##########
## 16.4 ##
##########
#(a)
mu <- 17
sigma <- 4.5
##(i)
1 - pnorm(20, mu, sigma)
##(ii)
pnorm(10, mu, sigma) - pnorm(5, mu, sigma)
##(iii)
slow10 <- qnorm(1 - 0.1, mu, sigma)
slow10
##(iv)
xvals <- seq(mu - 4 * sigma, mu + 4 * sigma, length = 200)
fx <- dnorm(xvals, mu, sigma)
xvals.sub <- xvals[xvals >= slow10]
fx.sub <- fx[xvals >= slow10]
plot(
      xvals,
      fx,
      type = "l",
      main = "N(17,4.5) distribution",
      xlab = "x",
      ylab = "f(x)"
)
abline(h = 0, col = "gray")
abline(v = slow10, lty = 2)
polygon(rbind(c(slow10, 0), cbind(xvals.sub, fx.sub), c(max(xvals), 0)), border =
              NA, col = "gray")
##(v)
rnorm(10, mu, sigma)
#(b)
mu <- 10
sigma <- sqrt(2)
##(i)
pnorm(11, mu, sigma) - pnorm(9.5, mu, sigma)
##(ii)
stan9.5 <- (9.5 - mu) / sigma
stan9.5
stan11 <- (11 - mu) / sigma
stan11
pnorm(stan11) - pnorm(stan9.5)
##(iii)
short2.5 <- qnorm(0.025, mu, sigma)
short2.5
##(iv)
(short2.5 - mu) / sigma

##########
## 16.5 ##
##########
#(a)
##(i)
lambda.day <- 3500 / 365.25
lambda.day
##(ii)
xvals <- seq(0, 1, length = 100)
plot(
      xvals,
      dexp(xvals, lambda.day),
      type = "l",
      xlab = "x",
      ylab = "f(x)",
      main = "EXP(0.89) distribution"
)
abline(h = 0, col = "gray")
abline(v = 0, col = "gray")
##(iii)
pexp(0.5 / 24, rate = lambda.day)
##(iv)
qexp(1 - 0.1, rate = lambda.day) * 24
#(b)
##(i)
pexp(5, 1 / 11)
##(ii)
pexp(6, 1 / 9)
##(iii)
1 - pexp(15, 1 / 11)
1 - pexp(15, 1 / 9)
