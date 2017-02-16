#a ----
#i realization of a random variable. - Discrete quantity
#ii Random variable - DIscrete quantity
#iii Random Variable - discrete quantity
#iv random variable - continuous quantity
#v realization - Discrete quantity
#vi random variable - Continuous quantity
#b ----
b.outcomes <-c(1,2,3,4,5)
b.prob<-c(0.1,0.13,0.21,0.41,0.15)

#i
x<-1-.1-.13-.21-.15
# x is 0.41

#ii
b.probsum<- cumsum(b.prob)

#iii
b.mean<-sum(b.outcomes*b.prob)

#iv the variance
b.var<- sum(((b.outcomes-b.mean)^2)*b.prob)

#v Pr(S<=3)
b.probatleast3<-sum(b.prob[1:3])





#vi
b.appearance<-barplot(b.prob,
                      
                      
                      
                      
                      names.arg = b.outcomes,
                      space=0,
                      main="Appearance",
                      xlab="Stars",
                      ylab="Probability")

#The probability is unimodal, skewed to the left
#C ----
#i
functionf <- function(w) {
      #  (w-40)/625 if 40<=w<=65
      #  (90-w)/625 if 65<w<=90
      #  0 if anything else

      answer <- rep(0, length(w))
      w.lower <- w >= 40 & w <= 65
      w.upper <- w > 65 & w < 90

      answer[w.lower] <- (w[w.lower] - 40) / 625
      answer[w.upper] <- (90 - w[w.upper]) / 625

      return(answer)
}
#ii
FunctionF <- function(w) {
      #w<40 <- 0
      #40<=x<=65 <- (w^2-80*w+1600)/1250
      #65<x<=90 <- (180*w-w^2-6850)/1250
      #else is 1

      answer <- rep(0, length(w))
      w.lower <- w >= 40 & w <= 65
      w.upper <- w > 65 & w <= 90
      w.uppest <- w > 90

      answer[w.lower] <- (w[w.lower] ^ 2 - 80 * w[w.lower] + 1600) / 1250
      answer[w.upper] <- (180 * w[w.upper] - w[w.upper] ^ 2 - 6850) / 1250
      answer[w.uppest] <- 1

      return(answer)
      #test
      #return.15.6(c(12,45,70,100))
}

#iii
functionf(55.2)
FunctionF(55.2)

#iv

1 - FunctionF(60)

#v
FunctionF(76.89) - FunctionF(60.3)


#d ----
#i Bimodal, symetric
#ii TRimodal, assymetric,skewed
#iii Unimodal, Symetric
#iv Unimodal, Skewed
