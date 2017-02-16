#a ----
myfib4 <- function(thresh, printme=F) {
      if (printme == T) {
            fib.a <- 1
            fib.b <- 1
            cat(fib.a, ", ", fib.b, ", ", sep = "")
            repeat {
                  temp <- fib.a + fib.b
                  fib.a <- fib.b
                  fib.b <- temp
                  cat(fib.b, ", ", sep = "")
                  if (fib.b > thresh) {
                        cat("BREAK NOW...")
                        break
                  }
            }
      } else{
            fibseq <- c(1, 1)
            counter <- 2
            repeat {
                  fibseq <- c(fibseq, fibseq[counter - 1] + fibseq[counter])
                  counter <- counter + 1
                  if (fibseq[counter] > thresh) {
                        break
                  }
            }
            print(fibseq)
      }
}

#b ----
myfac <- function(int) {
      result <- NA
      if (int == 0) {
            result <- 1
            return(result)
      } else
            while (int > 1) {
                  if (is.na(result)) {
                        result <- int
                        int <- int - 1
                  } else{
                        result <- result * int
                        int <- int - 1
                        
                  }
                  
            }
      return(result)
}

#c ----
myfac2 <- function(int) {
      result <- NA
      if (int == 0) {
            result <- 1
            return(result)
      } else if (int < 0) {
            return(result)
      } else{
            while (int > 1) {
                  if (is.na(result)) {
                        result <- int
                        int <- int - 1
                  } else{
                        result <- result * int
                        int <- int - 1
                  }
            }
            return(result)
      }
}