#a ----

prog_test_fancy <- function(n, ...) {
      result <- 0
      inicialtime <- Sys.time()
      progbar <- txtProgressBar(min = 0,
                                max = n,
                                ...)
      for (i in 1:n) {
            result <- result + 1
            Sys.sleep(0.5)
            setTxtProgressBar(progbar, value = i)
      }
      close(progbar)
      return(list(result, Sys.time() - inicialtime))
}

#b ----

myfibvectorTRY <- function(nvec) {
      #helper formula
      
      myfibrec2 <- function(n) {
            if (n < 0) {
                  warning("Assuming you meant 'n' to be positive -- doing that instead")
                  n <- n * -1
            } else if (n == 0) {
                  stop("'n' is uninterpretable at 0")
            }
            if (n == 1 || n == 2) {
                  return(1)
            } else{
                  return(myfibrec2(n - 1) + myfibrec2(n - 2))
            }
      }
      nterms <- length(nvec)
      result <- rep(0, nterms)
      progbar <-
            txtProgressBar(
                  label = "Progress",
                  max = nterms,
                  style = 3,
                  char = "="
            )
      for (i in 1:nterms) {
            attempt <- try(myfibrec2(nvec[i]), silent = T)
            if (class(attempt) == "try-error") {
                  result[i] <- NA
            } else{
                  result[i] <- attempt
            }
            setTxtProgressBar(progbar, value = i)
      }
      return(result)
}
#i 
myfibvectorTRY(c(3,2,7,0,9,13))

#ii#rewrite the code to calulcate how long does the process take
myfibvectortimeTRY <- function(nvec) {
      #helper formula
      initime<-Sys.time()
      myfibrec2 <- function(n) {
            if (n < 0) {
                  warning("Assuming you meant 'n' to be positive -- doing that instead")
                  n <- n * -1
            } else if (n == 0) {
                  stop("'n' is uninterpretable at 0")
            }
            if (n == 1 || n == 2) {
                  return(1)
            } else{
                  return(myfibrec2(n - 1) + myfibrec2(n - 2))
            }
      }
      nterms <- length(nvec)
      result <- rep(0, nterms)
      progbar <-
            txtProgressBar(
                  label = "Progress",
                  max = nterms,
                  style = 3,
                  char = "="
            )
      for (i in 1:nterms) {
            attempt <- try(myfibrec2(nvec[i]), silent = T)
            if (class(attempt) == "try-error") {
                  result[i] <- NA
            } else{
                  result[i] <- attempt
            }
            setTxtProgressBar(progbar, value = i)
      }
      return(list(result,Sys.time()-initime))
}

#C ----
#rewrite the code to store wach fibonacci number in a vector

t1 <- Sys.time()
fibvec <- c(1,1,rep(NA,33)) 
for(i in 3:35){
      fibvec[i] <- fibvec[i-2]+fibvec[i-1]
}
fibvec
t2 <- Sys.time()
t2-t1























#####test
test<-1:35
answer <- NA
for (i in 1:length(test)) {
      if (test[i] == 1 || test[i] == 2) {
            answer[i] <- 1
      }
      else{
            answer[i] <- answer[i - 1] + answer[i - 2]
            
      }
paste(answer)
}
