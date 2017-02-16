#a ----
foo <- list("a",
            c("b", "c", "d", "e"),
            "f",
            c("g", "h", "i"))
lapply(foo, function(x) {
      paste(x, "!")
})

#b ----
facnum <- function(number) {
      result <- 1
      if (!is.numeric(number)) {
            return("Not a number!")
      } else if (number < 0) {
            return("Not possible with this function")
      }
      if (number == 0) {
            return(result)
      } else {
            result <- facnum(number - 1) * (number)
            return(result)
      }
}

#c ----


geolist <- function(inputtedlist) {
      result <- list()
      
      #mean of the vector
      helper <- function(selectedvec) {
            return((prod(selectedvec)) ^ (1 / length(selectedvec)))
      }
      #define what is matrix and what is vector
      vectorflag <- sapply(inputtedlist, is.vector)
      
      #Analysis
      for (i in 1:length(inputtedlist)) {
            #if vector
            if (vectorflag[i] == T) {
                  result[[i]] <- helper(inputtedlist[[i]])
            }
            #if matrix
            else{
                  result[[i]] <- apply(inputtedlist[[i]], 1, helper)
            }
      }
      return(result)
}

#i
foo <- list(1:3,
            matrix(c(3.3, 3.2, 2.8, 2.1, 4.6, 4.5, 3.1, 9.4), 4, 2),
            matrix(c(3.3, 3.2, 2.8, 2.1, 4.6, 4.5, 3.1, 9.4), 2, 4))

#ii
bar<- list(1:9,matrix(1:9,1,9),matrix(1:9,9,1),matrix(1:9,3,3))
