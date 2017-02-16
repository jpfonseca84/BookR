#a ----
facnum <- function(number) {
      result <- 1
      if (!is.numeric(number)) {
            return("Not a number!")
      }
      if (number < 0) {
            stop("Number must be positive with this function")
      }
      if (number == 0) {
            return(result)
      } else {
            result <- facnum(number - 1) * (number)
            return(result)
      }
}

#b ----
#try to solve all matrixes in a list
solvelist <- function(x,
                      noninv = NA,
                      nonmat = "not a matrix",
                      silent = T) {
      #creation of solve list
      solvedlist <- list()
      #check if x is a list
      if (!is.list(x)) {
            stop("The data passed is not a list, please add a list.")
      }
      if (length(x) == 0) {
            stop("List is empty. Please populate the list.")
      }
      if (!is.character(nonmat)) {
            warning("Non matrix argument being converted to text.")
            nonmat <- as.character(nonmat)
      }
      for (i in 1:length(x)) {
            if (is.matrix(x[[i]])) {
                  solvedlist[[i]] <- try(solve(x[[i]]))
                  if (class(solvedlist[[i]]) == "try-error") {
                        solvedlist[[i]] <- noninv
                  }
            } else{
                  solvedlist[[i]] <- nonmat
            }
      }
      return(solvedlist)
}

#i ----
x<-list(1:4,matrix(1:4,1,4),matrix(1:4,4,1),matrix(1:4,2,2))

#ii
x<-list(1:4,matrix(1:4,1,4),matrix(1:4,4,1),matrix(1:4,2,2))
#noninv as Inf
#nonmat as 666,
#iii
x<-list(1:4,matrix(1:4,1,4),matrix(1:4,4,1),matrix(1:4,2,2))
#noninv as Inf
#nonmat as 666,
# silent as FALSE

#iv
x<-list(diag(9),matrix(c(0.2,0.4,0.2,0.1,0.1,0.2),3,3),
        rbind(c(5,5,1,2),c(2,2,1,8),c(6,1,5,5),c(1,0,2,0)),matrix(1:6,2,3),
        cbind(c(3,5),c(6,5)),as.vector(diag(2)))
solvelist(x,"unsuitable matrix")

#v
solvelist("Hello","unsuitable matrix")

#vi
solvelist(list(),"unsuitable matrix")
