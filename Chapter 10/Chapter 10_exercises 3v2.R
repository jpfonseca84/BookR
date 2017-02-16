#a ----
loopvec1 <- 5:7
loopvec2 <- 9:6
foo <- matrix(NA, length(loopvec1), length(loopvec2))
for (i in 1:length(loopvec1)) {
      foo[i, ] <- loopvec1[i] * loopvec2
}
foo
#b ----
switch(EXPR = mystring,Homer = 12,Marge = 34,Bart = 56,Lisa = 78,Maggie = 90,NA)

mystring<-c("Peter","Homer","Lois","Stewie","Maggie","Bart°")
numvec <- rep(NA, length(mystring))
for (i in 1:length(mystring))
      numvec[i] <- switch(
            EXPR = mystring[i],
            Homer = 12,
            Marge = 34,
            Bart = 56,
            Lisa = 78,
            Maggie = 90,
            NA
      )

#c----
#mylist is a list with lists and matrixes I need to count ho many matrixes 
##exists in it


counter <- 0

for (i in 1:length(mylist)) {
      member <- mylist[[i]]
      if (is.matrix(member) == T){
            counter <- counter + 1
      }else if (is.list(member)== T) {
            for (j in 1:length(member)) {
                  submember <- member[[j]]
                  if (is.matrix(submember) == T) {
                        counter <- counter + 1
                  }
            }
      }
}


#i 
mylist <-
      list(
            aa = c(3.4, 1),
            bb = matrix(1:4, 2, 2),
            cc = matrix(c(T, T, F, T, F, F), 3, 2),
            dd = "string here",
            ee = list(c("hello", "you"),matrix(c("Hello", "there"))),
            ff = matrix(c("red", "green", "blue", "yellow"))
      )

#ii
mylist <- list("tricked you", as.vector(matrix(1:6, 3, 2)))

#iii
mylist <-
      list(list(1, 2, 3), list(c(3, 2), 2), list(c(1, 2), matrix(c(1, 2))),
           rbind(1:10, 100:91))