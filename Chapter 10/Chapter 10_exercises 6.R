#a ====
foo <- 5
bar <- c(2, 3, 1.1, 4, 0, 4.1, 3)
#i ====
counter <- 1
loop2.result <- rep(NA, length(bar))
while (counter <= length(bar)) {
      if (bar[counter] != 0) {
            loop2.result[counter]  <-  foo  /  bar[counter]
      } else{
            loop2.result[counter] <- NA
      }
      counter <- counter + 1
}
loop2.result
#ii ----

loop3.result <- ifelse(is.finite(foo / bar), foo / bar, NA)

#b ----
mynumbers <- c(4, 5, 1, 2, 6, 2, 4, 6, 6, 2)

#i----
mydiag <- list()
for (i in 1:length(mynumbers)) {
      if (mynumbers[i] > 5) {
            break
      } else{
            mydiag[[i]] <- diag(mynumbers[i])
      }
}
#ii ----
counter <- 1
repdiag <- list()
repeat {
      if (mynumbers[counter] <= 5) {
            repdiag[[counter]] <- diag(mynumbers[counter])
      } else{
            break
      }
      counter <- counter + 1
}

#c

reslist <- list()
counter <- 1
for (i in 1:length(matlist1)) {
      for (j in 1:length(matlist2)) {
            if (ncol(matlist1[[i]]) == nrow(matlist2[[j]])) {
                  reslist[[counter]] <- matlist1[[i]] %*% matlist2[[j]]
            } else{
                  reslist[[counter]] <- "not possible"
            }
            counter <- counter + 1
      }
}
reslist


#i
matlist1 <- list(matrix(1:4, 2, 2), matrix(1:4), matrix(1:8, 4, 2))
matlist2 <- matlist1

#ii
matlist1<-list(matrix(1:4,2,2),matrix(2:5,2,2),matrix(1:16,4,2))
matlist2<-list(matrix(1:8,2,4),matrix(10:7,2,2),matrix(9:2,4,2))