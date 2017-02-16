#a ====
mynumbers <- c(4, 5, 1, 2, 6, 2, 4, 6, 7, 2)
mylist <- list()
counter <- 1
mycondition <- mynumbers[counter] <= 5
while (mycondition) {
      mylist[[counter]] <- diag(mynumbers[counter])
      counter <- counter + 1
      if (counter <= length(mynumbers)) {
            mycondition <- mynumbers[counter] <= 5
      } else{
            mycondition <- FALSE
      }
}
#i
mynumbers<-c(2,2,2,2,5,2)

#ii
mynumbers<-2:20

#iii
mynumbers<-c(10,1,10,1,2)

#b ====

#CODE
result <- NA
if (mynum == 0) {
      result <- 1
} else
      while (mynum > 1) {
            if (is.na(result)) {
                  result <- mynum
                  mynum <- mynum - 1
            } else{
                  result <- result * mynum
                  mynum <- mynum - 1
            }
      }

cat(result)
#i
mynum <- 5
#ii
mynum<-12
#iii
mynum<-0

#b faster code ====
result<-1
while(mynum>1){
      result<-result*mynum
mynum<-mynum-1
}
#c ====
mystring <- "R fever"

index <- 1
ecount <- 0
result <- mystring
while (ecount < 2 && index <= nchar(mystring)) {
      
      character <- substr(mystring, index, index)
      
      if (character == "e" || character == "E") {
            ecount <- ecount + 1
      }
      if (ecount == 2) {
            result <- substr(mystring, 1, index - 1)
      }
      index <- index + 1
}
result

#c tests---- 
mystring<-"beautiful"
mystring<-"ECCENTRIC"
mystring<-"Elab0rAte"
mystring<-"eeeeek!"
