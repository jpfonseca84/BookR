
######################################
#### SUGGESTED EXERCISE SOLUTIONS ####
######################################

#########
## 9.1 ##
#########
#(a)
ls("package:methods")[1:20]
length(ls("package:methods"))
#(b)
##(i)
environment(read.table)
##(ii)
environment(data)
##(iii)
environment(matrix)
##(iv)
environment(jpeg)
#(c)
any(ls("package:graphics")=="smoothScatter")

#########
## 9.2 ##
#########
#(a)
seq(-4,4,0.2)
#(b)
##(i)
array(8:1,dim=c(2,2,2)) # MIXED: 'data' positional, 'dim' exact.
##(ii)
rep(1:2,3) # POSITIONAL
##(iii)
seq(from=10,to=8,length=5) # MIXED: 'from' and 'to' exact, 'length.out' partial.
##(iv)
sort(decreasing=T,x=c(2,1,1,2,0.3,3,1.3)) # EXACT
##(v)
which(matrix(c(T,F,T,T),2,2)) # POSITIONAL
##(vi)
which(matrix(c(T,F,T,T),2,2),a=T) # MIXED: 'x', 'data', 'nrow', 'ncol' positional, 'arr.ind' partial
#(c)
# 'pch', 'lwd', 'lty' and 'col' are part of the ellipsis.

##########
## 10.1 ##
##########
#(a)
vec1 <- c(2,1,1,3,2,1,0)
vec2 <- c(3,8,2,2,0,0,0)
##(i)
if((vec1[1]+vec2[2])==10){ cat("Print me!") }  # condition SATISFIED
##(ii)
if(vec1[1]>=2&&vec2[1]>=2){	cat("Print me!") }  # condition SATISFIED
##(iii)
if(all((vec2-vec1)[c(2,6)]<7)){	cat("Print me!") }  # condition NOT SATISFIED
##(iv)
if(!is.na(vec2[3])){ cat("Print me!") }  # condition SATISFIED
#(b)
ifelse(vec1+vec2>3,vec1*vec2,vec1+vec2)
#(c)
if(any(substr(diag(mymat),1,1)=="g")||any(substr(diag(mymat),1,1)=="G")){
  indexes <- which(substr(diag(mymat),1,1)=="g"|substr(diag(mymat),1,1)=="G")
  diag(mymat)[indexes] <- "HERE"
} else {
  mymat <- diag(nrow(mymat))
}
mymat
##(i)
mymat <- matrix(as.character(1:16),4,4)
##(ii)
mymat <- matrix(c("DANDELION","Hyacinthus","Gerbera","MARIGOLD","geranium","ligularia","Pachysandra","SNAPDRAGON","GLADIOLUS"),3,3)
##(iii)
mymat <- matrix(c("GREAT","exercises","right","here"),2,2,byrow=T)

##########
## 10.2 ##
##########
mynum <- 3
mynum <- 0
#(a)
if(mynum==1){
  foo <- 12
} else if(mynum==2){
  foo <- 34
} else if(mynum==3){
  foo <- 56
} else if(mynum==4){
  foo <- 78
} else if(mynum==5){
  foo <- NA
} else {
  foo <- NULL
}
foo
#(b)
if(any(doselevel=="High")){
  if(lowdose>=10){
    lowdose <- 10
  } else {
    lowdose <- lowdose/2
  }
  if(meddose>=26){
    meddose <- 26
  }
  if(highdose<60){
    highdose <- 60
  } else {
    highdose <- highdose*1.5
  }
  dosage <- rep(lowdose,length(doselevel))
  dosage[doselevel=="Med"] <- meddose
  dosage[doselevel=="High"] <- highdose
} else {
  doselevel <- factor(doselevel,levels=c("Low","Med"),labels=c("Small","Large"))
  if(lowdose<15 && meddose<35){
    lowdose <- lowdose*2
    meddose <- meddose+highdose
  }
  dosage <- rep(lowdose,length(doselevel))
  dosage[doselevel=="Large"] <- meddose
}
##(i)
lowdose <- 12.5
meddose <- 25.3
highdose <- 58.1
doselevel <- factor(c("Low","High","High","High","Low","Med","Med"),levels=c("Low","Med","High"))
##(ii)
lowdose <- 12.5
meddose <- 25.3
highdose <- 58.1
doselevel <- factor(c("Low","Low","Low","Med","Low","Med","Med"),levels=c("Low","Med","High"))
##(iii)
lowdose <- 9
meddose <- 49
highdose <- 61
doselevel <- factor(c("Low","Med","Med"),levels=c("Low","Med","High"))
##(iv)
lowdose <- 9
meddose <- 49
highdose <- 61
doselevel <- factor(c("Low","High","High","High","Low","Med","Med"),levels=c("Low","Med","High"))
#(c)
mynum <- 3
ifelse(mynum>0,switch(mynum,"one","two","three","four","five","six","seven","eight","nine"),"zero")
mynum <- 0
ifelse(mynum>0,switch(mynum,"one","two","three","four","five","six","seven","eight","nine"),"zero")

##########
## 10.3 ##
##########
#(a)
loopvec1 <- 5:7
loopvec2 <- 9:6
foo <- matrix(NA,length(loopvec1),length(loopvec2))
for(i in 1:length(loopvec1)){
  foo[i,] <- loopvec1[i]*loopvec2
}
foo
#(b)
mystrings <- c("Peter","Homer","Lois","Stewie","Maggie","Bart")
mynums <- rep(NA,length(mystrings))
for(i in 1:length(mystrings)){
  mynums[i] <- switch(EXPR=mystrings[i],Homer=12,Marge=34,Bart=56,Lisa=78,Maggie=90,NA)
}
#(c)
counter <- 0
for(i in 1:length(mylist)){
  member <- mylist[[i]]
  if(is.matrix(member)){
    counter <- counter+1
  } else if(is.list(member)){
    for(j in 1:length(member)){
      if(is.matrix(member[[j]])){
        counter <- counter+1
      }
    }
  }
}
##(i)
mylist <- list(aa=c(3.4,1),bb=matrix(1:4,2,2),cc=matrix(c(T,T,F,T,F,F),3,2),dd="string here",ee=list(c("hello","you"),matrix(c("hello","there"))),ff=matrix(c("red","green","blue","yellow")))
##(ii)
mylist <- list("tricked you",as.vector(matrix(1:6,3,2)))
##(iii)
mylist <- list(list(1,2,3),list(c(3,2),2),list(c(1,2),matrix(c(1,2))),rbind(1:10,100:91))

##########
## 10.4 ##
##########
#(a)
##(i)
mylist <- list()
counter <- 1
mynumbers <- c(2,2,2,2,5,2)
mycondition <- mynumbers[counter]<=5
mycondition
while(mycondition){
  mylist[[counter]] <- diag(mynumbers[counter])
  counter <- counter+1
  if(counter<=length(mynumbers)){
    mycondition <- mynumbers[counter]<=5
  } else {
    mycondition <- FALSE
  }
}
mylist
##(ii)
mylist <- list()
counter <- 1
mynumbers <- 2:20
mycondition <- mynumbers[counter]<=5
mycondition
while(mycondition){
  mylist[[counter]] <- diag(mynumbers[counter])
  counter <- counter+1
  if(counter<=length(mynumbers)){
    mycondition <- mynumbers[counter]<=5
  } else {
    mycondition <- FALSE
  }
}
mylist
##(iii) below, the loop braced-area code won't even be entered -- the first element of 'mynumbers' is greater than 5 -- resulting list will be empty
mylist <- list()
counter <- 1
mynumbers <- c(10,1,10,1,2)
mycondition <- mynumbers[counter]<=5
mycondition
while(mycondition){
  mylist[[counter]] <- diag(mynumbers[counter])
  counter <- counter+1
  if(counter<=length(mynumbers)){
    mycondition <- mynumbers[counter]<=5
  } else {
    mycondition <- FALSE
  }
}
mylist
#(b)
mynum.fac <- 1
while(mynum>1){
  mynum.fac <- mynum.fac*mynum
  mynum <- mynum-1
}
mynum.fac
##(i)
mynum <- 5
##(ii)
mynum <- 12
##(iii)
mynum <- 0
#(c)
mystring <- "R fever"
mystring <- "beautiful"
mystring <- "ECCENTRIC"
mystring <- "ElAbOrAte"
mystring <- "eeeeek!"
#--#
index <- 1
ecount <- 0
result <- mystring
while(ecount<2 && index<=nchar(mystring)){
  temp.char <- substr(mystring,index,index)	
  if(temp.char=="e"||temp.char=="E"){
    ecount <- ecount+1
  }
  if(ecount==2){
    result <- substr(mystring,1,index-1)
  }
  index <- index + 1
}
result

##########
## 10.5 ##
##########
#(a)
foo <- matrix(1:12,4,3)
apply(apply(foo,1,sort,decreasing=TRUE),2,prod)
#(b)
matlist <- list(matrix(c(T,F,T,T),2,2),matrix(c("a","c","b","z","p","q"),3,2),matrix(1:8,2,4))
matlist
for(i in 1:length(matlist)){
  matlist[[i]] <- t(matlist[[i]])
}
matlist
matlist <- list(matrix(c(T,F,T,T),2,2),matrix(c("a","c","b","z","p","q"),3,2),matrix(1:8,2,4))
matlist
matlist <- lapply(matlist,t)
matlist
#(c)
qux <- array(96:1,dim=c(4,4,2,3))
##(i)
apply(qux[,,2,],3,diag)
##(ii)
apply(apply(qux[,4,,],3,dim),1,sum)

##########
## 10.6 ##
##########
#(a)
foo <- 5
bar <- c(2,3,1.1,4,0,4.1,3)
##(i)
loop2.result <- rep(NA,length(bar))
condition <- TRUE
counter <- 1
while(condition){
  temp <- foo/bar[counter]
  if(is.finite(temp)){
    loop2.result[counter] <- temp
  } else {
    condition <- FALSE
  }
  counter <- counter+1
}
loop2.result
##(ii)
loop3.result <- ifelse(is.finite(foo/bar),foo/bar,NA)
loop3.result
#(b)
mynumbers <- c(4,5,1,2,6,2,4,6,6,2)
##(i)
mylist <- list()
for(i in 1:length(mynumbers)){
  if(mynumbers[i]<=5){
    mylist[[i]] <- diag(mynumbers[i])
  } else {
    break
  }
}
mylist
##(ii)
mylist <- list()
counter <- 0
repeat{
  counter <- counter+1
  if(counter<=length(mynumbers)){
    if(mynumbers[counter]<=5){
      mylist[[counter]] <- diag(mynumbers[counter])
    } else {
      break
    }
  } else {
    break
  }
}
mylist
#(c)
counter <- 0
reslist <- list()
for(i in 1:length(matlist1)){
  for(j in 1:length(matlist2)){
    counter <- counter+1
    if(ncol(matlist1[[i]])!=nrow(matlist2[[j]])){
      reslist[[counter]] <- "not possible"
      next
    }
    reslist[[counter]] <- matlist1[[i]]%*%matlist2[[j]]
  }
}
reslist
##(i)
matlist1 <- list(matrix(1:4,2,2),matrix(1:4),matrix(1:8,4,2))
matlist2 <- matlist1
##(ii)
matlist1 <- list(matrix(1:4,2,2),matrix(2:5,2,2),matrix(1:16,4,2))
matlist2 <- list(matrix(1:8,2,4),matrix(10:7,2,2),matrix(9:2,4,2))


##########
## 11.1 ##
##########
#(a)
myfib4 <- function(thresh,printme){
  if(printme){
    fib.a <- 1
    fib.b <- 1
    cat(fib.a,", ",fib.b,", ",sep="")
    repeat{
      temp <- fib.a+fib.b
      fib.a <- fib.b
      fib.b <- temp
      cat(fib.b,", ",sep="")
      if(fib.b>thresh){
        cat("BREAK NOW...")
        break
      }
    }
  } else {
    fibseq <- c(1,1)
    counter <- 2
    repeat{
      fibseq <- c(fibseq,fibseq[counter-1]+fibseq[counter])
      counter <- counter+1
      if(fibseq[counter]>thresh){
        break
      }
    }
    return(fibseq)
  }
}
myfib4(thresh=150,printme=TRUE)
myfib4(1000000,T)
myfib4(150,FALSE)
myfib4(1000000,printme=F)
#(b)
##(i)
myfac <- function(int){
  result <- 1
  while(int>1){
    result <- result*int
    int <- int-1
  }
  return(result)
}
myfac(5)
myfac(12)
myfac(0)
##(ii)
myfac2 <- function(int){
  if(int<0){
    return(NaN)
  }
  result <- 1
  while(int>1){
    result <- result*int
    int <- int-1
  }
  return(result)
}
myfac2(5)
myfac2(12)
myfac2(0)
myfac2(-6)

##########
## 11.2 ##
##########
#(a)
comp <- function(P,i,t=12,y,plotit=TRUE,...){
  yseq <- 1:y
  values <- P*(1+i/(100*t))^(t*yseq)
  
  if(plotit){  
    plot(yseq,values,type="s",...)
  } else {
    return(values)
  }
}
##(i)
comp(5000,4.4,y=10,plotit=F)[10]
##(ii)
comp(100,22.9,12,20,plotit=T,main="Compound interest calculator",ylab="Balance (F)",xlab="Year (y)")
##(iii)
ann <- comp(100,22.9,1,20,plotit=F)
lines(1:20,ann,lty=2,type="s")
legend("topleft",lty=c(1,2),legend=c("monthly interest","annual interest"))
#(b)
quad <- function(k1,k2,k3){
  if(any(c(missing(k1),missing(k2),missing(k3)))){
    return("At least one of k1, k2, k3 was missing")
  }
  x <- k2^2-4*k1*k3
  if(x<0){
    cat("No real roots\n")
  } else if(x==0){
    return(-k2/(2*k1))
  } else {
    return(c((-k2-x^0.5)/(2*k1),(-k2+x^0.5)/(2*k1)))
  }
}
##(i)
quad(k1=2,k2=-1,k3=-5)
quad(1,1,1)
##(ii)
quad(k1=1.3,k2=-8,k3=-3.13)
quad(2.25,-3,1)
quad(1.4,-2.2,-5.1)
quad(-5,10.11,-9.9)
##(iii)
quad(0)

##########
## 11.3 ##
##########
#(a)
foo <- list("a",c("b","c","d","e"),"f",c("g","h","i"))
lapply(foo,function(x) paste(x,"!",sep=""))
#(b)
facrec <- function(x){
  if(x==0){
    return(1)
  } else {
    return(x*facrec(x-1))
  }
}
##(i)
facrec(5)
##(ii)
facrec(12)
##(iii)
facrec(0)
#(c)
geolist <- function(x){
  geo <- function(nums){
    return(prod(nums)^(1/length(nums)))
  }
  
  for(i in 1:length(x)){
    if(!is.matrix(x[[i]])){
      x[[i]] <- geo(x[[i]])
    } else {
      x[[i]] <- apply(x[[i]],1,geo)
    }
  }
  return(x)
}
##(i)
foo <- list(1:3,matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4),4,2),matrix(c(3.3,3.2,2.8,2.1,4.6,4.5,3.1,9.4),2,4))
geolist(foo)
##(ii)
bar <- list(1:9,matrix(1:9,1,9),matrix(1:9,9,1),matrix(1:9,3,3))
geolist(bar)

##########
## 12.1 ##
##########
#(a)
facrec2 <- function(x){
  if(x<0){
    stop("'x' must be a positive integer")
  }
  
  if(x==0){
    return(1)
  } else {
    return(x*facrec2(x-1))
  }
}
##(i)
facrec2(5)
##(ii)
facrec2(8)
##(iii)
facrec2(-8)
#(b)
matinv <- function(x,noninv=NA,nonmat="not a matrix",silent=TRUE){
  if(!is.list(x)){
    stop("'x' must be a list")
  }
  
  n <- length(x)
  if(n==0){
    stop("'x' appears to be empty")
  }
  
  if(!is.character(nonmat)){
    warning("attempting to coerce 'nonmat' to a character string")
    nonmat <- as.character(nonmat)
  }
  
  for(i in 1:n){
    if(is.matrix(x[[i]])){
      attempt <- try(solve(x[[i]]),silent=silent)
      if(class(attempt)=="try-error"){
        x[[i]] <- noninv
      } else {
        x[[i]] <- attempt
      }
    } else {
      x[[i]] <- nonmat
    }
  }
  
  return(x)
}
##(i)
x <- list(1:4,matrix(1:4,1,4),matrix(1:4,4,1),matrix(1:4,2,2))
matinv(x)
##(ii)
matinv(x,noninv=Inf,nonmat=666)
##(iii)
matinv(x,noninv=Inf,nonmat=666,silent=F)
##(iv)
x <- list(diag(9),matrix(c(0.2,0.4,0.2,0.1,0.1,0.2),3,3),rbind(c(5,5,1,2),c(2,2,1,8),c(6,1,5,5),c(1,0,2,0)),matrix(1:6,2,3),cbind(c(3,5),c(6,5)),as.vector(diag(2)))
matinv(x,noninv="unsuitable matrix")
##(v)
x <- "hello"
matinv(x)
##(vi)
x <- list()
matinv(x)

##########
## 12.2 ##
##########
#(a)
prog_test_fancy <- function(n,...){
  result <- 0
  progbar <- txtProgressBar(min=0,max=n,...)
  for(i in 1:n){
    result <- result + 1
    Sys.sleep(0.5)
    setTxtProgressBar(progbar,value=i)
  }
  close(progbar)
  return(result)
}
ence <- Sys.time()
prog_test_fancy(50,style=3,char="r")
differ <- Sys.time()
differ-ence
#(b)
myfibrec2 <- function(n){
  if(n<0){
    warning("Assuming you meant 'n' to be positive -- doing that instead")
    n <- n*-1
  } else if(n==0){
    stop("'n' is uninterpretable at 0")
  }
  
  if(n==1||n==2){
    return(1)
  } else {
    return(myfibrec2(n-1)+myfibrec2(n-2))
  }
}
myfibvectorTRY2 <- function(nvec){
  nterms <- length(nvec)
  result <- rep(0,nterms)
  progbar <- txtProgressBar(min=0,max=nterms,style=3,char="-")
  for(i in 1:nterms){
    attempt <- try(myfibrec2(nvec[i]),silent=T)
    if(class(attempt)=="try-error"){
      result[i] <- NA
    } else {
      result[i] <- attempt
    }
    setTxtProgressBar(progbar,value=i)
  }
  close(progbar)
  return(result)
}
##(i)
myfibvectorTRY2(nvec=c(3,2,7,0,9,13))
##(ii)
t1 <- Sys.time()
myfibvectorTRY2(1:35)
t2 <- Sys.time()
t2-t1
### This takes almost 1 minute on my machine... execution slows down as the recursion gets deeper... recursion perhaps not so good for computing Fibonacci sequence
#(c)
t1 <- Sys.time()
fibvec <- c(1,1,rep(NA,33)) 
for(i in 3:35){
  fibvec[i] <- fibvec[i-2]+fibvec[i-1]
}
fibvec
t2 <- Sys.time()
t2-t1
### This is substantially quicker than recursion!
