options(prompt="R> ")

#################
### CHAPTER 9 ###
#################

###############
# Section 9.1 #
###############

## 9.1.1 ##

foo <- 4+5
bar <- "stringtastic"
ls()

#

ls("package:graphics")

#

youthspeak <- matrix(data=c("OMG","LOL","WTF","YOLO"),nrow=2,ncol=2)
youthspeak


## 9.1.2 ##

search()

#

baz <- seq(from=0,to=3,length.out=5)
baz

#

environment(seq)
environment(arrows)

#

library("car")
search()

#

neither.here()
nor.there


## 9.1.3 ##

NaN <- 5

#

False <- "confusing"
nan <- "this is"
cat(nan,False)

#

T <- 42
F <- TRUE
F&&TRUE

#

ls()
rm(list=ls())
ls()



###############
# Section 9.2 #
###############

## 9.2.1 ##

bar <- matrix(data=1:9,nrow=3,ncol=3,dimnames=list(c("A","B","C"),c("D","E","F")))
bar

#

bar <- matrix(nrow=3,dimnames=list(c("A","B","C"),c("D","E","F")),ncol=3,data=1:9)
bar


## 9.2.2 ##

bar <- matrix(nr=3,di=list(c("A","B","C"),c("D","E","F")),nc=3,dat=1:9)
bar

#

bar <- matrix(nr=3,di=list(c("A","B","C"),c("D","E","F")),nc=3,d=1:9)


## 9.2.3 ##

args(matrix)

#

bar <- matrix(1:9,3,3,F,list(c("A","B","C"),c("D","E","F")))
bar

#

bar <- matrix(1:9,3,3,list(c("A","B","C"),c("D","E","F")))


## 9.2.4 ##

bar <- matrix(1:9,3,3,dim=list(c("A","B","C"),c("D","E","F")))
bar


## 9.2.5 ##

args(data.frame)

#

args(plot)




##################
### CHAPTER 10 ###
##################

################
# Section 10.1 #
################

## 10.1.1 ##

a <- 3
mynumber <- 4

#

if(a<=mynumber){
  a <- a^2
}

#

a

#

myvec <- c(2.73,5.40,2.15,5.29,1.36,2.16,1.41,6.97,7.99,9.52)
myvec
mymat <- matrix(c(2,0,1,2,3,0,3,0,1,1),5,2)
mymat

#

if(any((myvec-1)>9)||matrix(myvec,2,5)[2,1]<=6){
  cat("Condition satisfied --\n")
  new.myvec <- myvec
  new.myvec[seq(1,9,2)] <- NA
  mylist <- list(aa=new.myvec,bb=mymat+0.5)
  cat("-- a list with",length(mylist),"members now exists.")
}

#

mylist

#

myvec-1
(myvec-1)>9
any((myvec-1)>9)

#

matrix(myvec,2,5)
matrix(myvec,2,5)[2,1]
matrix(myvec,2,5)[2,1]<=6

#

any((myvec-1)>9)||matrix(myvec,2,5)[2,1]<=6


## 10.1.2 ##

a <- 3
mynumber <- 4

#

if(a<=mynumber){
  cat("Condition was",a<=mynumber)
  a <- a^2
} else {
  cat("Condition was",a<=mynumber)
  a <- a-3.5
}
a

#

if(a<=mynumber){
  cat("Condition was",a<=mynumber)
  a <- a^2
} else {
  cat("Condition was",a<=mynumber)
  a <- a-3.5
}
a


## 10.1.3 ##

if(c(FALSE,TRUE,FALSE,TRUE,TRUE)){}

#

x <- 5
y <- -5:5
y

#

y==0

#

result <- ifelse(test=y==0,yes=NA,no=x/y)
result


## 10.1.4 ##

a <- 3
mynumber <- 4
if(a<=mynumber){
  cat("First condition was TRUE\n")
  a <- a^2
  if(mynumber>3){
    cat("Second condition was TRUE")
    b <- seq(1,a,length=mynumber)
  } else {
    cat("Second condition was FALSE")
    b <- a*mynumber
  }
} else {
  cat("First condition was FALSE\n")
  a <- a-3.5
  if(mynumber>=4){
    cat("Second condition was TRUE")
    b <- a^(3-mynumber)
  } else {
    cat("Second condition was FALSE")
    b <- rep(a+mynumber,times=3)
  }
}
a
b

#

a <- 6
mynumber <- 4

#

if(a<=mynumber){
  cat("First condition was TRUE\n")
  a <- a^2
  if(mynumber>3){
    cat("Second condition was TRUE")
    b <- seq(1,a,length=mynumber)
  } else {
    cat("Second condition was FALSE")
    b <- a*mynumber
  }
} else {
  cat("First condition was FALSE\n")
  a <- a-3.5
  if(mynumber>=4){
    cat("Second condition was TRUE")
    b <- a^(3-mynumber)
  } else {
    cat("Second condition was FALSE")
    b <- rep(a+mynumber,times=3)
  }
}
a
b

#

a <- 3
mynumber <- 4
if(a<=mynumber && mynumber>3){
  cat("Same as 'first condition TRUE and second TRUE'")
  a <- a^2
  b <- seq(1,a,length=mynumber)
} else if(a<=mynumber && mynumber<=3){
  cat("Same as 'first condition TRUE and second FALSE'")	
  a <- a^2
  b <- a*mynumber
} else if(mynumber>=4){
  cat("Same as 'first condition FALSE and second TRUE'")
  a <- a-3.5
  b <- a^(3-mynumber)
} else {
  cat("Same as 'first condition FALSE and second FALSE'")
  a <- a-3.5
  b <- rep(a+mynumber,times=3)
}
a
b

#

a <- 6
mynumber <- 4
if(a<=mynumber && mynumber>3){
  cat("Same as 'first condition TRUE and second TRUE'")
  a <- a^2
  b <- seq(1,a,length=mynumber)
} else if(a<=mynumber && mynumber<=3){
  cat("Same as 'first condition TRUE and second FALSE'")	
  a <- a^2
  b <- a*mynumber
} else if(mynumber>=4){
  cat("Same as 'first condition FALSE and second TRUE'")
  a <- a-3.5
  b <- a^(3-mynumber)
} else {
  cat("Same as 'first condition FALSE and second FALSE'")
  a <- a-3.5
  b <- rep(a+mynumber,times=3)
}
a
b


## 10.1.5 ##

mystring <- "Lisa"
if(mystring=="Homer"){
  foo <- 12
} else if(mystring=="Marge"){
  foo <- 34
} else if(mystring=="Bart"){
  foo <- 56
} else if(mystring=="Lisa"){
  foo <- 78
} else if(mystring=="Maggie"){
  foo <- 90
} else {
  foo <- NA
}
foo

#

mystring <- "Peter"
if(mystring=="Homer"){
  foo <- 12
} else if(mystring=="Marge"){
  foo <- 34
} else if(mystring=="Bart"){
  foo <- 56
} else if(mystring=="Lisa"){
  foo <- 78
} else if(mystring=="Maggie"){
  foo <- 90
} else {
  foo <- NA
}
foo

#

mystring <- "Lisa"
foo <- switch(EXPR=mystring,Homer=12,Marge=34,Bart=56,Lisa=78,Maggie=90,NA)
foo

#

mystring <- "Peter"
foo <- switch(EXPR=mystring,Homer=12,Marge=34,Bart=56,Lisa=78,Maggie=90,NA)
foo

#

mynum <- 3
foo <- switch(mynum,12,34,56,78,NA)
foo

#

mynum <- 0
foo <- switch(mynum,12,34,56,78,NA)
foo



################
# Section 10.2 #
################

## 10.2.1 ##

for(myitem in 5:7){
  cat("--BRACED AREA BEGINS--\n")
  cat("the current item is",myitem,"\n")
  cat("--BRACED AREA ENDS--\n\n")
}

#

counter <- 0
for(myitem in 5:7){
  counter <- counter+1
  cat("The item in run",counter,"is",myitem,"\n")
}

#

myvec <- c(0.4,1.1,0.34,0.55)
for(i in myvec){
  print(2*i)
}
for(i in 1:length(myvec)){
  print(2*myvec[i])
}

#

foo <- list(aa=c(3.4,1),bb=matrix(1:4,2,2),cc=matrix(c(T,T,F,T,F,F),3,2),dd="string here",ee=matrix(c("red","green","blue","yellow")))
foo

#

name <- names(foo)
name
is.mat <- rep(NA,length(foo))
is.mat
nr <- is.mat
nc <- is.mat
data.type <- is.mat

#

for(i in 1:length(foo)){
  member <- foo[[i]]
  if(is.matrix(member)){
    is.mat[i] <- "Yes"
    nr[i] <- nrow(member)
    nc[i] <- ncol(member)
    data.type[i] <- class(as.vector(member))
  } else {
    is.mat[i] <- "No"
  }
}
bar <- data.frame(name,is.mat,nr,nc,data.type,stringsAsFactors=F)

#

bar

#

loopvec1 <- 5:7
loopvec1
loopvec2 <- 9:6
loopvec2
foo <- matrix(NA,length(loopvec1),length(loopvec2))
foo

#

for(i in 1:length(loopvec1)){
  for(j in 1:length(loopvec2)){
    foo[i,j] <- loopvec1[i]*loopvec2[j]
  }	
}
foo

#

foo <- matrix(NA,length(loopvec1),length(loopvec2))
foo
for(i in 1:length(loopvec1)){
  for(j in 1:i){
    foo[i,j] <- loopvec1[i]+loopvec2[j]
  }
}
foo


## 10.2.2 ##

myval <- 5
while(myval<10){
  myval <- myval+1
  cat("\n'myval' is now",myval,"\n")
  cat("'mycondition' is now",myval<10,"\n")
}

#

mylist <- list()
counter <- 1
mynumbers <- c(4,5,1,2,6,2,4,6,6,2)
mycondition <- mynumbers[counter]<=5
while(mycondition){
  mylist[[counter]] <- diag(mynumbers[counter])
  counter <- counter+1
  if(counter<=length(mynumbers)){
    mycondition <- mynumbers[counter]<=5
  } else {
    mycondition <- FALSE
  }
}

#

mylist


## 10.2.3 ##

foo <- matrix(1:12,4,3)
foo

#

sum(foo)

#

row.totals <- rep(NA,times=nrow(foo))
for(i in 1:nrow(foo)){
  row.totals[i] <- sum(foo[i,])
}
row.totals

#

row.totals2 <- apply(X=foo,MARGIN=1,FUN=sum)
row.totals2

#

apply(X=foo,MARGIN=2,FUN=sum)

#

bar <- array(1:18,dim=c(3,3,2))
bar

#

apply(bar,3,FUN=diag)

#

dia.url <- "http://www.amstat.org/publications/jse/v9n2/4cdata.txt"
diamonds <- read.table(dia.url)
names(diamonds) <- c("Carat","Color","Clarity","Cert","Price")
diamonds[1:5,]

#

tapply(diamonds$Price,INDEX=diamonds$Color,FUN=sum)

#

baz <- list(aa=c(3.4,1),bb=matrix(1:4,2,2),cc=matrix(c(T,T,F,T,F,F),3,2),dd="string here",ee=matrix(c("red","green","blue","yellow")))

#

lapply(baz,FUN=is.matrix)

#

sapply(baz,FUN=is.matrix)

#

apply(foo,1,sort,decreasing=TRUE)



################
# Section 10.3 #
################

## 10.3.1 ##

foo <- 5
bar <- c(2,3,1.1,4,0,4.1,3)

#

loop1.result <- rep(NA,length(bar))
loop1.result
for(i in 1:length(bar)){
  temp <- foo/bar[i]
  if(is.finite(temp)){
    loop1.result[i] <- temp
  } else {
    break
  }
}
loop1.result

#

loop2.result <- rep(NA,length(bar))
loop2.result
for(i in 1:length(bar)){
  if(bar[i]==0){
    next
  }
  loop2.result[i] <- foo/bar[i]
}
loop2.result

#

loopvec1 <- 5:7
loopvec1
loopvec2 <- 9:6
loopvec2
baz <- matrix(NA,length(loopvec1),length(loopvec2))
baz
for(i in 1:length(loopvec1)){
  for(j in 1:length(loopvec2)){
    temp <- loopvec1[i]*loopvec2[j]
    if(temp>=54){
      next
    }
    baz[i,j] <- temp
  }	
}
baz


## 10.3.2 ##

fib.a <- 1
fib.b <- 1
repeat{
  temp <- fib.a+fib.b
  fib.a <- fib.b
  fib.b <- temp
  cat(fib.b,", ",sep="")
  if(fib.b>150){
    cat("BREAK NOW...\n")
    break
  }
}




##################
### CHAPTER 11 ###
##################

################
# Section 11.1 #
################

## 11.1.1 ##

myfib <- function(){
  fib.a <- 1
  fib.b <- 1
  cat(fib.a,", ",fib.b,", ",sep="")
  repeat{
    temp <- fib.a+fib.b
    fib.a <- fib.b
    fib.b <- temp
    cat(fib.b,", ",sep="")
    if(fib.b>150){
      cat("BREAK NOW...")
      break
    }
  }
}

#

myfib()

#

myfib2 <- function(thresh){
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
}

#

myfib2(thresh=150)
myfib2(1000000)

#

myfib3 <- function(thresh){
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

#

myfib3(150)
foo <- myfib3(10000)
foo
bar <- foo[1:5]
bar


## 11.1.2 ##

dummy1 <- function(){
  aa <- 2.5
  bb <- "string me along"
  cc <- "string 'em up"
  dd <- 4:8
}

dummy2 <- function(){
  aa <- 2.5
  bb <- "string me along"
  cc <- "string 'em up"
  dd <- 4:8	
  return(dd)
}

#

foo <- dummy1()
foo
bar <- dummy2()
bar

#

dummy3 <- function(){
  aa <- 2.5
  bb <- "string me along"
  return(aa)
  cc <- "string 'em up"
  dd <- 4:8
  return(bb)
}

#

baz <- dummy3()
baz



################
# Section 11.2 #
################

## 11.2.1 ##

multiples1 <- function(x,mat,str1,str2){
  matrix.flags <- sapply(x,FUN=is.matrix)
  
  if(!any(matrix.flags)){
    return(str1)
  }
  
  indexes <- which(matrix.flags)
  counter <- 0
  result <- list()
  for(i in indexes){
    temp <- x[[i]]
    if(ncol(temp)==nrow(mat)){
      counter <- counter+1
      result[[counter]] <- temp%*%mat
    }
  }
  
  if(counter==0){
    return(str2)
  } else {
    return(result)
  }
}

#

foo <- list(matrix(1:4,2,2),"not a matrix","definitely not a matrix",matrix(1:8,2,4),matrix(1:8,4,2))
bar <- list(1:4,"not a matrix",c(F,T,T,T),"??")
baz <- list(1:4,"not a matrix",c(F,T,T,T),"??",matrix(1:8,2,4))

#

multiples1(x=foo,mat=diag(2),str1="no matrices in 'x'",str2="matrices in 'x' but none of appropriate dimensions given 'mat'")

#

multiples1(x=bar,mat=diag(2),str1="no matrices in 'x'",str2="matrices in 'x' but none of appropriate dimensions given 'mat'")

#

multiples1(x=baz,mat=diag(2),str1="no matrices in 'x'",str2="matrices in 'x' but none of appropriate dimensions given 'mat'")

#

multiples1(x=foo,mat=diag(2)) # demonstrating lazy evaluation

#

multiples1(x=bar,mat=diag(2)) # demonstrating error due to unspecified formal argument


## 11.2.2 ##

multiples2 <- function(x,mat,str1="no valid matrices",str2=str1){
  matrix.flags <- sapply(x,FUN=is.matrix)
  
  if(!any(matrix.flags)){
    return(str1)
  }
  
  indexes <- which(matrix.flags)
  counter <- 0
  result <- list()
  for(i in indexes){
    temp <- x[[i]]
    if(ncol(temp)==nrow(mat)){
      counter <- counter+1
      result[[counter]] <- temp%*%mat
    }
  }
  
  if(counter==0){
    return(str2)
  } else {
    return(result)
  }
}

#

multiples2(foo,mat=diag(2))
multiples2(bar,mat=diag(2))
multiples2(baz,mat=diag(2))


## 11.2.3 ##

multiples3 <- function(x,mat,str1,str2){
  matrix.flags <- sapply(x,FUN=is.matrix)
  
  if(!any(matrix.flags)){
    if(missing(str1)){
      return("'str1' was missing, so this is the message")
    } else {
      return(str1)
    }
  }
  
  indexes <- which(matrix.flags)
  counter <- 0
  result <- list()
  for(i in indexes){
    temp <- x[[i]]
    if(ncol(temp)==nrow(mat)){
      counter <- counter+1
      result[[counter]] <- temp%*%mat
    }
  }
  
  if(counter==0){
    if(missing(str2)){
      return("'str2' was missing, so this is the message")
    } else {
      return(str2)
    }
  } else {
    return(result)
  }
}

#

multiples3(foo,diag(2))
multiples3(bar,diag(2))
multiples3(baz,diag(2))


## 11.2.4 ##

myfibplot <- function(thresh,plotit=TRUE,...){
  fibseq <- c(1,1)
  counter <- 2
  repeat{
    fibseq <- c(fibseq,fibseq[counter-1]+fibseq[counter])
    counter <- counter+1
    if(fibseq[counter]>thresh){
      break
    }
  }
  
  if(plotit){
    plot(1:length(fibseq),fibseq,...)
  } else {
    return(fibseq)
  }
}

#

myfibplot(150)

#

myfibplot(150,type="b",pch=4,lty=2,main="Terms of the Fibonacci sequence",ylab="Fibonacci number",xlab="Term (n)")

#

unpackme <- function(...){
  x <- list(...)
  cat("Here is ... in its entirety as a list:\n")
  print(x)
  cat("\nThe names of ... are:",names(x),"\n\n")
  cat("\nThe classes of ... are:",sapply(x,class))
}

#

unpackme(aa=matrix(1:4,2,2),bb=TRUE,cc=c("two","strings"),dd=factor(c(1,1,2,1)))



################
# Section 11.3 #
################

## 11.3.1 ##

multiples_helper_ext <- function(x,matrix.flags,mat){
  indexes <- which(matrix.flags)
  counter <- 0
  result <- list()
  for(i in indexes){
    temp <- x[[i]]
    if(ncol(temp)==nrow(mat)){
      counter <- counter+1
      result[[counter]] <- temp%*%mat
    }
  }
  return(list(result,counter))
}

multiples4 <- function(x,mat,str1="no valid matrices",str2=str1){
  matrix.flags <- sapply(x,FUN=is.matrix)
  
  if(!any(matrix.flags)){
    return(str1)
  }
  
  helper.call <- multiples_helper_ext(x,matrix.flags,mat)
  result <- helper.call[[1]]
  counter <- helper.call[[2]]
  
  if(counter==0){
    return(str2)
  } else {
    return(result)
  }
}

#

multiples5 <- function(x,mat,str1="no valid matrices",str2=str1){
  matrix.flags <- sapply(x,FUN=is.matrix)
  
  if(!any(matrix.flags)){
    return(str1)
  }
  
  multiples_helper_int <- function(x,matrix.flags,mat){
    indexes <- which(matrix.flags)
    counter <- 0
    result <- list()
    for(i in indexes){
      temp <- x[[i]]
      if(ncol(temp)==nrow(mat)){
        counter <- counter+1
        result[[counter]] <- temp%*%mat
      }
    }
    return(list(result,counter))
  }	
  
  helper.call <- multiples_helper_int(x,matrix.flags,mat)
  result <- helper.call[[1]]
  counter <- helper.call[[2]]
  
  if(counter==0){
    return(str2)
  } else {
    return(result)
  }
}


## 11.3.2 ##

foo <- matrix(c(2,3,3,4,2,4,7,3,3,6,7,2),3,4)
foo

#

apply(foo,MARGIN=2,FUN=function(x){sort(rep(x,2))})


## 11.3.3 ##

myfibrec <- function(n){
  if(n==1||n==2){
    return(1)
  } else {
    return(myfibrec(n-1)+myfibrec(n-2))
  }
}

#

myfibrec(5)




##################
### CHAPTER 12 ###
##################

################
# Section 12.1 #
################

## 12.1.1 ##

warn_test <- function(x){
	if(x<=0){
		warning("'x' is less than or equal to 0 but setting it to 1 and continuing")
		x <- 1
	}
	return(5/x)
}

error_test <- function(x){
	if(x<=0){
		stop("'x' is less than or equal to 0... TERMINATE")
	}
	return(5/x)
}

#

warn_test(0)
error_test(0)

#

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

#

myfibrec2(6)
myfibrec2(-3)
myfibrec2(0)


## 12.1.2 ##

attempt1 <- try(myfibrec2(0),silent=TRUE)

#

attempt1

#

attempt2 <- try(myfibrec2(6),silent=TRUE)
attempt2

#

myfibvector <- function(nvec){
	nterms <- length(nvec)
	result <- rep(0,nterms)
	for(i in 1:nterms){
		result[i] <- myfibrec2(nvec[i])
	}
	return(result)		
}

#

foo <- myfibvector(nvec=c(1,2,10,8))
foo

#

bar <- myfibvector(nvec=c(3,2,7,0,9,13))

#

myfibvectorTRY <- function(nvec){
	nterms <- length(nvec)
	result <- rep(0,nterms)
	for(i in 1:nterms){
		attempt <- try(myfibrec2(nvec[i]),silent=T)
		if(class(attempt)=="try-error"){
			result[i] <- NA
		} else {
			result[i] <- attempt
		}
	}
	return(result)
}

#

baz <- myfibvectorTRY(nvec=c(3,2,7,0,9,13))
baz

#

attempt3 <- try(myfibrec2(-3),silent=TRUE)
attempt3

#

attempt4 <- suppressWarnings(myfibrec2(-3))
attempt4



################
# Section 12.2 #
################

## 12.2.1 ##

Sys.sleep(3)

#

sleep_test <- function(n){
	result <- 0
	for(i in 1:n){
		result <- result + 1
		Sys.sleep(0.5)
	}
	return(result)
}

#

sleep_test(8)

#

prog_test <- function(n){
	result <- 0
	progbar <- txtProgressBar(min=0,max=n,style=1,char="=")
	for(i in 1:n){
		result <- result + 1
		Sys.sleep(0.5)
		setTxtProgressBar(progbar,value=i)
	}
	close(progbar)
	return(result)
}

#

prog_test(8)


## 12.2.2 ##

Sys.time()

#

t1 <- Sys.time()
Sys.sleep(3)
t2 <- Sys.time()
t2-t1



################
# Section 12.3 #
################

## 12.3.1 ##

search()

#

foo <- c(4,1.5,3)
sum(foo)

#

sum <- function(x){
	result <- 0
	for(i in 1:length(x)){
		result <- result + x[i]^2
	}
	return(result)
}

#

sum(foo)

#

base::sum(foo)

#

rm(sum)

#

library("spatstat")
library("car")

#

cats <- "meow"

#

library("MASS")

#

search()

#

detach("package:car",unload=TRUE)
search()


## 12.3.2 ##

foo <- data.frame(surname=c("a","b","c","d"),sex=c(0,1,1,0),height=c(170,168,181,180),stringsAsFactors=F)
foo

#

attach(foo)
search()

#

surname

#

bar <- data.frame(surname=c("e","f","g","h"),sex=c(1,0,1,0),weight=c(55,70,87,79),stringsAsFactors=F)
bar

#

attach(bar)

#

search()

#

height

#

detach(foo)
search()

