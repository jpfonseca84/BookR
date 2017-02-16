options(prompt="R> ")




#################
### CHAPTER 2 ###
#################

###############
# Section 2.1 #
###############

##  2.1.1 ##

2+3
14/6
14/6+5
14/(6+5)
3^2
2^3

#

sqrt(x=9)
sqrt(x=5.311)

#

10^2+3*60/8-3
5^3*(6-2)/(61-3+4)
2^(2+1)-4+64^((-2)^(2.25-1/4))
(0.44*(1-0.44)/34)^0.5


## 2.1.2 ##

log(x=243,base=3)

#

exp(x=3)

#

log(x=20.08554)


## 2.1.3 ##

2342151012900
0.0000002533



###############
# Section 2.2 #
###############

x <- -5
x
x = x + 1
x

mynumber = 45.2

y <- mynumber*x
y

ls()



###############
# Section 2.3 #
###############

## 2.3.1 ##

myvec <- c(1,3,1,42)
myvec

#

foo <- 32.1
myvec2 <- c(3,-3,2,3.45,1e+03,64^0.5,2+(3-1.1)/9.44,foo)
myvec2

#

myvec3 <- c(myvec,myvec2)
myvec3


## 2.3.2 ##

3:27

#

foo <- 5.3
bar <- foo:(-47+1.5)
bar

#

seq(from=3,to=27,by=3)

#

seq(from=3,to=27,length.out=40)

#

foo <- 5.3
myseq <- seq(from=foo,to=(-47+1.5),by=-2.4)
myseq

#

myseq2 <- seq(from=foo,to=(-47+1.5),length.out=5)
myseq2

#

rep(x=1,times=4)
rep(x=c(3,62,8.3),times=3)
rep(x=c(3,62,8.3),each=2)
rep(x=c(3,62,8.3),times=3,each=2)

#

foo <- 4
c(3,8.3,rep(x=32,times=foo),seq(from=-2,to=1,length.out=foo+1))

#

sort(x=c(2.5,-1,-10,3.44),decreasing=FALSE)

sort(x=c(2.5,-1,-10,3.44),decreasing=TRUE)

foo <- seq(from=4.3,to=5.5,length.out=8)
foo
bar <- sort(x=foo,decreasing=TRUE)
bar

sort(x=c(foo,bar),decreasing=FALSE)

#

length(x=c(3,2,8,1))

length(x=5:13)

foo <- 4
bar <- c(3,8.3,rep(x=32,times=foo),seq(from=-2,to=1,length.out=foo+1))
length(x=bar)


## 2.3.3 ##

myvec <- c(5,-2.3,4,4,4,6,8,10,40221,-8)
length(x=myvec)
myvec[1]

foo <- myvec[2]
foo

myvec[length(x=myvec)]

#

myvec.len <- length(x=myvec)
bar <- myvec[myvec.len-1]
bar

#

1:myvec.len

#

myvec[-1]

#

baz <- myvec[-2]
baz

#

qux <- myvec[-(myvec.len-1)]
qux

#

c(qux[-length(x=qux)],bar,qux[length(x=qux)])

#

length(x=qux)
qux[-length(x=qux)]

#

bar <- myvec[myvec.len-1]
bar

#

qux[length(x=qux)]

#

myvec[c(1,3,5)]

#

1:4
foo <- myvec[1:4]
foo

#

length(x=foo):2
foo[length(foo):2]

indexes <- c(4,rep(x=2,times=3),1,1,2,3:1)
indexes
foo[indexes]

#

foo[-c(1,3)]

#

bar <- c(3,2,4,4,1,2,4,1,0,0,5)
bar
bar[1] <- 6
bar

#

bar[c(2,4,6)] <- c(-2,-0.5,-1)
bar

#

bar[7:10] <- 100
bar


## 2.3.4 ##

foo <- 5.5:0.5
foo
foo-c(2,4,6,8,10,12)

#

bar <- c(1,-1)
foo*bar

#

baz <- c(1,-1,0.5,-0.5)
foo*baz

#

qux <- 3
foo+qux

#

foo

#

sum(foo)

#

prod(foo)

#

foo
foo[c(1,3,5,6)] <- c(-99,99)
foo




#################
### CHAPTER 3 ###
#################

###############
# Section 3.1 #
###############

A <- matrix(data=c(-3,2,893,0.17),nrow=2,ncol=2)
A


## 3.1.1 ##

matrix(data=c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=FALSE)

#

matrix(data=c(1,2,3,4,5,6),nrow=2,ncol=3,byrow=TRUE)


## 3.1.2 ##

rbind(1:3,4:6)

#

cbind(c(1,4),c(2,5),c(3,6))

#

mymat <- rbind(c(1,3,4),5:3,c(100,20,90),11:13)
mymat

dim(mymat)
nrow(mymat)
ncol(mymat)
dim(mymat)[2]



###############
# Section 3.2 #
###############

A <- matrix(c(0.3,4.5,55.3,91,0.1,105.5,-4.2,8.2,27.9),nrow=3,ncol=3)
A
A[3,2]


## 3.2.1 ##

A[,2]

#

A[1,]

#

A[2:3,]

A[,c(3,1)]

A[c(3,1),2:3]

#

diag(x=A)


## 3.2.2 ##

A[,-2]

#

A[-1,3:2]

#

A[-1,-2]

#

A[-1,-c(2,3)]

#

B <- A
B

#

B[2,] <- 1:3
B

#

B[c(1,3),2] <- 900
B

#

B[,3] <- B[3,]
B

#

B[c(1,3),c(1,3)] <- c(-7,7)
B

#

B[c(1,3),2:1] <- c(65,-65,88,-88)
B

#

diag(x=B) <- rep(x=0,times=3)
B



###############
# Section 3.3 #
###############

## 3.3.1 ##

A <- rbind(c(2,5,2),c(6,1,4))
A
t(A)

#

t(t(A))


## 3.3.2 ##

A <- diag(x=3)
A


## 3.3.3 ##

A <- rbind(c(2,5,2),c(6,1,4))
a <- 2
a*A


## 3.3.4 ##

A <- cbind(c(2,5,2),c(6,1,4))
A
B <- cbind(c(-2,3,6),c(8.1,8.2,-9.8))
B
A-B


## 3.3.5 ##

A <- rbind(c(2,5,2),c(6,1,4))
dim(A)
B <- cbind(c(3,-1,1),c(-3,1,5))
dim(B)

#

A%*%B

#

B%*%A


## 3.3.6 ##

A <- matrix(data=c(3,4,1,2),nrow=2,ncol=2)
A
solve(A)

#

A%*%solve(A)



###############
# Section 3.4 #
###############

## 3.4.1 ##

AR <- array(data=1:24,dim=c(3,4,2))
AR

#

BR <- array(data=rep(x=1:24,times=3),dim=c(3,4,2,3))
BR


## 3.4.2 ##

AR[2,,2]

#

AR[2,c(3,1),2]

#

AR[1,,]

#

BR[2,1,1,3]

#

BR[1,,,1]

#

BR[,,2,]

#

BR[3:2,4,,]

#

BR[2,,1,]




#################
### CHAPTER 4 ###
#################

###############
# Section 4.1 #
###############

## 4.1.1 ##

foo <- TRUE
foo
bar <- F
bar

#

baz <- c(T,F,F,F,T,F,T,T,T,F,T,F)
baz
length(x=baz)

#

qux <- matrix(data=baz,nrow=3,ncol=4,byrow=foo)
qux


## 4.1.2 ##

1==2
1>2
(2-1)<=2
1!=(2+3)

#

foo <- c(3,2,1,4,1,2,1,-1,0,3)
bar <- c(4,1,2,1,1,0,0,3,0,4)
length(x=foo)==length(x=bar)

#

foo==bar
foo<bar
foo<=bar
foo<=(bar+10)

#

baz <- foo[c(10,3)]
baz

#

foo>baz

#

foo<3

#

foo.mat <- matrix(foo,nrow=5,ncol=2)
foo.mat
bar.mat <- matrix(bar,nrow=5,ncol=2)
bar.mat

#

foo.mat<=bar.mat
foo.mat<3

#

qux <- foo==bar
qux
any(qux)
all(qux)

#

quux <- foo<=(bar+10)
quux
any(quux)
all(quux)


## 4.1.3 ##

FALSE||((T&&TRUE)||FALSE)
!TRUE&&TRUE
(T&&(TRUE||F))&&FALSE
(6<4)||(3!=1)

#

foo <- c(T,F,F,F,T,F,T,T,T,F,T,F)
foo

#

bar <- c(F,T,F,T,F,F,F,F,T,T,T,T)
bar

#

foo&bar
foo|bar

#

foo&&bar
foo||bar


## 4.1.4 ##

TRUE+TRUE
FALSE-TRUE
T+T+F+T+F+F+T

#

1&&1
1||0
0&&1


## 4.1.5 ##

myvec <- c(5,-2.3,4,4,4,6,8,10,40221,-8)

#

myvec[c(F,T,F,F,F,F,F,F,F,T)]

#

myvec<0

#

myvec[myvec<0]

#

myvec[c(T,F)]

#

myvec[(myvec>0)&(myvec<1000)]

#

myvec[myvec<0] <- -200
myvec

#

which(x=c(T,F,F,T,T))

#

which(x=myvec<0)

#

myvec[-which(x=myvec<0)]

#

A <- matrix(c(0.3,4.5,55.3,91,0.1,105.5,-4.2,8.2,27.9),nrow=3,ncol=3)
A

#

A[c(T,F,F),c(F,T,T)]

#

A<1

#

A[A<1] <- -7
A

#

A>25

#

which(x=A>25)

#

which(x=c(A[,1],A[,2],A[,3])>25)

#

which(x=A>25,arr.ind=T)



###############
# Section 4.2 #
###############

## 4.2.1 ##

foo <- "This is a character string!"
foo
length(x=foo)

#

nchar(x=foo)

#

bar <- "23.3"
bar

#

bar*2

#

"alpha"=="alpha"
"alpha"!="beta"
c("alpha","beta","gamma")=="beta"

#

"alpha"<="beta"
"gamma">"Alpha"

#

"Alpha">"alpha"
"beta">="bEtA"

#

baz <- "&4 _ 3 **%.? $ymbolic non$en$e ,; "
baz


## 4.2.2 ##

qux <- c("awesome","R","is")
length(x=qux)
qux

#

cat(qux[2],qux[3],"totally",qux[1],"!")
paste(qux[2],qux[3],"totally",qux[1],"!")

#

paste(qux[2],qux[3],"totally",qux[1],"!",sep="---")
paste(qux[2],qux[3],"totally",qux[1],"!",sep="")

#

cat("Do you think ",qux[2]," ",qux[3]," ",qux[1],"?",sep="")

#

a <- 3
b <- 4.4
cat("The value stored as 'a' is ",a,".",sep="")
paste("The value stored as 'b' is ",b,".",sep="")
cat("The result of a+b is ",a,"+",b,"=",a+b,".",sep="")
paste("Is ",a+b," less than 10? That's totally ",a+b<10,".",sep="")


## 4.2.3 ##

cat("here is a string\nsplit\tto neww\b\n\n\tlines")

#

cat("I really want a backslash: \\\nand and a double quote: \"")


## 4.2.4 ##

foo <- "This is a character string!"
substr(x=foo,start=21,stop=27)

#

substr(x=foo,start=1,stop=4) <- "Here"
foo

#

bar <- "How much wood could a woodchuck chuck"
sub(pattern="chuck",replacement="hurl",x=bar)
gsub(pattern="chuck",replacement="hurl",x=bar)



###############
# Section 4.3 #
###############

## 4.3.1 ##

firstname <- c("Liz","Jolene","Susan","Boris","Rochelle","Tim","Simon","Amy")
sex.num <- c(0,0,0,1,0,1,1,0)
sex.char <- c("female","female","female","male","female","male","male","female")

#

sex.num.fac <- factor(x=sex.num)
sex.num.fac
sex.char.fac <- factor(x=sex.char)
sex.char.fac

#

levels(x=sex.num.fac)
levels(x=sex.char.fac)

#

levels(x=sex.num.fac) <- c("1","2")
sex.num.fac

#

sex.char.fac[2:5]
sex.char.fac[c(1:3,5,8)]

#

sex.num.fac=="2"

#

firstname[sex.char.fac=="male"]


## 4.3.2 ##

mob <- c("Apr","Jan","Dec","Sep","Nov","Jul","Jul","Jun")

#

mob[2]
mob[3]
mob[2]<mob[3]

#

ms <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
mob.fac <- factor(x=mob,levels=ms,ordered=TRUE)
mob.fac

#

mob.fac[2]
mob.fac[3]
mob.fac[2]<mob.fac[3]


## 4.3.3 ##

foo <- c(5.1,3.3,3.1,4)
bar <- c(4.5,1.2)
c(foo,bar)

#

new.values <- factor(x=c("Oct","Feb","Feb"),levels=levels(mob.fac),ordered=TRUE)
new.values

#

c(mob.fac,new.values)

#

levels(mob.fac)

#

levels(mob.fac)[c(mob.fac,new.values)]

#

mob.new <- levels(mob.fac)[c(mob.fac,new.values)]
mob.new.fac <- factor(x=mob.new,levels=levels(mob.fac),ordered=TRUE)
mob.new.fac

#

Y <- c(0.53,5.4,1.5,3.33,0.45,0.01,2,4.2,1.99,1.01)

#

br <- c(0,2,4,6)
cut(x=Y,breaks=br)

#

cut(x=Y,breaks=br,right=F)

#

cut(x=Y,breaks=br,right=F,include.lowest=T)

#

lab <- c("Small","Medium","Large")
cut(x=Y,breaks=br,right=F,include.lowest=T,labels=lab)



#################
### CHAPTER 5 ###
#################

###############
# Section 5.1 #
###############

## 5.1.1 ##

foo <- list(matrix(data=1:4,nrow=2,ncol=2),c(T,F,T,T),"hello")
foo

#

length(x=foo)

#

foo[[1]]
foo[[3]]

#

foo[[1]] + 5.5
foo[[1]][1,2]
foo[[1]][2,]
cat(foo[[3]],"you!")

#

foo[[3]]
foo[[3]] <- paste(foo[[3]],"you!")
foo

#

foo[[c(2,3)]]

#

bar <- foo[c(2,3)]
bar


## 5.1.2 ##

names(foo) <- c("mymatrix","mylogicals","mystring")
foo

#

foo$mymatrix

#

foo[[1]]

#

all(foo$mymatrix[,2]==foo[[1]][,2])

#

baz <- list(tom=c(foo[[2]],T,T,T,F),dick="g'day mate",harry=foo$mymatrix*2)
baz

#

names(baz)


## 5.1.3 ##

baz$bobby <- foo
baz

#

baz$bobby$mylogicals[1:3]
baz[[4]][[2]][1:3]
baz[[4]]$mylogicals[1:3]



###############
# Section 5.2 #
###############

## 5.2.1 ##

mydata <- data.frame(person=c("Peter","Lois","Meg","Chris","Stewie"),age=c(42,40,17,14,1),sex=factor(c("M","F","F","M","M")))
mydata

#

mydata[2,2]

#

mydata[3:5,3]

#

mydata[,c(3,1)]

#

mydata$age

#

mydata$age[2]

#

nrow(mydata)
ncol(mydata)
dim(mydata)

#

mydata$person

#

mydata <- data.frame(person=c("Peter","Lois","Meg","Chris","Stewie"),age=c(42,40,17,14,1),sex=factor(c("M","F","F","M","M")),stringsAsFactors=FALSE)
mydata
mydata$person


## 5.2.2 ##

newrecord <- data.frame(person="Brian",age=7,sex=factor("M",levels=levels(mydata$sex)))
newrecord

#

mydata <- rbind(mydata,newrecord)
mydata

#

funny <- c("High","High","Low","Med","High","Med")
funny <- factor(x=funny,levels=c("Low","Med","High"))
funny

#

mydata <- cbind(mydata,funny)
mydata

#

mydata$age.mon <- mydata$age*12
mydata


## 5.2.3 ##

mydata$sex=="M"

#

mydata[mydata$sex=="M",]

#

mydata[mydata$sex=="M",-3]

#

mydata[mydata$sex=="M",c("person","age","funny","age.mon")]

#

mydata[mydata$age>10|mydata$funny=="High",]

#

mydata[mydata$age>45,]




#################
### CHAPTER 6 ###
#################

###############
# Section 6.1 #
###############

## 6.1.1 ##

foo <- Inf
foo
bar <- c(3401,Inf,3.1,-555,Inf,43)
bar
baz <- 90000^100
baz

#

qux <- c(-42,565,-Inf,-Inf,Inf,-45632.3)
qux

#

Inf*-9

#

Inf+1
4*-Inf
-45.2-Inf
Inf-45.2
Inf+Inf
Inf/23

#

-59/Inf
-59/-Inf

#

-59/0
59/0
Inf/0

#

qux
is.infinite(x=qux)
is.finite(x=qux)

#

-Inf<Inf
Inf>Inf
qux==Inf
qux==-Inf


## 6.1.2 ##

foo <- NaN
foo
bar <- c(NaN,54.3,-2,NaN,90094.123,-Inf,55)
bar

#

-Inf+Inf
Inf/Inf

#

0/0

#

NaN+1
2+6*(4-4)/0
3.5^(-Inf/Inf)

#

bar
is.nan(x=bar)
!is.nan(x=bar)
is.nan(x=bar)|is.infinite(x=bar)
bar[-(which(is.nan(x=bar)|is.infinite(x=bar)))]


## 6.1.3 ##

foo <- c("character","a",NA,"with","string",NA)
foo
bar <- factor(c("blue",NA,NA,"blue","green","blue",NA,"red","red",NA,"green"))
bar
baz <- matrix(c(1:3,NA,5,6,NA,8,NA),nrow=3,ncol=3)
baz

#

qux <- c(NA,5.89,Inf,NA,9.43,-2.35,NaN,2.10,-8.53,-7.58,NA,-4.58,2.01,NaN)
qux

#

is.na(x=qux)

#

which(x=is.nan(x=qux))

#

which(x=(is.na(x=qux)&!is.nan(x=qux)))

#

quux <- na.omit(object=qux)
quux

#

3+2.1*NA-4
3*c(1,2,NA,NA,NaN,6)
NA>76
76>NaN


## 6.1.4 ##

foo <- NULL
foo
bar <- NA
bar

#

c(2,4,NA,8)
c(2,4,NULL,8)

#

c(NA,NA,NA)
c(NULL,NULL,NULL)

#

opt.arg <- c("string1","string2","string3")
is.na(x=opt.arg)
is.null(x=opt.arg)

# 

opt.arg <- c(NA,NA,NA)
is.na(x=opt.arg)

opt.arg <- c(NULL,NULL,NULL)
is.null(x=opt.arg)

#

NULL+53
53<=NULL

#

NaN-NULL+NA/Inf

#

foo <- list(member1=c(33,1,5.2,7),member2="NA or NULL?")
foo

#

foo$member3

#

foo$member3 <- NA
foo



###############
# Section 6.2 #
###############

## 6.2.1 ##

foo <- matrix(data=1:9,nrow=3,ncol=3)
foo

attributes(foo)

#

attr(x=foo,which="dim")

#

dim(foo)

#

bar <- matrix(data=1:9,nrow=3,ncol=3,dimnames=list(c("A","B","C"),c("D","E","F")))
bar

#

attributes(bar)

#

dimnames(bar)

#

dimnames(foo) <- list(c("A","B","C"),c("D","E","F"))
foo


## 6.2.2 ##

num.vec1 <- 1:4
num.vec1
num.vec2 <- seq(from=1,to=4,length=6)
num.vec2
char.vec <- c("a","few","strings","here")
char.vec
logic.vec <- c(T,F,F,F,T,F,T,T)
logic.vec
fac.vec <- factor(c("Blue","Blue","Green","Red","Green","Yellow"))
fac.vec

#

class(num.vec1)
class(num.vec2)
class(char.vec)
class(logic.vec)
class(fac.vec)

#

num.mat1 <- matrix(data=num.vec1,nrow=2,ncol=2)
num.mat1
num.mat2 <- matrix(data=num.vec2,nrow=2,ncol=3)
num.mat2
char.mat <- matrix(data=char.vec,nrow=2,ncol=2)
char.mat
logic.mat <- matrix(data=logic.vec,nrow=4,ncol=2)
logic.mat

#

class(num.mat1)
class(num.mat2)
class(char.mat)
class(logic.mat)

#

ordfac.vec <- factor(x=c("Small","Large","Large","Regular","Small"),levels=c("Small","Regular","Large"),ordered=TRUE)
ordfac.vec
class(ordfac.vec)


## 6.2.3 ##

num.vec1 <- 1:4
num.vec1
is.integer(num.vec1)
is.numeric(num.vec1)
is.matrix(num.vec1)
is.data.frame(num.vec1)
is.vector(num.vec1)
is.logical(num.vec1)


## 6.2.4 ##

1:4+c(T,F,F,T)

#

foo <- 34
bar <- T
paste("Definitely foo: ",foo,"; definitely bar: ",bar,".",sep="")

#

as.numeric(c(T,F,F,T))
1:4+as.numeric(c(T,F,F,T))
foo <- 34
foo.ch <- as.character(foo)
foo.ch
bar <- T
bar.ch <- as.character(bar)
bar.ch
paste("Definitely foo: ",foo.ch,"; definitely bar: ",bar.ch,".",sep="")

#

as.numeric("32.4")

#

as.numeric("g'day mate")

#

as.logical(c("1","0","1","0","0"))

#

as.logical(as.numeric(c("1","0","1","0","0")))

#

baz <- factor(x=c("male","male","female","male"))
baz
as.numeric(baz)

#

qux <- factor(x=c(2,2,3,5))
qux
as.numeric(qux)

#

foo <- matrix(data=1:4,nrow=2,ncol=2)
foo
as.vector(foo)

#

bar <- array(data=c(8,1,9,5,5,1,3,4,3,9,8,8),dim=c(2,3,2))
bar

as.matrix(bar)

as.vector(bar)

#

baz <- list(var1=foo,var2=c(T,F,T),var3=factor(x=c(2,3,4,4,2)))
baz
as.data.frame(baz)

#

qux <- list(var1=c(3,4,5,1),var2=c(T,F,T,T),var3=factor(x=c(4,4,2,1)))
qux
as.data.frame(qux)




#################
### CHAPTER 7 ###
#################

###############
# Section 7.1 #
###############

foo <- c(1.1,2,3.5,3.9,4.2)
bar <- c(2,2.2,-1.3,0,0.2)
plot(foo,bar)

#

baz <- cbind(foo,bar)
baz
plot(baz)



###############
# Section 7.2 #
###############

## 7.2.1 ##

plot(foo,bar,type="l")
plot(foo,bar,type="b")


## 7.2.2 ##

plot(foo,bar,type="b",main="My lovely plot",xlab="x axis label",ylab="location y")
plot(foo,bar,type="b",main="My lovely plot\ntitle on two lines",xlab="",ylab="")


## 7.2.3 ##

plot(foo,bar,type="b",main="My lovely plot",xlab="",ylab="",col=2)
plot(foo,bar,type="b",main="My lovely plot",xlab="",ylab="",col="seagreen4")


## 7.2.4 ##

plot(foo,bar,type="b",main="My lovely plot",xlab="",ylab="",col=4,pch=8,lty=2,cex=2.3,lwd=3.3)
plot(foo,bar,type="b",main="My lovely plot",xlab="",ylab="",col=6,pch=15,lty=3,cex=0.7,lwd=2)


## 7.2.5 ##

plot(foo,bar,type="b",main="My lovely plot",xlab="",ylab="",col=4,pch=8,lty=2,cex=2.3,lwd=3.3,xlim=c(-10,5),ylim=c(-3,3))
plot(foo,bar,type="b",main="My lovely plot",xlab="",ylab="",col=6,pch=15,lty=3,cex=0.7,lwd=2,xlim=c(3,5),ylim=c(-0.5,0.2))



###############
# Section 7.3 #
###############

x <- 1:20
y <- c(-1.49,3.37,2.59,-2.78,-3.94,-0.92,6.43,8.51,3.41,-8.23,-12.01,-6.58,2.87,14.12,9.63,-4.58,-14.78,-11.67,1.17,15.62)

#

plot(x,y,type="n",main="")

abline(h=c(-5,5),col="red",lty=2,lwd=2)

segments(x0=c(5,15),y0=c(-5,-5),x1=c(5,15),y1=c(5,5),col="red",lty=3,lwd=2)

points(x[y>=5],y[y>=5],pch=4,col="darkmagenta",cex=2)

points(x[y<=-5],y[y<=-5],pch=3,col="darkgreen",cex=2)

points(x[(x>=5&x<=15)&(y>-5&y<5)],y[(x>=5&x<=15)&(y>-5&y<5)],pch=19,cex=1,col="blue")

points(x[(x<5|x>15)&(y>-5&y<5)],y[(x<5|x>15)&(y>-5&y<5)])

lines(x,y,lty=4)

arrows(x0=8,y0=14,x1=11,y1=2.5)

text(x=8,y=15,label="sweet spot")

#

legend("bottomleft",legend=c("overall process","sweet","standard","too big","too small","sweet y range","sweet x range"),pch=c(NA,19,1,4,3,NA,NA),lty=c(4,NA,NA,NA,NA,2,3),col=c("black","blue","black","darkmagenta","darkgreen","red","red"),lwd=c(1,NA,NA,NA,NA,2,2),pt.cex=c(NA,1,1,2,2,NA,NA))



###############
# Section 7.4 #
###############

## 7.4.1 ##

library("ggplot2")

#

foo <- c(1.1,2,3.5,3.9,4.2)
bar <- c(2,2.2,-1.3,0,0.2)

#

qplot(foo,bar)

#

qplot(foo,bar,main="My lovely qplot",xlab="x axis label",ylab="location y")

#

baz <- plot(foo,bar)
baz
qux <- qplot(foo,bar)
qux


## 7.4.2 ##

qplot(foo,bar,geom="blank") + geom_point() + geom_line()

#

qplot(foo,bar,geom="blank") + geom_point(size=3,shape=6,color="blue") + geom_line(color="red",linetype=2)

#

myqplot <- qplot(foo,bar,geom="blank") + geom_line(color="red",linetype=2)
myqplot + geom_point(size=3,shape=3,color="blue")
myqplot + geom_point(size=3,shape=7,color="blue")


## 7.4.3 ##

x <- 1:20
y <- c(-1.49,3.37,2.59,-2.78,-3.94,-0.92,6.43,8.51,3.41,-8.23,-12.01,-6.58,2.87,14.12,9.63,-4.58,-14.78,-11.67,1.17,15.62)

#

ptype <- rep(NA,length(x=x))
ptype[y>=5] <- "too_big"
ptype[y<=-5] <- "too_small"
ptype[(x>=5&x<=15)&(y>-5&y<5)] <- "sweet"
ptype[(x<5|x>15)&(y>-5&y<5)] <- "standard"
ptype <- factor(x=ptype)
ptype

#

qplot(x,y,color=ptype,shape=ptype)

#

qplot(x,y,color=ptype,shape=ptype) + geom_point(size=4) + geom_line(mapping=aes(group=1),color="black",lty=2) + geom_hline(mapping=aes(yintercept=c(-5,5)),color="red") + geom_segment(mapping=aes(x=5,y=-5,xend=5,yend=5),color="red",lty=3) + geom_segment(mapping=aes(x=15,y=-5,xend=15,yend=5),color="red",lty=3)




##################
### CHAPTER 8 ###
##################

###############
# Section 8.1 #
###############

## 8.1.1 ##

library(help="datasets")

#

ChickWeight[1:15,]


## 8.1.2 ##

library("tseries")
library(help="tseries")

#

data(ice.river)
ice.river[1:5,]



###############
# Section 8.2 #        ## You will need to change your 'file' directory appropriately to execute the following commands successfully ##
###############

## 8.2.1 ##

mydatafile <- read.table(file="/Users/tdavies/mydatafile.txt",header=TRUE,sep=" ",na.strings="*",stringsAsFactors=FALSE)
mydatafile

#

list.files("/Users/tdavies")

#

file.choose()

#

mydatafile <- read.table(file=file.choose(),header=TRUE,sep=" ",na.strings="*",stringsAsFactors=FALSE)

#

mydatafile$sex <- as.factor(mydatafile$sex)
mydatafile$funny <- factor(x=mydatafile$funny,levels=c("Low","Med","High"))


## 8.2.2 ##

spread <- read.csv(file="/Users/tdavies/spreadsheetfile.csv",header=FALSE,stringsAsFactors=TRUE)
spread


## 8.2.3 ##

dia.url <- "http://www.amstat.org/publications/jse/v9n2/4cdata.txt"
diamonds <- read.table(dia.url)

#

names(diamonds) <- c("Carat","Color","Clarity","Cert","Price")
diamonds[1:5,]


## 8.2.4 ##

# no R code



###############
# Section 8.3 #        ## You will need to change your 'file' directory appropriately to execute the following commands successfully ##
###############

## 8.3.1 ##

write.table(x=mydatafile,file="/Users/tdavies/somenewfile.txt",sep="@",na="??",quote=FALSE,row.names=FALSE)


## 8.3.2 ##

jpeg(file="/Users/tdavies/myjpegplot.jpeg",width=600,height=600)
plot(1:5,6:10,ylab="a nice ylab",xlab="here's an xlab",main="a saved .jpeg plot")
points(1:5,10:6,cex=2,pch=4,col=2)
dev.off()

#

pdf(file="/Users/tdavies/mypdfplot.pdf",width=5,height=5)
plot(1:5,6:10,ylab="a nice ylab",xlab="here's an xlab",main="a saved .pdf plot")
points(1:5,10:6,cex=2,pch=4,col=2)
dev.off()

#

foo <- c(1.1,2,3.5,3.9,4.2)
bar <- c(2,2.2,-1.3,0,0.2)

#

qplot(foo,bar,geom="blank") + geom_point(size=3,shape=8,color="darkgreen") + geom_line(color="orange",linetype=4)
ggsave(filename="/Users/tdavies/mypngqplot.png")



###############
# Section 8.4 #        ## You will need to change your 'file' directory appropriately to execute the following commands successfully ##
###############

somelist <- list(foo=c(5,2,45),bar=matrix(data=c(T,T,F,F,F,F,T,F,T),nrow=3,ncol=3),baz=factor(c(1,2,2,3,1,1,3),levels=1:3,ordered=T))
somelist

#

dput(x=somelist,file="/Users/tdavies/myRobject.txt")

#

newobject <- dget(file="/Users/tdavies/myRobject.txt")
newobject

