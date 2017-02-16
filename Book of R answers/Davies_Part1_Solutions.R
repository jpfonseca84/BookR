
######################################
#### SUGGESTED EXERCISE SOLUTIONS ####
######################################

#########
## 2.1 ##
#########
#(a)
(6*2.3+42)/(3^(4.2-3.62))
#(b)
(-4)^2+2
#(c)
sqrt(x=0.5*((25.2+15+16.44+15.3+18.6)/5))
#(d)
log(x=0.3)
#(e)
exp(x=-1.203973)
#(f)
-0.00000000423546322

#########
## 2.2 ##
#########
#(a)
foo <- 3^2*4^(1/8)
#(b)
foo <- foo/2.33
foo
#(c)
bar <- -8.2e-13
#(d)
foo*bar

#########
## 2.3 ##
#########
#(a)
foo <- seq(from=5,to=-11,by=-0.3)
foo
#(b)
foo <- sort(x=foo,decreasing=FALSE)
foo
#(c)    
bar <- rep(x=c(-1,3,-5,7,-9),times=2,each=10)
sort(x=bar,decreasing=TRUE)
#(d)
baz <- c(6:12,rep(5.3,times=3),-3,seq(from=102,to=length(bar),length.out=9))
baz
#(e)
length(baz)

#########
## 2.4 ##
#########
#(a)
foo <- c(seq(from=3,to=6,length.out=5),rep(c(2,-5.1,-33),times=2),7/42+2)
foo
#(b)
bar <- foo[c(1,length(x=foo))]
bar
#(c)
baz <- foo[-c(1,length(x=foo))]
baz
#(d)
c(bar[1],baz,bar[2])
#(e)
foo <- sort(x=foo,decreasing=FALSE)
foo
#(f)
foo[length(x=foo):1]
sort(x=foo,decreasing=TRUE)
#(g)
baz[c(rep(x=3,times=3),rep(x=6,times=4),length(x=baz))]
#(h)
qux <- foo
qux[c(1,5:7,12)] <- 99:95
qux

#########
## 2.5 ##
#########
#(a)
c(2,0.5,1,2,0.5,1,2,0.5,1)/c(2,0.5,1)
#(b)
faren <- c(45,77,20,19,101,120,212)
cel <- 5/9*(faren-32)
cel
#(c)
foo <- rep(x=c(2,4,6),times=2)*rep(x=c(1,2),each=3)
foo
#(d)
foo[2:5] <- c(-0.1,-100)
foo

#########
## 3.1 ##
#########
#(a)
mymat <- matrix(data=c(4.3,3.1,8.2,8.2,3.2,0.9,1.6,6.5),nrow=4,ncol=2,byrow=TRUE)
mymat
#(b)
dim(mymat[-2,])
#(c)
mymat[,2] <- sort(x=mymat[,2])
mymat
#(d)
mymat[-4,-1]
matrix(data=mymat[-4,-1])
#(e)
mymat2 <- mymat[3:4,]
mymat2
#(f)
mymat[c(4,1),2:1] <- -0.5*diag(mymat2)

#########
## 3.2 ##
#########
#(a)
2/7*(cbind(c(1,2,7),c(2,4,6))-cbind(c(10,30,50),c(20,40,60)))
#(b)
A <- matrix(data=c(1,2,7))
B <- matrix(data=c(3,4,8))
##(i) Not possible
##(ii)
t(A)%*%B
##(iii)
t(B)%*%(A%*%t(A))
##(iv) Not possible
##(v)
solve(B%*%t(B)+A%*%t(A)-100*diag(3))
#(c)
A <- rbind(c(2,0,0,0),c(0,3,0,0),c(0,0,5,0),c(0,0,0,-1))
solve(A)%*%A-diag(4)

#########
## 3.3 ##
#########
#(a)
AR <- array(data=seq(from=4.8,to=0.1,length.out=48),dim=c(4,2,6))
AR
#(b)
BR <- AR[c(4,1),2,]
BR
#(c)
CR <- array(data=rep(x=BR[2,],times=4),dim=c(2,2,2,3))
CR
#(d)
DR <- AR[,,-6]
DR
#(e)
DR[c(2,4),2,c(1,3,5)] <- -99
DR

#########
## 4.1 ##
#########
#(a)
foo <- c(6,9,7,3,6,7,9,6,3,6,6,7,1,9,1)
foo
foo==6
foo>=6
foo<(6+2)
foo!=6
#(b)
bar <- foo[-(1:3)]
bar <- array(data=bar,dim=c(2,2,3))
bar
bar<=(6/2+4)
(bar+2)<=(6/2+4)
#(c)
diag(10)==0
#(d)
any(bar<=(6/2+4))
all(bar<=(6/2+4))
any((bar+2)<=(6/2+4))
all((bar+2)<=(6/2+4))
#(e)
any(diag(diag(10)==0))

#########
## 4.2 ##
#########
#(a)
foo <- c(7,1,7,10,5,9,10,3,10,8)
(foo>5)|(foo==2)
#(b)
bar <- c(8,8,4,4,5,1,5,6,6,8)
(bar<=6)&(bar!=4)
#(c)
((foo>5)|(foo==2))&((bar<=6)&(bar!=4))
#(d)
baz <- foo+bar
baz
##(i)
(baz>=14)&(baz!=15)
##(ii)
(baz/foo>4)|(baz/foo<=2)
#(e)
(foo>5)||(foo==2)
(bar<=6)&&(bar!=4)
((foo>5)||(foo==2))&&((bar<=6)&&(bar!=4))
(baz>=14)&&(baz!=15)
(baz/foo>4)||(baz/foo<=2)

#########
## 4.3 ##
#########
#(a)
foo <- c(7,5,6,1,2,10,8,3,8,2)
##(i)
bar <- foo[foo>=5]
##(ii)
foo[-which(x=foo>=5)]
#(b)
baz <- matrix(data=bar,nrow=2,ncol=3,byrow=T)
##(i)
baz[baz==8] <- baz[1,2]^2
##(ii)
all(baz<=25&baz>4)
#(c)
qux <- array(data=c(10,5,1,4,7,4,3,3,1,3,4,3,1,7,8,3,7,3),dim=c(3,2,3))
##(i)
which(x=qux==3|qux==4,arr.ind=T)
##(ii)
qux[qux<3|qux>=7] <- 100
#(d)
foo[c(F,T)]
foo[c(0,1)]

#########
## 4.4 ##
#########
#(a)
cat("\"The quick brown fox\n\tjumped over\n\t\tthe lazy dogs\"")
#(b)
num1 <- 4
num2 <- 0.75
paste("The result of multiplying",num1,"by",num2,"is",num1*num2)
#(c)
sub(pattern="tdavies",replacement="aschwarzenegger",x="/Users/tdavies/Documents/RBook")
#(d)
bar <- "How much wood could a woodchuck chuck"
##(i)
baz <- paste(bar,"if a woodchuck could chuck wood")
##(ii)
gsub(pattern="wood",replacement="metal",x=baz)
#(e)
foo <- "Two 6-packs for $12.99"
#(i)
substr(x=foo,start=5,stop=10)=="6-pack"
#(ii)
substr(x=foo,start=19,stop=19) <- "0"
foo

#########
## 4.5 ##
#########
#(a)
party <- rep("National",20)
party[c(1,4,12,15,16,19)] <- "Labour"
party[c(6,9,11)] <- "Greens"
party[c(10,20)] <- "Other"
party
sex <- rep("M",20)
sex[c(1,5:7,12,14:16)] <- "F"
sex
#(b)
sex.fac <- factor(x=sex)
sex.fac
party.fac <- factor(x=party,levels=c("National","Labour","Greens","Maori","Other"))
party.fac # Should not use ordered=TRUE, there is no 'natural' or 'low-to-high' ordering here. Factor levels are arranged in the order specified in the 'levels' argument.
#(c)
##(i)
party.fac[sex.fac=="M"]
##(ii)
sex.fac[party.fac=="National"]
#(d)
sex.newvals <- factor(x=c("M","M","F","F","F","M"))
sex.fac <- factor(x=levels(sex.fac)[c(sex.fac,sex.newvals)])
sex.fac
party.newvals <- factor(x=c("National","Maori","Maori","Labour","Greens","Labour"),levels=levels(party.fac))
party.fac <- factor(x=levels(party.fac)[c(party.fac,party.newvals)])
party.fac
#(e)
conf <- c(93,55,29,100,52,84,56,0,33,52,35,53,55,46,40,40,56,45,64,31,10,29,40,95,18,61)
conf.fac <- cut(x=conf,breaks=c(0,30,70,100),include.lowest=TRUE,labels=c("Low","Moderate","High"))
#(f)
conf.fac[party.fac=="Labour"]
conf.fac[party.fac=="National"] # Theres an indication that those who identify as "Labour" have greater confidence than those who identify as "National" when it comes to guessing how well Labour will do in the next election.

#########
## 5.1 ##
#########
#(a)
foo <- list(seq(from=-4,to=4,length=20),matrix(c(F,T,T,T,F,T,T,F,F),nrow=3,ncol=3),c("don","quixote"),factor(x=c("LOW","MED","LOW","MED","MED","HIGH")))
##(i)
foo[[2]][2:1,2:3]
##(ii)
foo[[3]][1] <- sub(pattern="d",replacement="D",x=foo[[3]][1])
foo[[3]][2] <- sub(pattern="q",replacement="Q",x=foo[[3]][2])
cat("\"Windmills! ATTACK!\"\n\t-\\",foo[[3]][1]," ",foo[[3]][2],"/-",sep="")
##(iii)
foo[[1]][foo[[1]]>1]
##(iv)
which(x=foo[[4]]=="MED")
#(b)
bar <- list(facs=foo[[4]],nums=c(3,2.1,3.3,4,1.5,4.9),oldlist=foo[1:3])
##(i)
bar$facs[bar$nums>=3]
##(ii)
bar$flags <- rep(x=bar$oldlist[[2]][,3],times=2)
##(iii)
bar$nums[!bar$flags]
##(iv)
bar$oldlist[[3]] <- "Don Quixote"

#########
## 5.2 ##
#########
#(a)
dframe <- data.frame(person=c("Stan","Francine","Steve","Roger","Hayley","Klaus"),sex=factor(x=c("M","F","M","M","F","M")),funny=factor(x=c("High","Med","Low","High","Med","Med"),levels=c("Low","Med","High")),stringsAsFactors=T)
dframe
#(b)
dframe$age <- c(41,41,15,1600,21,60)
dframe
#(c)
dframe <- dframe[,c(1,4,2,3)]
dframe
#(d)
mydata2 <- mydata[,-5] #(Assuming the presence of the 'mydata' object as left in Section 5.2.2)#
#(e)
mydataframe <- rbind(mydata2,dframe)
mydataframe
#(f)
mydataframe[mydataframe$sex=="F"&(mydataframe$funny=="Med"|mydataframe$funny=="High"),c("person","age")]
#(g)
mydataframe[substr(x=mydataframe$person,start=1,stop=1)=="S",]

#########
## 6.1 ##
#########
#(a)
foo <- c(13563,-14156,-14319,16981,12921,11979,9568,8833,-12968,8133)
##(i)
foo[is.finite(foo^75)]
##(ii)
foo[-which(foo^75==-Inf)]
#(b)
bar <- matrix(c(77875.4,-35466.25,-39803.81,27551.45,-73333.85,55976.34,23764.3,36599.69,76694.82,-36478.88,-70585.69,47032),nrow=3,ncol=4)
##(i)
which(is.nan(bar^65/Inf),arr.ind=T)
##(ii)
bar[!is.nan(bar^67+Inf)]
bar[bar^67!=-Inf]
##(iii)
bar[bar^67==-Inf|is.finite(bar^67)]

#########
## 6.2 ##
#########
#(a)
foo <- c(4.3,2.2,NULL,2.4,NaN,3.3,3.1,NULL,3.4,NA)
##(i)
length(x=foo)
##(ii)
which(x=is.na(x=foo))
##(iii)
is.null(x=foo)
##(iv)
is.na(x=foo[8])+4/NULL
#(b)
bar <- list(c(7,7,NA,3,NA,1,1,5,NA))
##(i)
names(bar) <- "alpha"
##(ii)
is.null(x=bar$beta)
##(iii)
bar$beta <- which(x=is.na(x=bar$alpha))
bar

#########
## 6.3 ##
#########
#(a)
##(i)
foo <- array(data=1:36,dim=c(3,3,4))
foo
class(foo)
attributes(foo) #implicit
##(ii)
bar <- as.vector(foo)
bar
class(bar)
attributes(bar) #implicit
##(iii)
baz <- as.character(bar)
baz
class(baz)
attributes(baz) #implicit
##(iv)
qux <- as.factor(baz)
qux
class(qux)
attributes(qux) #explicit
##(v)
quux <- bar+c(-0.1,0.1)
quux
class(quux)
attributes(quux) #implicit
#(b)
foo.sum <- is.numeric(foo)+is.integer(foo)
bar.sum <- is.numeric(bar)+is.integer(bar)
baz.sum <- is.numeric(baz)+is.integer(baz)
qux.sum <- is.numeric(qux)+is.integer(qux)
quux.sum <- is.numeric(quux)+is.integer(quux)
myfac <- factor(x=c(foo.sum,bar.sum,baz.sum,qux.sum,quux.sum),levels=c(0,1,2))
myfac
as.numeric(myfac)
#(c)
foo <- matrix(data=2:13,nrow=3,ncol=4)
foo
as.character(as.vector(t(foo)))
#(d)
foo <- cbind(c(34,23,33,42,41),c(0,1,1,0,0),c(1,2,1,1,2))
foo
##(i)
foo <- as.data.frame(foo)
foo
##(ii)
foo[,2] <- as.logical(foo[,2])
foo
##(iii)
foo[,3] <- as.factor(foo[,3])
foo
foo$V3

#########
## 7.1 ##
#########
#(a)
plot(-3:3,7:13,type="n",xlab="",ylab="")
text(x=0,y=10,labels="SOMETHING\nPROFOUND")
abline(v=c(-3,3),lty=2,lwd=4,col=8)
abline(h=c(7,13),lty=2,lwd=4,col=8)
arrows(x0=c(-2.5,-2.5,-2.5,2.5,2.5,2.5),y0=c(7.5,10,12.5,7.5,10,12.5),x1=c(-1,-1,-1,1,1,1),y1=c(9.5,10,10.5,9.5,10,10.5))
#(b)
w <- c(55,85,75,42,93,63,58,75,89,67)
h <- c(161,185,174,154,188,178,170,167,181,178)
s <- c("female","male","male","female","male","male","female","male","male","female")
plot(w,h,type="n",xlab="Weight (kg)",ylab="Height (cm)",main="Height against weight for 10 people")
points(w[s=="male"],h[s=="male"],pch=19)
points(w[s=="female"],h[s=="female"],pch=3,col=2)
legend("topleft",legend=c("male","female"),pch=c(19,3),col=c(1,2))

#########
## 7.2 ##
#########
w <- c(55,85,75,42,93,63,58,75,89,67)
h <- c(161,185,174,154,188,178,170,167,181,178)
s <- factor(c("female","male","male","female","male","male","female","male","male","female"))
qplot(w,h,color=s,shape=s,xlab="Weight (kg)",ylab="Height (cm)",main="Height against weight for 10 people") + geom_point(size=4)

##########
## 8.1 ##
##########
#(a)
##(i)
write.table(x=quakes[quakes$mag>=5,],file="/Users/tdavies/q5.txt",sep="!",row.names=F)
##(ii)
q5.dframe <- read.table(file="/Users/tdavies/q5.txt",sep="!",header=T)
#(b)
install.packages("car")
library("car")
data(Duncan)
##(i)
plot(Duncan$education[Duncan$prestige<=80],Duncan$income[Duncan$prestige<=80],xlim=c(0,100),ylim=c(0,100),xlab="Education",ylab="Income")
points(Duncan$education[Duncan$prestige>80],Duncan$income[Duncan$prestige>80],pch=19,col="blue")
##(ii)
png("/Users/tdavies/dunc.png",width=500,height=500)
plot(Duncan$education[Duncan$prestige<=80],Duncan$income[Duncan$prestige<=80],xlim=c(0,100),ylim=c(0,100),xlab="Education",ylab="Income")
points(Duncan$education[Duncan$prestige>80],Duncan$income[Duncan$prestige>80],pch=19,col="blue")
legend("topleft",legend=c("prestige > 80","prestige <= 80"),pch=c(19,1),col=c("blue","black"))
dev.off()
#(c)
exer <- list(quakes,q5.dframe,Duncan)
##(i)
dput(x=exer,file="/Users/tdavies/Exercise8-1Data.txt")
##(ii)
list.of.dataframes <- dget("/Users/tdavies/Exercise8-1Data.txt")
list.of.dataframes
#(d)
x <- 1:20
y <- c(-1.49,3.37,2.59,-2.78,-3.94,-0.92,6.43,8.51,3.41,-8.23,-12.01,-6.58,2.87,14.12,9.63,-4.58,-14.78,-11.67,1.17,15.62)
ptype <- rep(NA,length(x=x))
ptype[y>=5] <- "too_big"
ptype[y<=-5] <- "too_small"
ptype[(x>=5&x<=15)&(y>-5&y<5)] <- "sweet"
ptype[(x<5|x>15)&(y>-5&y<5)] <- "standard"
ptype <- factor(x=ptype)
qplot(x,y,color=ptype,shape=ptype) + geom_point(size=4) + geom_line(mapping=aes(group=1),color="black",lty=2) + geom_hline(mapping=aes(yintercept=c(-5,5)),color="red") + geom_segment(mapping=aes(x=5,y=-5,xend=5,yend=5),color="red",lty=3) + geom_segment(mapping=aes(x=15,y=-5,xend=15,yend=5),color="red",lty=3)
ggsave(filename="/Users/tdavies/elaborateqplot.tiff")
