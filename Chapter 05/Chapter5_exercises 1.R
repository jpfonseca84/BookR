#a
a<-list(seq(from=-4,to=4,length.out = 20),matrix(c(F,T,T,T,F,T,T,F,F),3,3),
        c("don","quixote"),factor(c("LOW","MED","LOW","MED","MED","HIGH"),
                                  levels = c("LOW","MED","HIGH")))
#ai
ai<-c(a[[2]][c(2,1),c(2,3)])

#aii
a[[3]]<-sub(pattern="quixote",a[[3]],replacement="Quixote")
a[[3]]<-sub(pattern="don",a[[3]],replacement="Don")

cat("\"Windmills! ATTACK!\"\n\t-\\",a[[3]][1]," ",a[[3]][2],"/-",sep="")

#aiii
aii<-a[[1]][a[[1]]>1]

#aiv
aiv<-which(a[[4]]=="MED")

#B
b<-list(facs=a[[1]],nums=c(3,2.1,3.3,4,1.5,4.9),oldlist=a[c(1:3)])

#bi
bi<-b$facs[b$nums>=3]

#bii
b$flags<-rep(b$oldlist[[2]][,3],length.out=6)

#biii
bii<-b$num[b$flags!=T]

#biv
b$oldlist[[3]]<-"Don Quixote"
