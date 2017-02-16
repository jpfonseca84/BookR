#a
#Remove comment below to print answer
#cat("The quick brown fox", "\n\tjumped over","\n","\t\tthe lazy dogs",sep="")

#b
num1<-4;num2<-0.75
b<-paste("\"The result of multiplying",num1,"by",num2,"is",num1*num2)


#c
c1<-"/Users/tdavies/Documents/RBook/"
c<-sub("tdavies","JPfonseca",c1)

#d
bar<-"How much wood could a woodchuck chuck"
#i
di<-paste(bar,"if a woodchuck could chuck wood.")
#ii
dii<-gsub("wood","metal",di)

#e
e<-"Two 6-packs for $12.99"
#i
ei<-substr(e,start = 5,stop=10)=="6-pack"
#eii
ei<-sub("12.99","10.99",e)