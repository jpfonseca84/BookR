
# a -----------------------------------------------------------------------

vec1<- c(2,1,1,3,2,1,0)
vec2<- c(3,8,2,2,0,0,0)
#i
if((veic1[1]+vec2[2])==10){cat("Print me!")}
#ii
if(vec1[1]>=2&&vec2[1]>=2){cat("Print me!")}
#iii
if(all((vec2-vec1)[c(2,6)]<7)){cat("Print me!")}
#iv
if(!is.na(vec2[3])){cat("Print me!")}
# b -----------------------------------------------------------------------

result<-ifelse(vec1+vec2>3,vec1*vec2,vec1+vec2)

# c -----------------------------------------------------------------------

#Code
firstletter<-substr(diag(mymat),1,1)
hasg<-ifelse(firstletter=="G"|firstletter=="g","HERE",diag(mymat))

if(any(hasg=="HERE")){
      diag(mymat)<-hasg
      
}else{
      mymat<-diag(1,dim(mymat)[1],dim(mymat)[2])
      
}

#i
mymat<-matrix(as.character(1:16),4,4)
#ii
mymat<-matrix(c("DANDALION","Hyacinthus","Gerbera","Marigold","geranium","ligularia","Pachysandra","SNAPRAGON","GLADIOLUS"),3,3)
#iii
mymat<-matrix(c("GREAT","exercises","right","here"),2,2,byrow=T)
