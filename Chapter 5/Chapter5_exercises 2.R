#a
dframe<-data.frame(person=c("Stan","Francine","Steve","Roger","Hayley","Klaus"),
                   sex=factor(c("M","F","M","M","F","M")),
                   funny=factor(c("High","Med","Low","High","Med","Med"),
                                levels = c("Low","Med","High"),ordered = T),
                   stringsAsFactors = F)
#b
dframe<-cbind(dframe,age=c(41,41,15,1600,21,60))

#c person, age, sex, funny
c<-dframe[c("person","age","sex","funny")]

#d
mydata<-data.frame(person=c("Peter","Lois","Meg","Chris","Stewie","Brian"),
                    age=c(42,40,17,14,1,7),
                    sex=factor(c("M","F","F","M","M","M")),
                    funny=factor(c("High","High","Low","Med","High","Med"),
                                 levels = c("Low","Med","High"),ordered = T),
                    age.mon=c(504,480,204,168,12,84),stringsAsFactors = F)
########################EXISTE OUTRO JEITO DE REMOVER UMA VARIÁVEL?
mydata2<-mydata[-5]

#e
mydataframe<-rbind(mydata2,dframe)

#f
mydataframe[mydataframe$sex=="M" & mydataframe$funny>="Med",c("person","age")]

#g
nameswiths<-mydataframe[substr(mydataframe$person,start=0,stop=1)=="S",1:ncol(mydataframe)]