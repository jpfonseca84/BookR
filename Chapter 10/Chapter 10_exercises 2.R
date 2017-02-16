
# a -----------------------------------------------------------------------
# a if statment that does the same as the switch(mynum,12,34,56,78,NA)

if(mynum==1){
      foo<-12
}else if(mynum==2){
      foo<-34
}else if(mynum==3){
      foo<-56
}else if(mynum==4){
      foo<-76
}else{
      foo<-NA
}
#i
mynum<-3
#ii
mynum<-0
# b -----------------------------------------------------------------------

#PROGRAM
if(any(doselevel=="High")){
      if(lowdose>=10){
            lowdose<-10
      }else{
            lowdose<-lowdose/2
      }
      if(meddose>=26){
            meddose<-26
      }
      if(highdose<60){
            highdose<-60
      }else{
            highdose<-highdose*1.5
      }
      dosage<-rep(lowdose,length(doselevel))
      dosage[doselevel=="Med"]<-meddose
      dosage[doselevel=="High"]<-highdose
}else{
      doselevel<-factor(doselevel[doselevel=="Low"|doselevel=="Med"],
                        labels = c("Small","Large"))
      if(lowdose<15&meddose<35){
            lowdose<-lowdose*2
            meddose<-meddose+highdose
      }
      dosage<-rep(lowdose,length(doselevel))
      dosage[doselevel=="Large"]<-meddose
}

#i 
lowdose<-12.5
meddose<-25.3
highdose<-58.1
doselevel<- factor(c("Low","High","High","High","Low","Med","Med"),
                   levels = c("Low","Med","High"),
                   ordered=T)

#ii 
lowdose<-12.5
meddose<-25.3
highdose<-58.1
doselevel<-factor(c("Low","Low","Low","Med","Low","Med","Med"),
                  levels=c("Low","Med","High"))

#iii 
lowdose<-9
meddose<-49
highdose<-61
doselevel<-factor(c("Low","Med","Med"),levels=c("Low","Med","High"))
# c ----

mynum<-0

ifelse(mynum==0,"Zero",
       switch(EXPR=mynum,"one","Two","three","four","five","six","seven","eight","nine"))
       