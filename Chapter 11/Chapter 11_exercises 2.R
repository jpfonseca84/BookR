#a code ----

accann <- function(P, i, t = 12, y, plotit = TRUE,...) {
      #To store the values per year
      yseq <- 1:y
      finalamount <- P * (1 + i / (100 * t)) ^ (t * yseq)
      
      if (plotit == T) {
            plotgraph<-plot(yseq, finalamount, type = "s",...)
      } else{
            return(finalamount)
      }
}
#questions ----
#i
accann(5000,4.4,y=10,plotit=F)[10]
#ii
accann(100,22.9,y=20)
#iii
comp1<-accann(P=100,i=22.9,t=1,y=20,plotit=F)
plotlines<-lines(1:20,comp1,col = "blue")
legend("topleft",c("monthly","Annually"),col=c("black","blue"),lty=c(1,1))

#b 
quad<-function(k1,k2,k3){
      if(any(missing(k1),missing(k2),missing(k3))){
            return("Missing Values!")
      }
      if(((k2^2)-(4*k1*k3))<0){
            return("No possible solution")
      }
      if(((k2^2)-(4*k1*k3))==0){
            return(-k2/(2*k1))
      }else{
            x1<-(-k2-((k2^2)-(4*k1*k3))^0.5)/(2*k1)
            x2<-(-k2+((k2^2)-(4*k1*k3))^0.5)/(2*k1)
            return(cat(x1,x2))
      }
}

