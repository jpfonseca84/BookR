#a 
round(mean(quakes$depth>=300),2)

#b
mean(quakes$mag[quakes$depth>=300])
median(quakes$mag[quakes$depth>=300])

#c
for (i in levels(chickwts$feed)) {
      cat("Feed ",i,"has the average wheight of: ",
          mean(chickwts$weight[chickwts$feed==i]),".\n",sep="")
}

#d Variables: count is numerical-discrete, spray is categorical-nominal

#e
insecttable<-table(InsectSprays$count)
insecttable[insecttable==max(insecttable)]

#f
tapply(InsectSprays$count, InsectSprays$spray, sum)

#g
for (i in levels(InsectSprays$spray)) {
      cat("Spray ",i,"has a mean of: ",
          round(mean(InsectSprays$count[InsectSprays$spray == i] >= 5)*100),
          "% when counting only >=5.\n",sep="")
}


#h
tapply(InsectSprays$count, 
       InsectSprays$spray, 
       function(x){round(mean(x>=5)*100,2)})




