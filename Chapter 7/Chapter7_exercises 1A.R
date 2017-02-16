#A============================= 
#creating the plot in blank
plot(c(-3.25,3.25),c(6.75,13.25),type="n")
#adding the orizontal borders
abline(h=c(7,13),lty="dashed",lwd=2,col="red")
#Adding the vertical borders
segments(c(-3,3),c(6,6),c(-3,3),c(14,14),"red","dashed",lwd=2)
#ading the arrows
#FROM
arrows (c(-2.5,-2.5,-2.5,2.5,2.5,2.5),c(12.5,10,7.5,12.5,10,7.5),
        #To
        c(-1,-1,-1,1,1,1),c(10.5,10,9.5,10.5,10,9.5))
#Ads the text in the middle
text(y=10,x=0,"Será que a Erika\nvai demorar\nno trabalho?")


