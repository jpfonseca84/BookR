#The First Part
#a ----
Galileu<-data.frame(Iniheight=c(1000,800,600,450,300,200,100),
                    Dist=c(573,534,495,451,395,337,257))
plot(Dist~Iniheight,
     data=Galileu,
     xlab="Initial Height",
     ylab="Distance",
     main="Distance traveled vs. Initial height")

#b -------------------------------

#i
galfit.2<-lm(Dist~Iniheight+I(Iniheight^2),data=Galileu)
summary(galfit.2)

#ii
galfit.3<-lm(Dist~Iniheight+I(Iniheight^2)+I(Iniheight^3),data=Galileu)
summary(galfit.3)
galfit.4<-lm(Dist~Iniheight+I(Iniheight^2)+I(Iniheight^3)+I(Iniheight^4),
             data=Galileu)
summary(galfit.4)

#Increasing the number of powers in the linear regression, we see that we have 
#an improovment in the prediction model, The model get to answer 99.99% of the
#response variable. altough, the new variables tend to loose statistical 
#relevance

#c ------------------------------

galseq<- seq(min(Galileu[,1])-50,max(Galileu[,1]+50),length.out = 1000)
galfit.4.pred <- predict(
      galfit.4,
      newdata = data.frame(Iniheight = galseq),
      interval = "confidence",
      level = 0.9
)
#The plot
plot(Dist~Iniheight,
     data=Galileu,
     xlab="Initial Height",
     ylab="Distance",
     main="Distance traveled vs. Initial height")
lines(galseq,
      galfit.4.pred[, 1],
      lwd = 2,
      col = "blue") 
lines(galseq,
      galfit.4.pred[, 2],
      lwd = 2,
      col = "red")
lines(galseq,
      galfit.4.pred[, 3],
      lwd = 2,
      col = "green")

legend("topleft",
       col="blue",
       lwd=2,
       legend="Order 4")

summary(gal.pred)

#The second Part ----
install.packages("faraway")
library("faraway")
?trees
#d ----
names(trees)
plot(Volume~Girth,data=trees)

#e -------------------
#Quadratic fit model for VOlume
tree.quad.fit<-lm(Volume~Girth+I(Girth^2),data=trees)
summary(tree.quad.fit) #"Mean volume" = 10.79 - 2.09*"girth" + 0.254*"girth^2"

tree.log.fit<-lm(log(Volume)~log(Girth),
                     data=trees)
summary(tree.log.fit)
#Model equation is -2.35332 +219997*log(Girth)

#The models have a very strong R-Squared value, with the prediction explaining 
#more than 95% of the variance. Girth appears to be a good predictor for volume.

#f ------------------------
tree.seq<-seq(min(trees[,1]),max(trees[,1]),length.out = 100)

tree.quad.pred<-predict(tree.quad.fit,
                        newdata = data.frame(Girth=tree.seq),
                        interval="predict")
tree.log.pred <- predict(tree.log.fit,
                         newdata = data.frame(Girth = tree.seq),
                         interval = "predict")

plot(Volume~Girth,data=trees)

lines(tree.seq,tree.quad.pred[,1],lwd=2,col="blue",lty=3)
lines(tree.seq,tree.quad.pred[,2],lwd=0.5,col="blue",lty=3)
lines(tree.seq,tree.quad.pred[,3],lwd=0.5,col="blue",lty=3)

lines(tree.seq,exp(tree.log.pred[,1]),lwd=2,col="green",lty=4)
lines(tree.seq,exp(tree.log.pred[,2]),lwd=0.5,col="green",lty=4)
lines(tree.seq,exp(tree.log.pred[,3]),lwd=0.5,col="green",lty=4)
#Although the models themselves are very similar, confidence interval for the 
#log one is much wider than the quadratic, for higher values. To define the best
#model, more information will be needed.

#Third Part ----
#g ----
library("MASS")
names(mtcars)
carsfit<-lm(mpg~hp+wt+disp,data=mtcars)
summary(carsfit)

#h ----
carsfit2<-lm(I(1/mpg)~hp+wt+disp,data=mtcars)
summary(carsfit2)
#the model that explains a great amount of the variation in this example is the 
#one with the transformation of the variable. as it's R-Squared presents that 
#85% of the variance is explaines by the model. Instead of the 83% previously
# achieved.