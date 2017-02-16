############################## First Part ----
#a ----
data(anorexia)
alpha<-0.05
#H0: d.mu=0 // HA: d.mu>0 The dist is a Student one

t.test(anorexia[,3],
       anorexia[,2],
       alternative = "greater",
       paired=TRUE)

#there is strong evidence to accept H0, as the p.value is small.
#b ----
# Now a test for all the treatments
#Cont
t.test(anorexia[anorexia$Treat=="Cont",3],
       anorexia[anorexia$Treat=="Cont",2],
       alternative="greater",
       paired=TRUE)
# There is no statistical evidence to reject the claim that there is no 
# difference between  pre and post mean in the control group.

#CBT
t.test(anorexia[anorexia$Treat=="CBT",3],
       anorexia[anorexia$Treat=="CBT",2],
       alternative="greater",
       paired=TRUE)
# There is mild statistical evidence to accept that the post mean is greater 
# than the pre mean in the CBT group.

#FT
t.test(anorexia[anorexia$Treat=="FT",3],
       anorexia[anorexia$Treat=="FT",2],
       alternative="greater",
       paired=TRUE)

# There is strong statistical evidence to accept that in the FT group the mean 
#of post measurement is greater than the pre weight.
#c ----
head(PlantGrowth)
levels(PlantGrowth[, 2])
data.1 <- PlantGrowth[PlantGrowth$group == "ctrl", ]
data.2 <- PlantGrowth[PlantGrowth$group != "ctrl", ]
#Hypotheses--> H0: ctrlmean-trtmean=0 // HA: ctrlmean-trtmean<0 

#Rule of thumb
sd(PlantGrowth[PlantGrowth$group != "ctrl",1])/
      sd(PlantGrowth[PlantGrowth$group == "ctrl",1])
      
# The rule of thumb states that there is enought to accept both variances are
# the same for that, we well use the pooled variance estimative test
#d ----
t.test(PlantGrowth[PlantGrowth$group == "ctrl", 1],
       PlantGrowth[PlantGrowth$group != "ctrl", 1],
       alternative="less",
       var.equal = TRUE)
# The p.value ~0.41 there is no evidence to reject H0 as it is bigger than the 
#alpha 0.05. There is insufficient statistical evidence that the trtmean is 
# greater than the ctrl mean
#e ----
my.t.test <- function(x,
                      y,
                      var.equal = FALSE,
                      paired = FALSE,
                      ...) {
      if (!paired) {
            var.equal <- max(c(sd(x), sd(y))) /
                  min(c(sd(x), sd(y))) < 2
      }
      return(t.test(
            x = x,
            y = y,
            var.equal = var.equal,
            paired = paired,
            ...
      ))
}
my.t.test2 <- function(x,
                      y,
                      var.equal = FALSE,
                      paired = FALSE,
                      ...) {
      if (paired) {
            var.equal <- max(c(sd(x), sd(y))) /
                  min(c(sd(x), sd(y))) < 2
      }
      return(t.test(
            x = x,
            y = y,
            var.equal = var.equal,
            paired = paired,
            ...
      ))
}

#f ----
#i 
snacks <- c(87.7,80.01,77.28,78.76,81.52,74.2,80.71,79.5,77.87,81.94,80.7,82.32,
            75.78,80.19,83.91,79.4,77.52,77.62,81.4,74.89,82.95,73.59,77.92,77.18,
            79.83,81.23,79.28,78.44,79.01,80.47,76.23,78.89,77.14,69.94,78.54,79.7,
            82.45,77.29,75.52,77.21,75.99,81.94,80.41,77.7)
snacks2 <- c(80.22,79.73,81.1,78.76,82.03,81.66,80.97,81.32,80.12,78.98,79.21,
             81.48,79.86,81.06,77.96,80.73,80.34,80.01,81.82,79.3,79.08,79.47,
             78.98,80.87,82.24,77.22,80.03,79.2,80.95,79.17,81)
my.t.test(snacks2,snacks,alternative="greater")


#ii
men <- c(102,87,101,96,107,101,91,85,108,67,85,82)
women <- c(73,81,111,109,143,95,92,120,93,89,119,79,90,126,62,92,77,106,105,111)

my.t.test(men,women,alternative="two.sided")

#iii
rate.before <- c(52,66,89,87,89,72,66,65,49,62,70,52,75,63,65,61) 
rate.after <- c(51,66,71,73,70,68,60,51,40,57,65,53,64,56,60,59) 

my.t.test(rate.after,rate.before,alternative="less",paired=TRUE)
