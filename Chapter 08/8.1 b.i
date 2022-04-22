#using base R graphics to create a blank canvas for the plot
plot(Duncan$education, Duncan$income, xlim = c(0, 100), ylim=c(0, 100), xlab = "Education", ylab="Income", type="n")
#adding points for prestige<=80
points(Duncan[ , 3][Duncan[ , 4]<=80], Duncan[ , 2][Duncan[ , 4]<=80], pch=1)
#adding points for prestige>80
points(Duncan[ , 3][Duncan[ , 4]>80], Duncan[ , 2][Duncan[ , 4]>80], pch=20, col="blue")
#adding appropriate legend
legend(0, 100, legend=c("Least prestigious", "Most prestigious"), col=c("black", "blue"), pch=c(1, 20), cex=0.5)
