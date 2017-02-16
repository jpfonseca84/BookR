#a ----
InsectSprays

hist(InsectSprays$count,
     xlab="count of insects",
     main="Count of insects Histogram",
     col="blue",
     border="white")
#b ----
sumofinsects<-tapply(InsectSprays$count,InsectSprays$spray,sum)

barplot(sumofinsects,
        main="Total number of insects per spray",
        names.arg = names(sumofinsects),
        col="lightskyblue1",
        border="black"
)
