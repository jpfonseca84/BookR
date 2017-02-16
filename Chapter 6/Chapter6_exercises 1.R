#a
foo<-c(13563,-14156,-14319,16981,12921,11979,9568,8833,-12968,8133)
#ai
foo[is.finite(foo^75)]
#aii
foo[-foo^75==-Inf]

#b
bar<-matrix(c(77875.40,-35466.25,-39803.81,27551.45,-73333.85,55976.34,23764.30,
              36599.69,76694.82,-36478.88,-70585.69,47032.00),3,4)
#bi
which(is.nan(bar^65/Inf),arr.ind = T)

#bii
!is.nan(bar^67+Inf)==(bar^67!=-Inf)

#biii
bar[bar^67==-Inf|is.finite(bar^67)]