#a
ls("package:methods")
#b ====
#bi
environment(read.table)
#bii
environment(data)
#biii
environment(matrix)
#biv
environment(jpeg)

#c ====
any(ls("package:graphics")=="smoothScatter")
