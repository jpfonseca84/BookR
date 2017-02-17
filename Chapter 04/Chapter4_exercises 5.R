#a
vec.sex<-c("F",rep("M",times=3),rep("F",times=3),rep("M",times=4),
           "F","M",rep("F",times=3),rep("M",times=4))
vec.par<-c("Labour","National","National","Labour","National","Greens",
           "National","National","Greens","Other","National","Labour",
           "National","National","Labour","Labour","National","National",
           "Labour","Other")

#b
fac.sex<-factor(vec.sex)
fac.par<-factor(vec.par)

#c
#i
ci<-fac.par[fac.sex=="M"]
#ii
cii<-fac.sex[fac.par=="National"]

#d
vec2.par<-c("National","Maori","Maori","Labour","Greens","Labour")
fac2.par<-factor(vec2.par)
vectemp.par<-levels(fac.par)[c(fac.par,fac2.par)]
fac3.par<-factor(vectemp.par,levels=levels(fac.par))

vec2.sex<-c("M","M","F","F","F","M")
fac2.sex<-factor(vec2.sex)
vectemp.sex<-levels(fac2.sex)[c(fac.sex,fac2.sex)]
fac3.sex<-factor(vectemp.sex,levels=levels(fac.sex))

#e
vec.conf<-c(93,55,29,100,52,84,56,0,33,52,35,53,55,46,40,40,56,45,64,31,10,
            29,40,95,8,62)
br<-c(0,30,70,100)
lb<-c("Low","Moderate","High")
fac.conf<-cut(vec.conf,breaks=br,labels=lb,include.lowest = T)

#f
#i
fi<-fac.conf[fac3.par[fac3.par=="Labour"]]
#ii
fii<-fac.conf[fac3.par[fac3.par=="National"]]
















