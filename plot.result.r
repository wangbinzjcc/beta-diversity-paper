########################################
require(vegan)
##

dir()
dat.pcn00 <- read.csv('result-pcnm2013-1-23 104431.csv')
dat.pcn <- with(dat.pcn00, dat.pcn00[grid==10 & side==150, ])
head(dat.pcn)
names(dat.pcn)
 
dat00 <- dat.pcn[, c('a..','b..','c..','d..','ss.var')]
dat01 <- dat.pcn[,c('meanelev_m', 'slope_m', 'convex_m', 'asp.sin_m', 'asp.cos_m', 'meanelev_sd', 'slope_sd', 'convex_sd', 'asp.sin_sd', 'asp.cos_sd')]
dat02 <- dat.pcn[,c('dbh_sum0', 'dbh_m', 'abu0', 'rich', 'grid', 'side',  'ox', 'oy')]
# ~~~~
rd01 <- rda(decostand(dat01, 'range'))
sum01 <- summary(rd01)$species
sum02 <- summary(rd01)$sites


image(as.matrix(cbind(x=dat02$ox, y=dat02$oy, z=sum02[,4]*100+100)))



xy <- c(x=1:10,y=1:10)
z <- rep(1:10,10)

image(x=1:10,y=1:10,z)



x11()

plot(sum01[,1:2])
text(sum01[,1:2], rownames(sum01))

x11()
plot(sum01[,2:3])
text(sum01[,2:3], rownames(sum01))

x11()
plot(sum01[,3:4])
text(sum01[,3:4], rownames(sum01))

x11()
plot(sum01[,4:5])
text(sum01[,4:5], rownames(sum01))



 dat00 <- dat[dat$grid==10,]

 for(i in 1:20){
   x11()
     plot(dat00[,i], dat00$b.., xlab=colnames(dat)[i])
               } 

#############################################################
dat01 <- decostand(dat,'range')
summary(dat01)
dat11 <- dat01[,c('a..','b..','c..','d..','ss.var')]
dat12 <- dat01[,c('dbh_sum0', 'dbh_m', 'abu0', 'rich', 'grid', 'side',  'ox', 'oy', 'meanelev_m', 'slope_m', 'convex_m', 'asp.sin_m', 'asp.cos_m', 'meanelev_sd', 'slope_sd', 'convex_sd', 'asp.sin_sd', 'asp.cos_sd')]

dat.rd <- rda(dat12)
names(summary(dat.rd))
p.rd <- summary(dat.rd)$species
plot(p.rd[,1], p.rd[,2])
text(p.rd[,1], p.rd[,2], rownames(as.data.frame(p.rd)))

rownames(as.data.frame(p.rd))[order(p.rd[,4])]


x11()
biplot(dat.rd)
dat01 <- as.data.frame(summary(dat.rd)$sites)
text(dat01$PC1, dat01$PC2, rownames(dat01))
###
yy <- dat11$d..
lm04 <- lm(yy ~ .,dat01)
summary(lm04)



xx <- dat12[,19]
lm00 <- lm(yy ~ xx,data=dat12)
lm01 <- lmer(yy ~ xx + (1|side),data=dat12)
lm02 <- lmer(yy ~ xx + (1|grid),data=dat12)
lm03 <- lmer(yy ~ xx + (1|side) + (1|grid) ,data=dat12)

AIC(lm00)
AIC(lm01)
AIC(lm02)
AIC(lm03)

x11()
plot(yy,xx)


summary(lm04)
summary(lm00)
summary(lm01)
summary(lm02)
summary(lm03)







names(summary(lm00)[[4]][,4])[summary(lm00)[[4]][,4] < 0.1]

require(lme4)
lm01 <- lmer(dat11$d.. ~ meanelev_m + convex_m + asp.sin_m + asp.cos_m + asp.sin_sd + asp.cos_sd + (1|side), dat12)
lm01

## linear mixed models
(fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy))
(fm2 <- lmer(Reaction ~ Days + (1|Subject) + (0+Days|Subject), sleepstudy))
anova(fm1, fm2)





###


(dat.rd <- rda(dat11, dat12))



plot(dat.rd)


pca.sum <- summary(dat.rd)$species
text(pca.sum[,1:2], colnames(dat11))

#############################################################
























