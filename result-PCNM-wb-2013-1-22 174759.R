##############################################################
#               wangbinzjcc
###############
# 
library(PCNM)
#
setwd("F:/lg-data/subplots-dataSets") ; dir()
#
nam.pc.fwd <- dir(path='./grid-pcnm', pattern='PCNM-fwd')
nam.fwd <- nam.pc.fwd[1]

#，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，，
ResultVarpart <- function(nam.fwd){
#
    spl.fwd <- unlist(strsplit(nam.fwd,' '))
    nam.env <- dir('./grid-env')
    path.env <- paste(spl.fwd[2], paste(spl.fwd[3:7], collapse = ' '), sep = '  ')
    nam.env00 <- nam.env[grep(path.env, nam.env)]
#
    nam.pcnm <- dir('./grid-pcnm')
    path.pcn <- paste(spl.fwd[4], 'grid', spl.fwd[2], 'Pcnm.summ vectors')
    nam.pcn00 <- nam.pcnm[grep(path.pcn, nam.pcnm)]
#
    nam.spec <- dir('./grid-spec')
    path.spe <- paste('grid.spec', paste(spl.fwd[2:7], collapse=' '))
    nam.spe00 <- nam.spec[grep(path.spe, nam.spec)]
    
###    
    spe.data <- read.csv(paste('./grid-spec/', nam.spe00, sep=''))
#
    env.data <- read.csv(paste('./grid-env/', nam.env00, sep=''))
       
###~~~~~~~~~~~~~~~~~~~~
    env11 <- env.data[,-c(1:4)]
    env12 <- with(env11, 
              data.frame(ele2 = meanelev^2, ele3 = meanelev^3
                         ,slo2 = slope^2, slo3 = slope^3
                         ,con2 = convex^2, con3 = convex^3
                         )
                  )
    env13 <- with(env11,
              data.frame(asp.sin = sin(aspect), asp.cos = cos(aspect))
                 )
    env22 <- cbind(env11[c('meanelev', 'slope','convex')], env12,env13)
###
    pcn.dat00 <- read.csv(paste('./grid-pcnm/', nam.pcn00, sep=''))
     pcn.fw <- read.csv(paste('./grid-pcnm/', nam.fwd, sep=''))
    pcnm_fwd <- pcn.dat00[,-1][, pcn.fw$order]
# 
    names(pcnm_fwd) <- gsub('vectors', 'pcnm', names(pcnm_fwd))
###~~~~~~~~~~~~~~~~~~~~
    subDat <- read.csv(paste('./sub-data/',paste(spl.fwd[3:7], collapse=' '), ' sub-data.csv', sep=''))
    aa00 <- c(dbh_sum=sum(subDat$dbh),dbh_m=mean(subDat$dbh),abu=length(subDat$sp),rich=length(unique(subDat$sp)))
#####    
    bbb <- apply(env22[c('meanelev','slope','convex','asp.sin','asp.cos')], 2, function(x){c(mean(x),sd(x))})
    bb00 <- c(bbb[1,],bbb[2,])
    names(bb00) <- paste(names(bb00), rep(c('_m','_sd'),each=5), sep='')
#####
#  partioning the varation
    LG.varp <- varpart(spe.data[,-c(1:3)], env22, pcnm_fwd) 
    LG.cc00 <- c(LG.varp$part$indfract$Adj.R.squared*100, LG.varp$part$SS.Y, LG.varp$part$n)
    names(LG.cc00) <- c('a %','b %','c %','d %','SS.Y','n')
#####
   dd00 <- as.numeric(spl.fwd[c(2,4,6,7)]) ; names(dd00) <- c('grid', 'side', 'ox', 'oy')
#     
  c(round(aa00, 2), dd00, round(bb00, 2), round(LG.cc00, 2))
# 
                                     }
#`````````````````````````````````````````````````````````````````````````

######

aaa <- sapply(nam.pc.fwd, ResultVarpart)
dput(aaa, 'result-pcnm.dput')
write.csv(t(aaa), 'result-pcnm.csv')

################################################





