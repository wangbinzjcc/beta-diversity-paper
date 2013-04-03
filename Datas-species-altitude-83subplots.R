#####################################################################   
#          wangbinzjcc   2013-1-5 9:35:35
#
#  111.   Extracte sub-plot species datas at  83 different scales  
#  222.   Elevation-datas of sub-plots at 83 different scales
#  from LG 15 hectare plots.  
#####################################################################

#..................................................
StartXY <- function(sid0=side, plotdim=plotdim){
  expand.grid(x=seq(0,plotdim[1]-sid0,50), y=seq(0,plotdim[2]-sid0,50))                                           
                                               }
#... 
CreateLogi <- function(j=j, sid0=sid0, sta.xy=sta.xy, dat=dat){
  x0 <- sta.xy$x[j]; y0 <- sta.xy$y[j]
 logi.0 <- dat$x <= x0+sid0 & dat$x >= x0 & dat$y <= y0+sid0 & dat$y >= y0
       logi.0                                                  }
#...
staXY_CreateLogi <- function(sid0=sid0, startxy.data=startxy.data, dat=dat){
   sta.xy <- startxy.data[[as.character(sid0)]]
   lapply(1:dim(sta.xy)[1], function(j)CreateLogi(j, sid0=sid0, sta.xy=sta.xy, dat=dat)) 
                                                                           }
#```````````````````````````````````````````````````

##############################################################
##############################################################

#  111  Extracte sub-plot data sets at  83 different scales  

#####

setwd("F:/lg-data")
dir()
dir.create('subplots-dataSets')
#

### 
plotdim<- c(500, 300)
side <- c(150,200,240,250,300)
###

##########

LGdat0 <- read.csv('LGdat000add2012-11-2 124439.csv')
dat <- LGdat0[is.na(LGdat0$bra), ]
head(dat)

###
startxy.data <- lapply(side, function(sid0)StartXY(sid0, plotdim=plotdim)) 
names(startxy.data) <- side
###

###
result.side.staXY.Logi <- lapply(side, 
     function(sid0)staXY_CreateLogi(sid0, startxy.data=startxy.data, dat=dat)
                                )                                                                   
###
 
nam.sta <- names(startxy.data) 

for (i.sid in 1:length(nam.sta)){
  sss <- as.data.frame(startxy.data[[i.sid]])
  p.ss <- paste(sss[,1], sss[,2])
  for (j.sta in 1:dim(startxy.data[[i.sid]])[1]){
    logi.a <- result.side.staXY.Logi[[i.sid]][j.sta]
    dat0 <- dat[unlist(logi.a), ]
    write.csv(dat0, 
file = paste('F:/lg-data/subplots-dataSets/','side',nam.sta[i.sid],'xy',p.ss[j.sta],'sub-data.csv')
              )
                                                }
                                 }

##

##############################################################
##############################################################

##  222  Elevation-datas of sub-plots at 83 different scales

######## 
dat <- read.csv('弄岗样地 elev5m 2012-7-24 223400.csv')

###
startxy.data <- lapply(side, function(sid0)StartXY(sid0, plotdim=plotdim)) 
names(startxy.data) <- side
###

###
result.side.staXY.Logi <- lapply(side, 
                                 function(sid0)staXY_CreateLogi(sid0, startxy.data=startxy.data, dat=dat)
)                                                                   
###

for (i.sid in 1:length(nam.sta)){
  sss <- as.data.frame(startxy.data[[i.sid]])
  p.ss <- paste(sss[,1], sss[,2])
  for (j.sta in 1:dim(startxy.data[[i.sid]])[1]){
    logi.a <- result.side.staXY.Logi[[i.sid]][j.sta]
    dat0 <- dat[unlist(logi.a), ]
    write.csv(dat0, 
              file = paste('F:/lg-data/subplots-dataSets/','side',nam.sta[i.sid],'xy',p.ss[j.sta],'sub-env.csv')
    )
  }
}
##################################################################





### end






