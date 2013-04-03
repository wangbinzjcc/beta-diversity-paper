# ######################################################################
#   create PCNM and save their summary     wangbin 2013-1-5 22:22:35   #
# **********************************************************************
#
# ####################################################
#  create PCNM  ~~~~~~~~~~~~~~~~~~
 
require(PCNM)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#####

 
Create_XY <- function(plot.size, size.lis){
    grid.size <- size.lis[[paste('s',plot.size,sep='')]]
    lapply(grid.size, function(grid.size)seq(from=0, to=plot.size, by=grid.size))
                                          }
pgs.xy <- lapply(plot.size, Create_XY, size.lis=size.lis)
names(pgs.xy) <- plot.size
summary(pgs.xy)

 

PcnmCreatSave <- function(s.xy,nam.pcnm){ #  data.names=dat.nam0[1]
  mite.xy <-  expand.grid(x=s.xy, y=s.xy)
  xy.d1 <- dist(mite.xy)
  pcnm.result <- PCNM(xy.d1)  
  pcn.nam0 <- names(pcnm.result)[1:4]
   for(i in 1:length(pcn.nam0)){
    file0 <- paste(nam.pcnm,"Pcnm.summ",
                     pcn.nam0[i], ".csv")
    write.csv(pcnm.result[i], file=file0)
  }  }
 

setwd("F:/lg-data/subplots-dataSets/grid-pcnm")

size.lis <- list(s150=c(10, 15, 25, 30, 50),
                 s200=c(10, 20, 25, 40, 50),
                 s240=c(10, 15, 20, 30, 40),     
                 s250=c(10, 25, 50),       
                 s300=c(10, 15, 20, 25, 30, 50) 
)

plot.size <- c(150,200,240,250,300)


for(i in as.character(plot.size)){
  
   for(j in 1:length(pgs.xy[[i]])){
     nam.pcnm <- paste('plotside', i, 'grid', 
                      size.lis[[paste('s', i, sep='')]][j])
    s.xy <- pgs.xy[[i]][[j]]
    PcnmCreatSave(s.xy=s.xy, nam.pcnm=nam.pcnm)
                  }  }
















#
logic1 <- c(grep(c(" 10 m"), dat.nam0), grep(c(" 5 m"), dat.nam0))
#
dat.nam1 <- dat.nam0[-logic1]  # ??5m??10m?߶?
dat.nam2 <- dat.nam0[logic1]   # 5m??10m?߶?
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ִ??~~~~~~~~~~~
lapply(dat.nam1, PcnmCreatSave)
lapply(dat.nam2, PcnmCreatSave)
#   dir(, "Pcnm.summ")
#
######################################################################## 



#####################################################################
# ???? PCNM ??ͼ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
require(ade4)
#
( som.nam <- dir(, "vectors") )
som.pcn.vect <- read.csv(som.nam[1])
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (vect.dm <- dim(som.pcn.vect))
  len.2 <- vect.dm[2]
  xy00 <- expand.grid(1:30,1:50)*10
#~~~~~~~~
  samp.pcn <- sort(sample(1:len.2,9))

  windows(title="PCNM variables (grid)",40,30)
  par(mfrow=c(3,3),oma=c(0, 0, 0, 0) )

   for(i in samp.pcn ){
       s.value(xy00, som.pcn.vect[,i], method="greylevel"
         , csize=0.25, sub= i , csub=1*1.5
         , grid = F, clegend =  1 , origin = c(120,120)
          )           }
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
image(matrix(som.pcn.vect[,15],30,50))


############################################################################
require(vegan)
dir()


cca()

























 
