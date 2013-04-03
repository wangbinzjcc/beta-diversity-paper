#################################################################
#   wangbinzjcc 2013-1-5 9:51:43
#
#  the  second step for R code
#  Topographical-factors of sub-plots at 83 different scales 
#  at diffrent gridsizes
#################################################################
source("F:/sci-2013-wangbin/Calculate-AllTopogra-wangbin.2012-10-10 110018.r")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

size_topo.cal <- function(gridsize=gridsize, elevfile=elevfile){
  lapply(gridsize, 
         function(gridsize)AllTopograCalcResult(gridsize=gridsize, elevfile=elevfile)
        )                                                      }

elev_size_topo.cal <- function(elevfile=elevfile, gridsize=gridsize){
  lapply(elevfile, 
         function(elevfile)size_topo.cal(elevfile=elevfile, gridsize=gridsize)
         )                                                          }
#

######################################################

# ~~~ 

plot.size <- c(150,200,240,250,300)

size.lis <- list(s150=c(10, 15, 25, 30, 50),
                 s200=c(10, 20, 25, 40, 50),
                 s240=c(10, 15, 20, 30, 40),     
                 s250=c(10, 25, 50),       
                 s300=c(10, 15, 20, 25, 30, 50) 
                )

setwd("F:/lg-data/subplots-dataSets") 

nam.dir <- dir(pattern = "sub-env.csv")

elevdata <- lapply(nam.dir, read.csv) ; names(elevdata) <- nam.dir 

RESULT <- lapply(plot.size, function(plot.size){
   gridsize <- size.lis[[paste('s', plot.size, sep='')]]
   elevfile <- elevdata[grep(paste('side', plot.size), nam.dir)]
   
   elev_size_topo.cal(gridsize=gridsize, elevfile=elevfile)    
                                              }
                 )
                                                      
dput(RESULT,"RESULT.doput")

for( i in 1:length(plot.size)){
  len.j <- length(RESULT[[i]])
  for(j in 1:len.j){
    len.k <- length(RESULT[[i]][[j]])
    for(k in 1:len.k){
     write.csv(RESULT[[i]][[j]][[k]],
        paste('grid.size',size.lis[[i]][k],
           rownames(summary(RESULT[[i]]))[j]
        )
      )
                     }   }      }





####################


