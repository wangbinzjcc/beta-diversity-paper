#######################################################################
#  save the species-grid dataframe of 83 subplot at diffrent gridsizes
#
#                          wangbinzjcc  2013-1-5 20:23:11              #
#######################################################################

setwd("F:/lg-data/subplots-dataSets/sub-data")
dir()
#```

LGdata <- lapply(dir(),read.csv)
names(LGdata) <- dir()
 

 Tab_SpeciPlot <- function(LGdat=LGdat, grid.size=grid.size){
     nam.lg <- names(LGdat)
     s.xy <- as.numeric(unlist(strsplit(nam.lg, ' '))[4:5])
     LGdat <- LGdat[[1]]
     head(LGdat)
     LGdat$x[LGdat$x>=500] <- 499.999
     LGdat$y[LGdat$y>=300] <- 299.999
     LG_tab <- table(paste((LGdat$x-s.xy[1]) %/% grid.size + 1, 
                           (LGdat$y-s.xy[2]) %/% grid.size + 1)
                , LGdat$sp)
     a <- rownames(LG_tab)
     bb <- order(ox <- as.numeric(substr(a, 1, 2)), oy <-as.numeric(substr(a, 3, 5)))
     LG_tab2 <- cbind(x=ox*grid.size+s.xy[1],
                      y=oy*grid.size+s.xy[2], 
                      LG_tab)[bb,]
     write.csv(LG_tab2, paste('grid.spec', grid.size, nam.lg))
                                                           }
##


dir.create("F:/lg-data/subplots-dataSets/grid-spec")
setwd("F:/lg-data/subplots-dataSets/grid-spec")
##


size.lis <- list(s150=c(10, 15, 25, 30, 50),
                 s200=c(10, 20, 25, 40, 50),
                 s240=c(10, 15, 20, 30, 40),     
                 s250=c(10, 25, 50),       
                 s300=c(10, 15, 20, 25, 30, 50) 
                )

plot.size <- c(150,200,240,250,300)

#
for(i in plot.size){
  grid.size <- size.lis[[paste('s', i, sep='')]]
  LGdat0 <- LGdata[grep(paste('side', i), names(LGdata))]
  for(j in 1:length(grid.size)){
    for(k in 1:length(LGdat0)){
      Tab_SpeciPlot(grid.size=grid.size[j], LGdat=LGdat0[k])
                              } }
                   }

#

############



  
  
  
  
  
  
  
  





