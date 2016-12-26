## CLIMATE PAPER ROUND 2.
#A. FIRST NEED TO KNOW WHICH RASTERS ARE EMPTY
#B. THEN NEED TO REPROJECT ON THEM BASED ON A MODEL OBJECT

###TESTING IF RASTERS ARE EMPTY
library(raster)
setwd("/Volumes/SeaGate/ClimatePaperCM2.6/Species_Projections_all")
blank=list()
fileslist=list.files()
for(f in fileslist){
  path=paste(getwd(),"/",f,sep="")
  #print(path)
  small_list=list.files(path,full.names = TRUE)
  for(tif in small_list){
    if((file.info(tif)$size<10000)==TRUE){
      blank=list(blank,tif)
    }
  }
}

final_list=unlist(blank)

