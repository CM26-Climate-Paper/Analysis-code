######### new script to display the temporal trends of predictors

###### code from Jenn ####
ggplot(FLpL, aes(x = Year, y = value)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 12) +
  geom_smooth(method = lm) +
  xlab("") + ylab("Precipitation (in)") +
  theme(axis.text.x = element_text(angle = 45))

## https://lh3.googleusercontent.com/-406sBFCWwaY/WdPKAI0itJI/AAAAAAAAIjU/75-0aV0-3-sp_oMXLwFp3zUs3OsXGn3MgCL0BGAYYCw/h1505/2017-10-03.png
######

######### ----------------------------> define global objects ####
library(tidyverse)
library(raster)

######### ----------------------------> create contemporary rasters averaged by season ####
contempDir_monthly="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project" ## monthly current rasters
contempDir_seaonal="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/contemporary" #;dir.create(contempDir_seaonal) ## folder to house current rasters averaged by season

winter=c("m12","m01","m02")
spring=c("m03","m04","m05")
summer=c("m06","m07","m08")
fall=c("m09","m10","m11")

seasons=c("DJF","MAM","JJA","SON")

  for(season in seasons){
    print(season)
    a=list()
    assign(season,a)
  }

vars=c("bt","bs","st","SS","sh")

allfiles=list.files(contempDir_monthly,recursive = T,full.names = T)

for(file in allfiles){
  print(file)
  a=strsplit(file,"/")[[1]][9]
  b=strsplit(a,"_")
  month=b[[1]][1]
  year=b[[1]][2]
  
  ########## 20-40
  if(month %in% winter){
    DJF=unlist(list(file,DJF))
  }
  if(month %in% spring){
    MAM=unlist(list(file,MAM))
  }
  if(month %in% summer){
    JJA=unlist(list(file,JJA))
  }
  if(month %in% fall){
    SON=unlist(list(file,SON))
  }
}

master=list()
  for(season in seasons){
    print(season)
    master=unlist(list(master,season))
  }

for(mas in master){
  print(mas)
  a=paste0(contempDir_seaonal,"/contemp_",mas);dir.create(a)
  bs=grep("bs",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(bs,paste0(contempDir_seaonal,"/contemp_",mas,"/bs.tif"),format="GTiff")
  bt=grep("bt",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(bt,paste0(contempDir_seaonal,"/contemp_",mas,"/bt.tif"),format="GTiff")
  st=grep("st",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(st,paste0(contempDir_seaonal,"/contemp_",mas,"/st.tif"),format="GTiff")
  SS=grep("SS",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(SS,paste0(contempDir_seaonal,"/contemp_",mas,"/SS.tif"),format="GTiff")
  sh=grep("sh",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(sh,paste0(contempDir_seaonal,"/contemp_",mas,"/sh.tif"),format="GTiff")
}
#########