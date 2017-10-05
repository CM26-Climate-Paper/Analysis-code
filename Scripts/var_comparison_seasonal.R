##### ----------------------------> new code October 4th, 2017
# Purpose. Following call with GFDL, decided to maintain monthly variability
# Need: prediction surfaces, Winter (DJF), Spring (MAM), Summer (JJA), Fall (SON) for three time periods: (+20-40,+40-60,+60-80)

##this script is so Goddamn ugly but #yoco

## --> 1. Average surfaces in "/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_future"
## --> 2. Project full and partial models over these surfaces

library(raster)
library(tidyverse)

## --> 1. Average surfaces in "/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_future"
############################## RUN THIS ONCE, CREATS NEW PROJECTION SURFACES
######### ----------------------------> define global objects ####
monthlyDir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_future"
#seasonalDir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal" #;dir.create(seasonalDir)
seasonalDir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/project" ;#dir.create(seasonalDir)
seasons=c("DJF","MAM","JJA","SON")
years=c("av20_40","av40_60","av60_80")
months=c("m01","m02","m03","m04","m05","m06","m07","m08","m09","m10","m11","m12")

##### ----------------------------> creating new directories ####
# for(year in years){
#   for(season in seasons){
#     dirname=paste0(year,"_",season)
#     print(dirname)
#     dir.create(paste0(seasonalDir,"/",dirname))
#   }
# }

##### ----------------------------> creating empty list objects ####
for(year in years){
  for(season in seasons){
    dirname=paste0(year,"_",season)
    print(dirname)
    a=list()
    assign(dirname,a)
  }
}

##### ----------------------------> creating time periods to cycle through ####
period_1=seq(2040,2060) #20-40=40-60 (i started projections at 20, not at 1)
period_2=seq(2061,2080) #40-60=61-80
period_3=seq(2081,2099) #60-80=81-00
period_3=unlist(list(period_3,2000)) ##funky  naming convention

winter=c("m12","m01","m02")
spring=c("m03","m04","m05")
summer=c("m06","m07","m08")
fall=c("m09","m10","m11")

vars=c("bt","bs","st","SS","sh")

##### ----------------------------> grabbing all files that need to be modified ####
# this script works by first assessing the appropriate season/time period for each layer, and then iteratively adding it to its list object
allfiles=list.files(monthlyDir,recursive = T,full.names = T)

for(file in allfiles){
  print(file)
  a=strsplit(file,"/")[[1]][9]
  b=strsplit(a,"_")
  month=b[[1]][1]
  year=b[[1]][2]
  
  ########## 20-40
  if(month %in% winter && year %in% period_1){
    av20_40_DJF=unlist(list(file,av20_40_DJF))
  }
  if(month %in% spring && year %in% period_1){
    av20_40_MAM=unlist(list(file,av20_40_MAM))
  }
  if(month %in% summer && year %in% period_1){
    av20_40_JJA=unlist(list(file,av20_40_JJA))
  }
  if(month %in% fall && year %in% period_1){
    av20_40_SON=unlist(list(file,av20_40_SON))
  }
  
  ########## 40-60
  if(month %in% winter && year %in% period_2){
    av40_60_DJF=unlist(list(file,av40_60_DJF))
  }
  if(month %in% spring && year %in% period_2){
    av40_60_MAM=unlist(list(file,av40_60_MAM))
  }
  if(month %in% summer && year %in% period_2){
    av40_60_JJA=unlist(list(file,av40_60_JJA))
  }
  if(month %in% fall && year %in% period_2){
    av40_60_SON=unlist(list(file,av40_60_SON))
  }
  
  ########## 60-80
  if(month %in% winter && year %in% period_3){
    av60_80_DJF=unlist(list(file,av60_80_DJF))
  }
  if(month %in% spring && year %in% period_3){
    av60_80_MAM=unlist(list(file,av60_80_MAM))
  }
  if(month %in% summer && year %in% period_3){
    av60_80_JJA=unlist(list(file,av60_80_JJA))
  }
  if(month %in% fall && year %in% period_3){
    av60_80_SON=unlist(list(file,av60_80_SON))
  }
}


##### ----------------------------> averaging all rasters within each list object ####
# this script works by iterating thru all list objects and creating an average raster for each variable, with is then written to the appropriate directory
master=list()
for(year in years){
  for(season in seasons){
    dirname=paste0(year,"_",season)
    print(dirname)
    master=unlist(list(master,dirname))
  }
}

for(mas in master){
  print(mas)
bs=grep("bs",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(bs,paste0(seasonalDir,"/",mas,"/bs.tif"),format="GTiff")
bt=grep("bt",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(bt,paste0(seasonalDir,"/",mas,"/bt.tif"),format="GTiff")
st=grep("st",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(st,paste0(seasonalDir,"/",mas,"/st.tif"),format="GTiff")
SS=grep("SS",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(SS,paste0(seasonalDir,"/",mas,"/SS.tif"),format="GTiff")
sh=grep("sh",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(sh,paste0(seasonalDir,"/",mas,"/sh.tif"),format="GTiff")
}

##### ----------------------------> copying over the static bathymetric variables ####
folders=list.files(seasonalDir,full.names = T)
depth=paste0(monthlyDir,"/m01_2000/Depth.tif")
rugosity=paste0(monthlyDir,"/m01_2000/Rugosity.tif")

copy=function(x){
  file.copy(depth,x)
  file.copy(rugosity,x)
}

lapply(folders,FUN=copy)

##############################  end run once #####

## --> 2. Project full and partial models over these surfaces
seasonalDir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/project"
years=list.files(seasonalDir)
spp_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/species"
species=list.files(spp_dir)%>%lapply(.,function(x)gsub(".csv","",x))%>%unlist
proj_full="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/species_projections_full" #;dir.create(proj_full)
proj_partial="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/species_projections_partial" #;dir.create(proj_partial)
model_full="/Volumes/SeaGate/ClimatePaperCM2.6/species_models"
model_partial="/Volumes/SeaGate/ClimatePaperCM2.6/species_models_partial"
proj_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/project"

##partial models first
for(sp in species){
  print(sp)
  modelRDS=readRDS(paste0(model_partial,"/",sp,".rds"))
  path=paste0(proj_partial,"/",sp);dir.create(path)
  for(year in years){
    print(year)
    layers_dir=paste0(proj_dir,"/",year)
    print(layers_dir)
    Depth<-raster(paste(layers_dir,"/Depth.tif",sep=""))
    Rugosity<-raster(paste(layers_dir,"/Rugosity.tif",sep=""))
    SS<-raster(paste(layers_dir,"/SS.tif",sep=""))
    bs<-raster(paste(layers_dir,"/bs.tif",sep=""))
    st<-raster(paste(layers_dir,"/st.tif",sep=""))
    bt<-raster(paste(layers_dir,"/bt.tif",sep=""))
    sh<-raster(paste(layers_dir,"/sh.tif",sep=""))
    rasterStack<-stack(Depth,Rugosity,SS,bs,st,bt,sh)
    names(rasterStack)<-c("Depth","Rugosity","SS","bs","st","bt","sh") ## make a raster stack for the missing date
    filename=paste0(path,"/",sp,"_",year,".tif")
    print(filename)
    raster::predict(rasterStack,modelRDS,filename=paste0(path,"/",sp,"_",year,".tif"),fun=predict,format="GTiff", type="response",na.rm=TRUE,overwrite=TRUE,progress='text')
  }
}

##full models second
for(sp in species){
  print(sp)
  modelRDS=readRDS(paste0(model_full,"/",sp,".rds"))
  path=paste0(proj_full,"/",sp);dir.create(path)
  for(year in years){
    print(year)
    layers_dir=paste0(proj_dir,"/",year)
    print(layers_dir)
    Depth<-raster(paste(layers_dir,"/Depth.tif",sep=""))
    Rugosity<-raster(paste(layers_dir,"/Rugosity.tif",sep=""))
    SS<-raster(paste(layers_dir,"/SS.tif",sep=""))
    bs<-raster(paste(layers_dir,"/bs.tif",sep=""))
    st<-raster(paste(layers_dir,"/st.tif",sep=""))
    bt<-raster(paste(layers_dir,"/bt.tif",sep=""))
    sh<-raster(paste(layers_dir,"/sh.tif",sep=""))
    rasterStack<-stack(Depth,Rugosity,SS,bs,st,bt,sh)
    names(rasterStack)<-c("Depth","Rugosity","SS","bs","st","bt","sh") ## make a raster stack for the missing date
    filename=paste0(path,"/",sp,"_",year,".tif")
    print(filename)
    raster::predict(rasterStack,modelRDS,filename=paste0(path,"/",sp,"_",year,".tif"),fun=predict,format="GTiff", type="response",na.rm=TRUE,overwrite=TRUE,progress='text')
  }
}

