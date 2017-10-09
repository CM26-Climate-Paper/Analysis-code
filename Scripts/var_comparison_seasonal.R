##### ----------------------------> new code October 4th, 2017
# Purpose. Following call with GFDL, decided to maintain monthly variability
# Need: prediction surfaces, Winter (DJF), Spring (MAM), Summer (JJA), Fall (SON) for three time periods: (+20-40,+40-60,+60-80)

##this script is so Goddamn ugly but #yoco

## --> 1. Average surfaces in "/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_future"
## --> 2. Project full and partial models over these surfaces

library(raster)
library(tidyverse)
library(maps)

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
sh=grep("sh.tif",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(sh,paste0(seasonalDir,"/",mas,"/sh.tif"),format="GTiff",overwrite=T)
}

sh=grep("sh.tif",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(sh,paste0(seasonalDir,"/",mas,"/sh.tif"),format="GTiff")
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

make_png=function(r,year,species,model_type){ ### does what it says
  
    png(paste0("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/pngs/",species,"_",year,"_",model_type,".png"), width=7, height=5, units="in", res=400)
  
  par(ps=10) #settings before layout
  layout(matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE), heights=c(4,1), widths=7)
  #layout.show(2) # run to see layout; comment out to prevent plotting during .pdf
  par(cex=1) # layout has the tendency change par()$cex, so this step is important for control
  
  par(mar=c(4,4,1,1)) # I usually set my margins before each plot
  #pal <- colorRampPalette(c("blue", "grey", "red"))
  pal <- colorRampPalette(c("darkblue","blue", "cyan", "yellow", "red","dark red"))
  #pal <- colorRampPalette(c("purple4", "white", "blue"))
  ncolors <- 200
  breaks <- seq(0,1,,ncolors+1)
  image(r, col=pal(ncolors), breaks=breaks)
  map("world", add=TRUE, lwd=2)
  #contour(r, add=TRUE, col="black",levels=c(-.75,-.5,.5,.75))
  box()
  
  par(mar=c(4,4,0,1)) # I usually set my margins before each plot
  levs <- breaks[-1] - diff(breaks)/2
  image(x=levs, y=1, z=as.matrix(levs), col=pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
  
  # if(species=="d193"){
  #   common_name="ocean pout"
  # }
  # 
  # if(species=="d171"){
  #   common_name="northern sea robin"
  # }
  # 
  # if(species=="d24"){
  #   common_name="clearnose skate"
  # }
  # 
  # if(species=="bp15"){
  #   common_name="spiny dogfish"
  # }
  # 
  # if(species=="d103"){
  #   common_name="summer flounder"
  # }
  # 
  # if(species=="d106"){
  #   common_name="winter flounder"
  # }
  # 
  # if(species=="d73"){
  #   common_name="atlantic cod"
  # }
  # 
  # if(species=="d74"){
  #   common_name="haddock"
  # }
  # 
  # if(species=="e301"){
  #   common_name="american lobster"
  # }
  # 
  # if(species=="bp502"){
  #   common_name="longfin squid"
  # }
  # 
  # if(species=="d141"){
  #   common_name="black sea bass"
  # }
  # 
  # if(species=="d143"){
  #   common_name="scup"
  # }
  # 
  # if(species=="p135"){
  #   common_name="bluefish"
  # }
  # 
  # if(species=="bp131"){
  #   common_name="butterfish"
  # }
  # 
  # if(species=="d84"){
  #   common_name="cusk"
  # }
  # if(species=="d631"){
  #   common_name="sheepshead"
  # }
  # if(species=="p121"){
  #   common_name="Atlantic mackerel"
  # }
  # if(species=="bp32"){
  #   common_name="Atlantic herring"
  # }
  # if(species=="d654"){
  #   common_name="red drum"
  # }
  # if(species=="d147"){
  #   common_name="black drum"
  # }
  # if(species=="e401"){
  #   common_name="sea scallop"
  # }
  # if(species=="d139"){
  #   common_name="striped bass"
  # }
  
  common_name=species ### will need to remove this for paper to use real names
  
  mtext(paste0("Habitat suitability for ",common_name,". ",year,". Model type = ",model_type), side=1, line=2.5)
  
  box()
  
  dev.off() # closes device
}

for(sp in species){
  path=paste0(proj_full,"/",sp)
  for(year in years){
    ras=paste0(path,"/",sp,"_",year,".tif")
    r=raster(ras)
    make_png(r=r,year=year,species=sp,model_type = "full")
  }
}

for(sp in species){
  path=paste0(proj_partial,"/",sp)
  for(year in years){
    ras=paste0(path,"/",sp,"_",year,".tif")
    r=raster(ras)
    make_png(r=r,year=year,species=sp,model_type = "partial")
  }
}
