###########adding cm2.6 deltas to climatological bias-corrected hycom
library(raster)

primary_dir=paste("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/")# swap folder names if necessary
raster_dir=paste(primary_dir,"project_future/",sep="");setwd(raster_dir)
delta_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/CM2.6/rasters_point08deg/"
hist_dir=paste(primary_dir,"project/",sep="")

###first, get list of missing rasters
blank=list()
fileslist=list.files()
for(f in fileslist){
  path=paste(getwd(),"/",f,sep="")
  #print(path)
  small_list=list.files(path,pattern="*.tif$",full.names = TRUE)
  for(tif in small_list){
    if((file.info(tif)$size<10000)==TRUE){
      blank=list(blank,tif)
    }
  }
}

final_list=unlist(blank) ## list of blank rasters with path

for(f in final_list){
  print(f)
  a=unlist(strsplit(f,"/"))
  b=paste(a[10],a[9],sep="_")
  c=gsub(".tif","",b)
  d=gsub("_20","_",c)
  month=unlist(strsplit(a[9],"_"))
  month=month[1]
  var=gsub(".tif","",a[10])
  hist_raster=raster(paste(hist_dir,month,"_2010","/",a[10],sep=""))
  print(paste(hist_dir,month,"_2010","/",a[10],sep=""))
  delta_raster=raster(paste(delta_dir,d,".tif",sep=""))
  print(paste(delta_dir,d,".tif",sep=""))
  math=sum(hist_raster,delta_raster)
  writeRaster(math,f,format="GTiff",bylayer=TRUE,overwrite=TRUE)
}


