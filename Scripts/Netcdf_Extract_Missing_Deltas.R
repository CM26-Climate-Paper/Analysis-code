################ CM2.6.R adapted to batch all netcdfs
library(chron)
library(sp)
library(rgdal)
library(raster)
library(ncdf4)

template=raster("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/CM2.6/raster/fshnt_fin.tif") ### .08 deg raster made from fishneting netcdf points
study_area=raster("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/AVISO/extrct_ras")
setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/CM2.6")
netcdfs=list.files(pattern="*.nc$")#names of netcdffiles
delta_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/CM2.6/rasters_point08deg"

##### 1. get list of missing deltas
setwd(delta_dir)
blank=list()
fileslist=list.files(pattern="*.tif$")
for(f in fileslist){
  path=paste(getwd(),"/",f,sep="")
  if((file.info(path)$size<2000)==TRUE){
    blank=list(blank,f)
    }
  }


final_list=unlist(blank) ## list of blank rasters with path

bt_list=list()
for(f in final_list){
  if(grepl("bt",f)){
    bt_list=unlist(list(bt_list,f))
  }
}
sh_list=list()
for(f in final_list){
  if(grepl("sh",f)){
    sh_list=unlist(list(sh_list,f))
  }
}
SS_list=list()
for(f in final_list){
  if(grepl("SS",f)){
    SS_list=unlist(list(SS_list,f))
  }
}
st_list=list()
for(f in final_list){
  if(grepl("st",f)){
    st_list=unlist(list(st_list,f))
  }
}
# bs_list=list()  ### NO missing bs rasters
# for(f in final_list){
#   if(grepl("bs",f)){
#     bs_list=unlist(list(bs_list,f))
#   }
# }


netcdfs
#[1] "bs.nc" "bt.nc" "SS.nc" "st.nc" "sh.nc"
nc="st"   ###########clunky, change for each variable
#for(nc in netcdfs){
  print(nc)
  ncc=paste(nc,".nc",sep="")
  ncin <- nc_open(ncc)
  print(ncin)
  dname="DELTA" # define variable of interest ########### change for each variable
  print("defining variables")
  lon <- ncvar_get(ncin, "XT_OCEAN2001_2250") # define longitude
  nlon <- dim(lon)
  lat <- ncvar_get(ncin, "YT_OCEAN1560_1809", verbose = F) # define latitude
  nlat <- dim(lat)
  t <- ncvar_get(ncin, "TIME") # define time field
  tunits <- ncatt_get(ncin, "TIME", "units") # get time units
  nt <- dim(t)
  tmp.array <- ncvar_get(ncin, dname)
  dlname <- ncatt_get(ncin, dname, "long_name") #grab global attributes
  dunits <- ncatt_get(ncin, dname, "units") #grab global attributes
  fillvalue <- ncatt_get(ncin, dname, "_FillValue") #grab global attributes
  print("changing date format")
  tustr <- strsplit(tunits$value, " ") #changing date format
  tdstr <- strsplit(unlist(tustr)[3], "-") #changing date format
  tmonth = as.integer(unlist(tdstr)[2]) #changing date format
  tday = as.integer(unlist(tdstr)[3]) #changing date format
  tyear = as.integer(unlist(tdstr)[1]) #changing date format
  date=chron(t, origin = c(tmonth, tday, tyear)) #changing date format
  tmp.array[tmp.array==fillvalue$value]=NA #setting fill value
  tmp.vec.long <- as.vector(tmp.array)
  length(tmp.vec.long)
  tmp.mat <- matrix(tmp.vec.long, nrow = nlon * nlat, ncol = nt)
  print("Formatting column names")
  date2=as.character(chron(t, origin = c(tmonth, tday, tyear))) ####getting names together
  d=lapply(date2,function(x)strsplit(x, " "))
  e=lapply(d,function(x)strsplit(unlist(x)[1], "/"))
  f=lapply(e,function(x)unlist(x)[c(1,3)])
  g=lapply(f,function(x)paste(x[1],x[2],sep="_"))
  h=lapply(g,function(x)gsub("\\(","m",x))
  print("Creating spatial dataframe")
  lonlat <- expand.grid(lon, lat)
  names(lonlat) <- c("lon","lat")
  tmp.df02 <- data.frame(tmp.mat)
  names(tmp.df02) <- h
  tmp.df02 <- cbind(lonlat, tmp.df02)
  coordinates(tmp.df02)=~lon+lat
  print("converting to raster")
  for(n in st_list){   ############################ change for each!!!!!!!!!!!!!!!!!!!!!!!!
    name=gsub("st_","",n) ########################### change for each!!!!!!!!!!!!!!!!!!!!!!!!
    name=gsub(".tif","",name)
    r=rasterize(tmp.df02,template,field=name,fun=mean) # points to raster (.1 deg)
    x=resample(r,study_area,method="ngb") # origin and resolution to match study area
    e=mask(x,study_area) #clip to study area
    save=paste(delta_dir,"/",nc,"_",name,sep="")
    print(save)
    writeRaster(e,save,format="GTiff",bylayer=TRUE,overwrite=TRUE)
  }

