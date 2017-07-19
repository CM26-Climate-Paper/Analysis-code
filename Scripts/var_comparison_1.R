## June 26th 2017, benthic vars + temp vs all vars
# this is to create figures for final paper. We are comparing temp only methods with multivariate methods

#1. build two models (temp and all_vars) for each species, save as rds
#2. create projection surfaces (20-40,40-60,60-80)
#3. find names for projection surfaces, based on average temperature delta
#4. add newly created deltas to historical rasters
#5. project for each species

library(utils)
library(mgcv)
library(pROC)
library(tools)
library(base)
library(plyr)
library(biomod2)
library(maps)
##########################

##########################1. build two models (temp and all_vars) for each species, save as rds

### full models (all_vars) are "/Volumes/SeaGate/ClimatePaperCM2.6/species_models"
### partial models (temp + bethic only) are "/Volumes/SeaGate/ClimatePaperCM2.6/species_models_partial"
part_dir="/Volumes/SeaGate/ClimatePaperCM2.6/species_models_partial"
### species variable accociated records are "/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/species"
spp_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/species"

#species=c("d193","d171","d24","bp15","d103","d106","d73","d74","e301",'bp502','d141',"d143","p135","bp131") ## subset of commercial species (June 26th subset)
species=c("d84","d631","p121","bp32","d654","d147","e401","d139") ## Jenn's additional species (July 3rd, 2017)

### read in species csv
for(sp in species){
  a=read.csv(paste0(spp_dir,"/",sp,".csv"))
  assign(sp,a)
}

### Fitting partial models and saving RDS files
for (spp in species){
  print(paste("Starting ",spp,sep=""))
  #Spp_name<-file_path_sans_ext(spp)
  dat<-get(spp) 
  dat=dat[complete.cases(dat),]
  fit<-gam(p_a ~s(Depth,bs="ts")+s(Rugosity,bs="ts")+s(st,bs="ts")+s(bt,bs="ts"),family=binomial("logit"),data=dat,method="GCV.Cp") #temp, depth, rug only
  print(paste("Saving model for ",spp,sep=""))
  saveRDS(fit, paste0(part_dir,"/",spp,".rds"))
  print(paste("Finished ",spp,sep=""))
}
##########################

##########################--------only run this once--------##########################

##########################2. create projection surfaces (20-40,40-60,60-80)
###raw deltas: "/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/CM2.6/rasters_point08deg"
delta_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/CM2.6/rasters_point08deg"
proj_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs"  # directory to create new 20 yr average rasters
months=c("m01","m02","m03","m04","m05","m06","m07","m08","m09","m10","m11","m12")
bt=list.files(delta_dir,pattern = "bt_*",full.names = T)
bs=list.files(delta_dir,pattern = "bs_*",full.names = T)
st=list.files(delta_dir,pattern = "st_*",full.names = T)
SS=list.files(delta_dir,pattern = "SS_*",full.names = T)
sh=list.files(delta_dir,pattern = "sh_*",full.names = T)
vars=c("bt","bs","st","SS","sh")

#projection years vs. my labeling years
av20_40=as.character(seq(41,60)) #20-40=40-60 (i started projections at 20, not at 1)
av40_60=as.character(seq(61,80)) #40-60=61-80
av60_80=unlist(list(seq(81,99),"00")) #60-80=81-00

##create dirs
av20_40=paste0(proj_dir,"/av20_40");dir.create(av20_40)
av40_60=paste0(proj_dir,"/av40_60");dir.create(av40_60)
av60_80=paste0(proj_dir,"/av60_80");dir.create(av60_80)

################ CM2.6.R adapted to batch all netcdfs
library(chron)
library(sp)
library(rgdal)
library(raster)
library(ncdf4)

template=raster("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/CM2.6/raster/fshnt_fin.tif") ### .1 deg raster made from fishneting netcdf points
study_area=raster("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/AVISO/extrct_ras")
setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/CM2.6")
netcdfs=list.files(pattern="*.nc$")#names of netcdffiles

#netcdfs
#[1] "bs.nc" "bt.nc" "SS.nc" "st.nc" "sh.nc"
nc="sh"   ###########clunky, change for each variable
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
print("converting to raster")
tmp.df02$av20_40=rowMeans(tmp.df02[,c(243:482)]) #20-40=40-60 (i started projections at 20, not at 1)
tmp.df02$av40_60=rowMeans(tmp.df02[,c(483:722)]) #40-60=61-80
tmp.df02$av60_80=rowMeans(tmp.df02[,c(723:962)]) #60-80=81-00
coordinates(tmp.df02)=~lon+lat
for(n in names(tmp.df02)[961:963]){
  r=rasterize(tmp.df02,template,field=n,fun=mean) # points to raster (.1 deg)
  x=resample(r,study_area,method="ngb") # origin and resolution to match study area
  e=mask(x,study_area) #clip to study area
  name=paste(proj_dir,"/",nc,"_",n,sep="")
  print(name)
  writeRaster(e,name,format="GTiff",bylayer=TRUE)
}

##i went through and removed their date-specific names
##and moved them to project_20y_avs/deltas
################a

################ 3. find names for projection surfaces, based on average temperature delta
av20_40=raster(paste0(proj_dir,"/av20_40/st.tif"))
av40_60=raster(paste0(proj_dir,"/av40_60/st.tif"))
av60_80=raster(paste0(proj_dir,"/av60_80/st.tif"))

z20_40=zonal(av20_40,study_area,fun='mean') ################ 0.9532168 (0.95)  # these are suface temperature deltas
z40_60=zonal(av40_60,study_area,fun='mean') ################ 2.413975 (2.41)
z60_80=zonal(av60_80,study_area,fun='mean') ################ 3.048009 (3.05)
################a

################ 4. add newly created deltas to historical rasters
### a. average contemporary rasters across months to make one for each variable
contemp_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project"
contemp20_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/contemporary"
### b. add each delta
delta_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/deltas"
### c. create new folder for projection rasters
proj_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/project"
### d. create 20yr folders w.in proj_dir
##create dirs
av20_40=paste0(proj_dir,"/av20_40");dir.create(av20_40)
av40_60=paste0(proj_dir,"/av40_60");dir.create(av40_60)
av60_80=paste0(proj_dir,"/av60_80");dir.create(av60_80)

### a. average contemporary rasters across months to make one for each variable
a=list.files(contemp_dir,full.names = T,recursive = T)
bs=stack(grep("bs.tif",a,value = T))
bt=stack(grep("bt.tif",a,value = T))
SS=stack(grep("SS.tif",a,value = T))
st=stack(grep("st.tif",a,value = T))
sh=stack(grep("sh.tif",a,value = T))

bs=calc(bs,fun=mean)
bt=calc(bt,fun=mean)
SS=calc(SS,fun=mean)
st=calc(st,fun=mean)
sh=calc(sh,fun=mean)

writeRaster(bs,paste0(contemp20_dir,"/bs.tif"),format="GTiff")
writeRaster(bt,paste0(contemp20_dir,"/bt.tif"),format="GTiff")
writeRaster(SS,paste0(contemp20_dir,"/SS.tif"),format="GTiff")
writeRaster(st,paste0(contemp20_dir,"/st.tif"),format="GTiff")
writeRaster(sh,paste0(contemp20_dir,"/sh.tif"),format="GTiff")

### b. add each delta
years=c("av20_40","av40_60","av60_80")
proj_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/project"
contemp20_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/contemporary"
delta_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/deltas"
variable=c("bs","bt","SS","st","sh")

for(year in years){
  for(v in variable){
    delta=raster(paste0(delta_dir,"/",year,"/",v,".tif"))
    delta2=paste0(delta_dir,"/",year,"/",v,".tif")
    print(delta2)
    contemp=raster(paste0(contemp20_dir,"/",v,".tif"))
    a=sum(delta,contemp)
    path=paste0(proj_dir,"/",year,"/",v,".tif")
    print(path)
    writeRaster(a,path,format="GTiff")
    
  }
}
################a 

##########################--------end only run this once--------##########################

################5. project for each species
#species=c("d193","d171","d24","bp15","d103","d106","d73","d74","e301",'bp502','d141',"d143","p135","bp131") ## subset of commercial species
species=c("d84","d631","p121","bp32","d654","d147","e401","d139") ## Jenn's additional species (July 3rd, 2017)
proj_full="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/species_projections_full"
proj_partial="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/species_projections_partial"
model_full="/Volumes/SeaGate/ClimatePaperCM2.6/species_models"
model_partial="/Volumes/SeaGate/ClimatePaperCM2.6/species_models_partial"
years=c("av20_40","av40_60","av60_80")
proj_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/project"

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
  if(year=="av20_40"){
    year2="Averaged y20-y40 (+0.95C SST)"
    png(paste0("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/pngs/",species,"_",year,"_",model_type,".png"), width=7, height=5, units="in", res=400)
  }
  
  if(year=="av40_60"){
    year2="Averaged y40-y60 (+2.41C SST)"
    png(paste0("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/pngs/",species,"_",year,"_",model_type,".png"), width=7, height=5, units="in", res=400)
  }
  
  if(year=="av60_80"){
    year2="Averaged y60-y80 (+3.05C SST)"
    png(paste0("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/pngs/",species,"_",year,"_",model_type,".png"), width=7, height=5, units="in", res=400)
  }
  
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
  
  if(species=="d193"){
    common_name="ocean pout"
  }
  
  if(species=="d171"){
    common_name="northern sea robin"
  }
  
  if(species=="d24"){
    common_name="clearnose skate"
  }
  
  if(species=="bp15"){
    common_name="spiny dogfish"
  }
  
  if(species=="d103"){
    common_name="summer flounder"
  }
  
  if(species=="d106"){
    common_name="winter flounder"
  }
  
  if(species=="d73"){
    common_name="atlantic cod"
  }
  
  if(species=="d74"){
    common_name="haddock"
  }
  
  if(species=="e301"){
    common_name="american lobster"
  }
  
  if(species=="bp502"){
    common_name="longfin squid"
  }
  
  if(species=="d141"){
    common_name="black sea bass"
  }
  
  if(species=="d143"){
    common_name="scup"
  }
  
  if(species=="p135"){
    common_name="bluefish"
  }
  
  if(species=="bp131"){
    common_name="butterfish"
  }
  
  if(species=="d84"){
    common_name="cusk"
  }
  if(species=="d631"){
    common_name="sheepshead"
  }
  if(species=="p121"){
    common_name="Atlantic mackerel"
  }
  if(species=="bp32"){
    common_name="Atlantic herring"
  }
  if(species=="d654"){
    common_name="red drum"
  }
  if(species=="d147"){
    common_name="black drum"
  }
  if(species=="e401"){
    common_name="sea scallop"
  }
  if(species=="d139"){
    common_name="striped bass"
  }
  
  mtext(paste0("Habitat suitability for ",common_name,". ",year2,". Model type = ",model_type), side=1, line=2.5)
  
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
