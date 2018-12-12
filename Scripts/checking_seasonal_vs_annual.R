############## --------------> checking why seasonal predictions are so different from annual
library(lattice)

#### reading in rasters
setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/contemporary")
for(file in list.files(recursive = T)){
  a=raster(file)
  plot(a,main=paste0(file))
  assign(file,a)
}

setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/project")
for(file in list.files(recursive = T)){
  a=raster(file)
  plot(a,main=paste0(file))
  assign(file,a)
}

setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/contemporary")
for(file in list.files(recursive = T)){
  a=raster(file)
  plot(a,main=paste0("annual_",file))
  assign(file,a)
}

setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/project")
for(file in list.files(recursive = T)){
  a=raster(file)
  plot(a,main=paste0("annual_",file))
  assign(file,a)
}

### check 1 day lags
################ writing out plots

library(raster)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(grid)
library(rasterVis)
library(maps)
library(mapdata)
library(maptools)

############# ----------------------> Contemporary, SST

###############################################################################################
########
dates=seq(as.Date("2012-08-01"),as.Date("2012-08-15"),by = "day")
studyarea=raster("/Volumes/SeaGate/EcoCast_HW/EcoCastGit_private/EcoCast-private/Code/RShinyApp/data/sa_small.tif")

bJFMA1=matrix(c(-140,25,  ## define SST box
                -140,50,
                -110,50,
                -110,25,
                -140,25),
              ncol=2,byrow = T)
p=Polygon(bJFMA1)
ps=Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps)=CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
plot(sps,add=T)

e=extent(sps)
rm(p,ps,sps)


### read in chla
lss=list()
for(i in 1:length(dates)){
  print(i)
  date=as.character(dates[i])
  print(date)
  ev_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive/SpatialPredictions_EnvData/Satellite/"
  date_dir=paste0(ev_dir,date,"/l.blendChl.grd")
  #lss=list(lss,date_dir)
  a=raster(date_dir)*studyarea
  b=crop(a,e)
  name=paste0("chl_",date)
  assign(name,b)
}
### read in lbst lagged
#lss=list()
for(i in 1:length(dates)){
  print(i)
  date=as.character(dates[i])
  print(date)
  Lagged_dir_lbst="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/Lagged"
  date_dir=paste0(paste0(Lagged_dir_lbst,"/lbst_pa_",dates[i],"_CHLA_1_mean.grd"))
  #lss=list(lss,date_dir)
  a=raster(date_dir)*studyarea
  b=crop(a,e)
  name=paste0("lbst_CHLA_1dlag",date)
  assign(name,b)
}

### read in lbst OO
for(i in 1:length(dates)){
  print(i)
  date=as.character(dates[i])
  print(date)
  OO_dir_lbst="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/lbst/predCIs/OO"
  date_dir=paste0(paste0(OO_dir_lbst,"/lbst_pa_",dates[i],"__mean.grd"))
  #lss=list(lss,date_dir)
  a=raster(date_dir)*studyarea
  b=crop(a,e)
  name=paste0("lbst_OO_",date)
  assign(name,b)
}

### read in ecocast lagged
#lss=list()
for(i in 1:length(dates)){
  print(i)
  date=as.character(dates[i])
  print(date)
  Lagged_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/Lagged"
  date_dir=paste0(paste0(Lagged_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[i],"_CHLA_1.grd"))
  #lss=list(lss,date_dir)
  a=raster(date_dir)*studyarea
  b=crop(a,e)
  name=paste0("EcoCast_Lagged_CHL_1_",date)
  assign(name,b)
}

### read in ecocast OO
#lss=list()
for(i in 1:length(dates)){
  print(i)
  date=as.character(dates[i])
  print(date)
  Ecocast_dir="/Volumes/SeaGate/ERD_DOM/EcoCast_CodeArchive_Sensitivity/EcoCastRuns/output/mean/OO"
  date_dir=paste0(paste0(Ecocast_dir,"/EcoCast_-0.2 -0.2 -0.05 -0.9 0.9_",dates[i],"_.grd"))
  #lss=list(lss,date_dir)
  a=raster(date_dir)*studyarea
  b=crop(a,e)
  name=paste0("EcoCast_OO_",date)
  assign(name,b)
}
########
ext <- as.vector(extent(`st.tif`))
boundaries <- map('worldHires', fill=TRUE,
                  xlim=ext[1:2], ylim=ext[3:4],
                  plot=FALSE)
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS(projection(`st.tif`)))

myTheme_temp=rasterTheme(region=rev(brewer.pal('RdBu', n=11)))
myTheme_salinity=rasterTheme(region=brewer.pal('PiYG', n=11))
myTheme_sh=rasterTheme(region=brewer.pal('PRGn', n=11))

my_at=seq(`st.tif`@data@min,`av60_80/st.tif`@data@max,by=1)
my_at_bt=seq(`bt.tif`@data@min,`av60_80/bt.tif`@data@max,by=1)
my_at_SS=seq(28,`av60_80/SS.tif`@data@max,by=1)
my_at_bs=seq(28,`av60_80/bs.tif`@data@max,by=1)
my_at_ssh=seq(`sh.tif`@data@min,`av60_80/sh.tif`@data@max,by=.02)

######### all variables, 20 year averages ####
png("/Volumes/SeaGate/ClimatePaperCM2.6/20 year averages.png",
    width     = 12,
    height    = 15,
    units     = "in",
    res       = 1200)
a=levelplot(`st.tif`,main="SST, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at)+ layer(sp.polygons(bPols))
b=levelplot(`av20_40/st.tif`,main="SST, Y20-40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at)+ layer(sp.polygons(bPols))
bb=levelplot(`av40_60/st.tif`,main="SST, Y40-60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at)+ layer(sp.polygons(bPols))
c=levelplot(`av60_80/st.tif`,main="SST, Y60-80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at)+ layer(sp.polygons(bPols))

d=levelplot(`bt.tif`,main="BT, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_bt)+ layer(sp.polygons(bPols))
e=levelplot(`av20_40/bt.tif`,main="BT, Y20-40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_bt)+ layer(sp.polygons(bPols))
f=levelplot(`av40_60/bt.tif`,main="BT, Y40-60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_bt)+ layer(sp.polygons(bPols))
g=levelplot(`av60_80/bt.tif`,main="BT, Y60-80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_bt)+ layer(sp.polygons(bPols))

`SS.tif`[`SS.tif` < 28] <- NA
`av20_40/SS.tif`[`av20_40/SS.tif` < 28] <- NA
`av40_60/SS.tif`[`av40_60/SS.tif` < 28] <- NA
`av60_80/SS.tif`[`av60_80/SS.tif` < 28] <- NA

h=levelplot(`SS.tif`,main="SS, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_SS)+ layer(sp.polygons(bPols))
i=levelplot(`av20_40/SS.tif`,main="SS, Y20-40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_SS)+ layer(sp.polygons(bPols))
j=levelplot(`av40_60/SS.tif`,main="SS, Y40-60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_SS)+ layer(sp.polygons(bPols))
k=levelplot(`av60_80/SS.tif`,main="SS, Y60-80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_SS)+ layer(sp.polygons(bPols))

`bs.tif`[`bs.tif` < 28] <- NA
`av20_40/bs.tif`[`av20_40/bs.tif` < 28] <- NA
`av40_60/bs.tif`[`av40_60/bs.tif` < 28] <- NA
`av60_80/bs.tif`[`av60_80/bs.tif` < 28] <- NA

l=levelplot(`bs.tif`,main="BS, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_bs)+ layer(sp.polygons(bPols))
m=levelplot(`av20_40/bs.tif`,main="BS, Y20-40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_bs)+ layer(sp.polygons(bPols))
n=levelplot(`av40_60/bs.tif`,main="BS, Y40-60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_bs)+ layer(sp.polygons(bPols))
o=levelplot(`av60_80/bs.tif`,main="BS, Y60-80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_bs)+ layer(sp.polygons(bPols))

p=levelplot(`sh.tif`,main="SSH, contemporary",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_ssh)+ layer(sp.polygons(bPols))
q=levelplot(`av20_40/sh.tif`,main="SSH, Y20-40",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_ssh)+ layer(sp.polygons(bPols))
r=levelplot(`av40_60/sh.tif`,main="SSH, Y40-60",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_ssh)+ layer(sp.polygons(bPols))
s=levelplot(`av60_80/sh.tif`,main="SSH, Y60-80",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T,at=my_at_ssh)+ layer(sp.polygons(bPols))


grid.arrange(a,b,bb,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,ncol=4,widths=c(4,4,4,4),heights=c(5,5,5,5,5))
dev.off()


######### SST ####
png("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/checking_errors/st.png",
    width     = 12,
    height    = 15,
    units     = "in",
    res       = 1200)
a=levelplot(`st.tif`,main="st, annual, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
b=levelplot(`av20_40/st.tif`,main="st, annual, 20_40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
bb=levelplot(`av40_60/st.tif`,main="st, annual, 40_60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
c=levelplot(`av60_80/st.tif`,main="st, annual, 60_80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

d=levelplot(`contemp_DJF/st.tif`,main="st, DJF, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
e=levelplot(`av20_40_DJF/st.tif`,main="st, DJF, 20_40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
f=levelplot(`av40_60_DJF/st.tif`,main="st, DJF, 40_60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
g=levelplot(`av60_80_DJF/st.tif`,main="st, DJF, 60_80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

h=levelplot(`contemp_MAM/st.tif`,main="st, MAM, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
i=levelplot(`av20_40_MAM/st.tif`,main="st, MAM, 20_40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
j=levelplot(`av40_60_MAM/st.tif`,main="st, MAM, 40_60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
k=levelplot(`av60_80_MAM/st.tif`,main="st, MAM, 60_80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

l=levelplot(`contemp_JJA/st.tif`,main="st, JJA, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
m=levelplot(`av20_40_JJA/st.tif`,main="st, JJA, 20_40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
n=levelplot(`av40_60_JJA/st.tif`,main="st, JJA, 40_60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
o=levelplot(`av60_80_JJA/st.tif`,main="st, JJA, 60_80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

p=levelplot(`contemp_SON/st.tif`,main="st, SON, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
q=levelplot(`av20_40_SON/st.tif`,main="st, SON, 20_40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
r=levelplot(`av40_60_SON/st.tif`,main="st, SON, 40_60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
s=levelplot(`av60_80_SON/st.tif`,main="st, SON, 60_80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

grid.arrange(a,b,bb,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,ncol=4,widths=c(4,4,4,4),heights=c(5,5,5,5,5))
dev.off()

######### bt ####
png("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/checking_errors/bt.png",
    width     = 12,
    height    = 15,
    units     = "in",
    res       = 1200)
a=levelplot(`bt.tif`,main="bt, annual, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
b=levelplot(`av20_40/bt.tif`,main="bt, annual, 20_40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
bb=levelplot(`av40_60/bt.tif`,main="bt, annual, 40_60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
c=levelplot(`av60_80/bt.tif`,main="bt, annual, 60_80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

d=levelplot(`contemp_DJF/bt.tif`,main="bt, DJF, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
e=levelplot(`av20_40_DJF/bt.tif`,main="bt, DJF, 20_40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
f=levelplot(`av40_60_DJF/bt.tif`,main="bt, DJF, 40_60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
g=levelplot(`av60_80_DJF/bt.tif`,main="bt, DJF, 60_80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

h=levelplot(`contemp_MAM/bt.tif`,main="bt, MAM, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
i=levelplot(`av20_40_MAM/bt.tif`,main="bt, MAM, 20_40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
j=levelplot(`av40_60_MAM/bt.tif`,main="bt, MAM, 40_60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
k=levelplot(`av60_80_MAM/bt.tif`,main="bt, MAM, 60_80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

l=levelplot(`contemp_JJA/bt.tif`,main="bt, JJA, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
m=levelplot(`av20_40_JJA/bt.tif`,main="bt, JJA, 20_40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
n=levelplot(`av40_60_JJA/bt.tif`,main="bt, JJA, 40_60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
o=levelplot(`av60_80_JJA/bt.tif`,main="bt, JJA, 60_80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

p=levelplot(`contemp_SON/bt.tif`,main="bt, SON, contemporary",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
q=levelplot(`av20_40_SON/bt.tif`,main="bt, SON, 20_40",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
r=levelplot(`av40_60_SON/bt.tif`,main="bt, SON, 40_60",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
s=levelplot(`av60_80_SON/bt.tif`,main="bt, SON, 60_80",par.settings=myTheme_temp,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

grid.arrange(a,b,bb,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,ncol=4,widths=c(4,4,4,4),heights=c(5,5,5,5,5))
dev.off()



######### SS ####
png("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/checking_errors/SS.png",
    width     = 12,
    height    = 15,
    units     = "in",
    res       = 1200)
a=levelplot(`SS.tif`,main="SS, annual, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
b=levelplot(`av20_40/SS.tif`,main="SS, annual, 20_40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
bb=levelplot(`av40_60/SS.tif`,main="SS, annual, 40_60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
c=levelplot(`av60_80/SS.tif`,main="SS, annual, 60_80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

d=levelplot(`contemp_DJF/SS.tif`,main="SS, DJF, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
e=levelplot(`av20_40_DJF/SS.tif`,main="SS, DJF, 20_40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
f=levelplot(`av40_60_DJF/SS.tif`,main="SS, DJF, 40_60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
g=levelplot(`av60_80_DJF/SS.tif`,main="SS, DJF, 60_80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

h=levelplot(`contemp_MAM/SS.tif`,main="SS, MAM, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
i=levelplot(`av20_40_MAM/SS.tif`,main="SS, MAM, 20_40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
j=levelplot(`av40_60_MAM/SS.tif`,main="SS, MAM, 40_60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
k=levelplot(`av60_80_MAM/SS.tif`,main="SS, MAM, 60_80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

l=levelplot(`contemp_JJA/SS.tif`,main="SS, JJA, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
m=levelplot(`av20_40_JJA/SS.tif`,main="SS, JJA, 20_40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
n=levelplot(`av40_60_JJA/SS.tif`,main="SS, JJA, 40_60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
o=levelplot(`av60_80_JJA/SS.tif`,main="SS, JJA, 60_80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

p=levelplot(`contemp_SON/SS.tif`,main="SS, SON, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
q=levelplot(`av20_40_SON/SS.tif`,main="SS, SON, 20_40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
r=levelplot(`av40_60_SON/SS.tif`,main="SS, SON, 40_60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
s=levelplot(`av60_80_SON/SS.tif`,main="SS, SON, 60_80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

grid.arrange(a,b,bb,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,ncol=4,widths=c(4,4,4,4),heights=c(5,5,5,5,5))
dev.off()
######### bs ####
png("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/checking_errors/bs.png",
    width     = 12,
    height    = 15,
    units     = "in",
    res       = 1200)
a=levelplot(`bs.tif`,main="bs, annual, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
b=levelplot(`av20_40/bs.tif`,main="bs, annual, 20_40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
bb=levelplot(`av40_60/bs.tif`,main="bs, annual, 40_60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
c=levelplot(`av60_80/bs.tif`,main="bs, annual, 60_80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

d=levelplot(`contemp_DJF/bs.tif`,main="bs, DJF, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
e=levelplot(`av20_40_DJF/bs.tif`,main="bs, DJF, 20_40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
f=levelplot(`av40_60_DJF/bs.tif`,main="bs, DJF, 40_60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
g=levelplot(`av60_80_DJF/bs.tif`,main="bs, DJF, 60_80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

h=levelplot(`contemp_MAM/bs.tif`,main="bs, MAM, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
i=levelplot(`av20_40_MAM/bs.tif`,main="bs, MAM, 20_40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
j=levelplot(`av40_60_MAM/bs.tif`,main="bs, MAM, 40_60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
k=levelplot(`av60_80_MAM/bs.tif`,main="bs, MAM, 60_80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

l=levelplot(`contemp_JJA/bs.tif`,main="bs, JJA, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
m=levelplot(`av20_40_JJA/bs.tif`,main="bs, JJA, 20_40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
n=levelplot(`av40_60_JJA/bs.tif`,main="bs, JJA, 40_60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
o=levelplot(`av60_80_JJA/bs.tif`,main="bs, JJA, 60_80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

p=levelplot(`contemp_SON/bs.tif`,main="bs, SON, contemporary",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
q=levelplot(`av20_40_SON/bs.tif`,main="bs, SON, 20_40",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
r=levelplot(`av40_60_SON/bs.tif`,main="bs, SON, 40_60",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
s=levelplot(`av60_80_SON/bs.tif`,main="bs, SON, 60_80",par.settings=myTheme_salinity,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

grid.arrange(a,b,bb,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,ncol=4,widths=c(4,4,4,4),heights=c(5,5,5,5,5))
dev.off()

######### sh ####
png("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/checking_errors/sh.png",
    width     = 12,
    height    = 15,
    units     = "in",
    res       = 1200)
a=levelplot(`sh.tif`,main="sh, annual, contemporary",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
b=levelplot(`av20_40/sh.tif`,main="sh, annual, 20_40",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
bb=levelplot(`av40_60/sh.tif`,main="sh, annual, 40_60",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
c=levelplot(`av60_80/sh.tif`,main="sh, annual, 60_80",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

d=levelplot(`contemp_DJF/sh.tif`,main="sh, DJF, contemporary",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
e=levelplot(`av20_40_DJF/sh.tif`,main="sh, DJF, 20_40",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
f=levelplot(`av40_60_DJF/sh.tif`,main="sh, DJF, 40_60",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
g=levelplot(`av60_80_DJF/sh.tif`,main="sh, DJF, 60_80",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

h=levelplot(`contemp_MAM/sh.tif`,main="sh, MAM, contemporary",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
i=levelplot(`av20_40_MAM/sh.tif`,main="sh, MAM, 20_40",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
j=levelplot(`av40_60_MAM/sh.tif`,main="sh, MAM, 40_60",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
k=levelplot(`av60_80_MAM/sh.tif`,main="sh, MAM, 60_80",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

l=levelplot(`contemp_JJA/sh.tif`,main="sh, JJA, contemporary",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
m=levelplot(`av20_40_JJA/sh.tif`,main="sh, JJA, 20_40",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
n=levelplot(`av40_60_JJA/sh.tif`,main="sh, JJA, 40_60",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
o=levelplot(`av60_80_JJA/sh.tif`,main="sh, JJA, 60_80",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

p=levelplot(`contemp_SON/sh.tif`,main="sh, SON, contemporary",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
q=levelplot(`av20_40_SON/sh.tif`,main="sh, SON, 20_40",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
r=levelplot(`av40_60_SON/sh.tif`,main="sh, SON, 40_60",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)
s=levelplot(`av60_80_SON/sh.tif`,main="sh, SON, 60_80",par.settings=myTheme_sh,between = list(x=0.1, y=0.1),margin=F,colorkey=T)

grid.arrange(a,b,bb,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,ncol=4,widths=c(4,4,4,4),heights=c(5,5,5,5,5))
dev.off()