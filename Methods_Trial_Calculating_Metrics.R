#Methods_Trial_Landscape_Metrics 

###########################################################################################################
# 1. Removes everything in the working environment

rm(list=ls()) 

###########################################################################################################
# 2. Set the primary working directories

primary_dir=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/Full_Run_PA_GAM_climatologicalpoints/ALL_Species_Projections/Species_Projections_stacked/");setwd(primary_dir)# swap folder names if necessary
shared_git=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/CM26_Climate_Paper_Shared/CM26_Climate_Paper_Shared/")
test_dir=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/Full_Run_PA_GAM_climatologicalpoints/ALL_Species_Projections/Species_Projections_stacked/d73/")
test_prob_dir=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/Full_Run_PA_GAM_climatologicalpoints/ALL_Species_Projections/Species_Projections_all/bp1006/")
All_prob_dir=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/Full_Run_PA_GAM_climatologicalpoints/ALL_Species_Projections/Species_Projections_all/")
PA_raster_dir=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/Full_Run_PA_GAM_climatologicalpoints/ALL_Species_Projections/Species_Projections_stacked/")

output_dir=paste();dir.create(output_dir)


###########################################################################################################
# 3. Installing and loading all relevant libraries

# #install packages
# install.packages("dplyr")
# install.packages("doBy")
# install.packages("dismo")
# install.packages("mgcv")
# install.packages("pROC")
# install.packages("caTools")
# install.packages("DiceEval")
# install.packages("biomod2")
# install.packages("ncdf4")
install.packages("spatialEco")
install.packages("rgdal")
install.packages("SDMTools")
install.packages("sp")


#libraries needed
library(dplyr)
library(doBy)
library(dismo)
library(mgcv)
library(pROC)
library(caTools)
library(DiceEval)
library(plyr)
library(tools)
library(biomod2)
library(ncdf4)
library(spatialEco)
library(rgdal)
library(SDMTools)
library(sp)
?evaluate

##########################################################################################################
###########################################################################################################
#4. Calculating centroid values for rasters 
#bringing in rasters and then calculating centroid values through a for loop

setwd(PA_raster_dir)
folders<-list.files();folders
Centroids_Stats<-as.data.frame(NULL)
for (folder in folders) {
  print(paste("Starting PA process for ",folder,sep=""))
  setwd(paste(PA_raster_dir,"/",folder,sep=""))
  raster_data<-list.files(pattern=".tif")
  rs_stk_PA<-stack(raster_data)
  print(paste("Starting probability rasters process for ",folder,sep=""))
  setwd(paste(All_prob_dir,"/",folder,sep=""))
  raster_data_prob<-list.files(pattern=".tif")
  rs_stk_prob<-stack(raster_data_prob)
  for (i in 1:nlayers(rs_stk_PA)) {
    tryCatch({
      print(paste("Starting loop of ",names(rs_stk_PA[[i]]),sep=""))
      layer=rs_stk_PA[[i]]
      name<-names(rs_stk_PA[[i]])
      layer_prob<-mask(rs_stk_prob[[i]],layer)
      cent<-as.data.frame(COGravity(x=layer_prob))
      cent_coords<-cbind(cent[1,],cent[3,]);colnames(cent_coords)<-c("Longitude","Latitude")
      cent_named_coords<-as.data.frame(cbind(name,cent_coords))
      print(paste("Compiling output stats for ", names(rs_stk_PA[[i]]),names(rs_stk_prob[[i]]),sep=" "))
      Centroid_Stats<-rbind(Centroid_Stats,cent_named_coords)},error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}

setwd(shared_git)
write.csv(Centroids_Stats,file="Centrod_Stats_CM26_StudyMetrics.csv")
?COG

###########################################################################################################
# 5. Calculating a number of landscape metrics from the SDM Toolbox.

# Outputs derived from the ClassStat function form SDM Toolbox
# [1] "X"                       "name"                    "class"                   "n.patches"
# [5] "total.area"              "prop.landscape"          "patch.density"           "total.edge"
# [9] "edge.density"            "landscape.shape.index"   "largest.patch.index"     "mean.patch.area"
# [13] "sd.patch.area"           "min.patch.area"          "max.patch.area"          "perimeter.area.frac.dim"
# [17] "mean.perim.area.ratio"   "sd.perim.area.ratio"     "min.perim.area.ratio"    "max.perim.area.ratio"
# [21] "mean.shape.index"        "sd.shape.index"          "min.shape.index"         "max.shape.index"
# [25] "mean.frac.dim.index"     "sd.frac.dim.index"       "min.frac.dim.index"      "max.frac.dim.index"
# [29] "total.core.area"         "prop.landscape.core"     "mean.patch.core.area"    "sd.patch.core.area"
# [33] "min.patch.core.area"     "max.patch.core.area"     "prop.like.adjacencies"   "aggregation.index"
# [37] "lanscape.division.index" "splitting.index"         "effective.mesh.size"     "patch.cohesion.index"

#bringing in rasters and then calculating landscape metrics via for loop
setwd(PA_raster_dir)
folders<-list.files();folders
Class_Stats<-as.data.frame(NULL)
for (folder in folders) {
  print(paste("Starting ",folder,sep=""))
  setwd(paste(PA_raster_dir,"/",folder,sep=""))
  raster_data<-list.files(pattern=".tif")
  rs_stk<-stack(raster_data)
  for (i in 1:nlayers(rs_stk)) {
    tryCatch({
      print(paste("Starting loop of ",names(rs_stk[[i]]),sep=""))
      layer=rs_stk[[i]]
      name<-names(rs_stk[[i]])
      stat<-as.data.frame(ClassStat(layer,cellsize = 8904, bkgd = NA, latlon = TRUE))
      stat_P<-as.data.frame(cbind(name,stat))
      print(paste("Compiling output stats for ", names(rs_stk[[i]]),sep=""))
      Class_Stats<-rbind(Class_Stats,stat_P)},error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
}
# setwd(shared_git)
# write.csv(Class_Stats,file="Class_Stats_CM26_StudyMetrics.csv")

# 
# #bringing in rasters and then calculating anumber of landscape metrics via lapply (doesnt work just now)
# setwd(PA_raster_dir)
# folders<-list.files(pattern="i");folders
# Class_Stats<-as.data.frame(NULL)
# CS_Operation<-lapply(folders,function(x) {
#   print(paste("Starting ",x,sep=""))
#   setwd(paste(PA_raster_dir,"/",x,sep=""))
#   raster_data<-list.files(pattern="2000.tif") 
#   rs_stk<-stack(raster_data)
#   Class_Stats<-lapply(rs_stk,function(y){
#       print(paste("Starting loop of ",names(y),sep=""))
#       layer=y
#       name<-names(y)
#       ClassStat(layer,cellsize = 8904, bkgd = NA, latlon = TRUE)
#       print(paste("Compiling output stats for ", names(y),sep=""))
#       })
#   return(Class_Stats)})



###########################################################################################################
# 6. Calculating Annual Overlap of Home Range and/or overlap between months accross years?






###########################################################################################################
#8. Determining which rasters have values and which don't. NB. Some empty rasters exist
##list of ALL LAYERS
setwd(All_prob_dir)
layers<-list.files(recursive = TRUE);layers
layers<-as.data.frame(lapply(basename(layers),file_path_sans_ext));layers
layers<-t(layers)
colnames(layers)<-c("name")
rownames(layers)<-NULL
layers<-as.data.frame(layers)

##writeout list of all rasters
setwd(shared_git)
write.csv(layers,file="layer_names_CM26.csv")

##bring in the list of metrics
metrics<-read.csv("Class_Stats_CM26_StudyMetrics.csv")
metrics_P<-subset(metrics,metrics$class==1)

#join full list of rasters to metrics
library(plyr)
metrics_joined<-join(metrics_P,layers,by="name",type="full",match="all")
?join

## write out the list of metrics as well as empty rasters
## NOTE that this list includes both presence patches and absence patches. 
## going to need to slim this down to just presence patches. 
write.csv(metrics_joined,file="Class_Stats_CM26_StudyMetrics_all.csv")


##Need to determine whether the empty rasters are really empty or whether they are just predictions of 0 probability 
# #converting NAs into meaningful values
metrics_all<-read.csv("Class_Stats_CM26_StudyMetrics_all.csv")
# names(metrics_all)
# metrics_all[["class"]][is.na(metrics_all[["class"]])] <- 0
# summary(metrics_all$class)

metrics_NA<-subset(metrics_all,is.na(metrics_all$class)==TRUE)
write.csv(metrics_NA,file="Class_Stats_CM2.6_StudyMetrics_NA.csv")









######################################################################################################

# #### Experimental code 



area(rs3.1)
rs3.1<-raster("d73_m03_2000.tif")
plot(rs3.1)
rs3.2<-raster("d73_m03_2099.tif")
plot(rs3.2)
rs9.1<-raster("d73_m09_2000.tif")
plot(rs9.1)
rs9.2<-raster("d73_m09_2099.tif")
plot(rs9.2)

rs73_stk<-stack(rs3.1,rs3.2,rs9.1,rs9.2)

test<-lapply(rs73_stk,COGravity)

rs3.1_COG<-as.matrix(COGravity(rs3.1))
rs3.2_COG<-as.matrix(COGravity(rs3.2))



rs3.1_asc<-asc.from.raster(rs3.1)
rs3.2_asc<-asc.from.raster(rs3.2)  

I_test<-Istat(rs3.1,rs3.2)

plot(rs);points(c(41.92,-70.06),c(42,-70.52))

# d73<-stack(rs_list)



setwd(test_prob_dir)
rs_3.1<-raster("d73_m03_2000.tif")
plot(rs_3.1)


rs_3.1_cnt<-contour(rs_3.1)
?contour

setwd(test_dir)
rs_list<-list.files(pattern="2000")

d73_2000<-stack(rs_list)

plot(d73_2000)


d73_2000_sum<-calc(d73_2000,sum)
plot(d73_2000_sum)
