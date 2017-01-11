#Methods_Trial_Landscape_Metrics 

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

###########################################################################################################
# 5. Calculating a number of landscape metrics from the SDM Toolbox + centroids

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
PA_raster_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/Species_Projections_all_PA/"
prob_raster_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/Species_Projections_all/"
out_csv_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/Habitat_Metrics/";dir.create(out_csv_dir)

## get an empty dataframe to fill in when rasters have no presence
templateDF=read.csv("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/Habitat_Metrics/templateDF.csv")
#template=templateDF[1,]
#template[1,]="NaN"
templateDF=template[,c(2:39)]
write.csv(templateDF,paste(out_csv_dir,"templateDF.csv"))

setwd(PA_raster_dir)
folders<-list.files();folders
for (folder in folders) {
  print(paste("Starting ",folder,sep=""))
  rs_stk_PA=readRDS(paste(PA_raster_dir,folder,"/",folder,"_PA.rds",sep=""))
  rs_stk_prob=readRDS(paste(prob_raster_dir,folder,"/",folder,".rds",sep=""))
  Class_Stats<-as.data.frame(NULL)
  for (i in 1:nlayers(rs_stk_PA)) {
      print(paste("Starting loop of ",names(rs_stk_PA[[i]]),sep=""))
      pa_layer=rs_stk_PA[[i]]
      
      ## get the species name and date of layer
      name<-names(rs_stk_PA[[i]])
      a=unlist(strsplit(name,"_"))
      b=paste("01/",gsub("m","",a[2]),"/",gsub(".tif","",a[3]),sep="")
      if(grepl("2000",b)==TRUE){ ## put 2000 at the end where it belongs
        b=(gsub("2000","2100",b))
      }
      date=as.Date(b,format = "%d/%m/%Y") ## generic 01 as date
      
      ## calculate Class Stats
      stat<-as.data.frame(ClassStat(pa_layer,cellsize = 8904, bkgd = NA, latlon = TRUE))
      
      if((length(stat)==0)==TRUE){
        print("no data, using adding blank row")
        stat=templateDF
      }
      stat_P<-as.data.frame(cbind(date,stat))
      stat_P=as.data.frame(cbind(name,stat_P))
      
      ## calculate centroid lat/log
      prob_layer=rs_stk_prob[[name]]
      masked_prob<-mask(prob_layer,pa_layer)
      cent<-as.data.frame(COGravity(x=masked_prob))
      cent_coords<-as.data.frame(cbind(cent[1,],cent[3,]));colnames(cent_coords)<-c("Centroid_Longitude","Centroid_Latitude")
      
      ## combine into final dataframe for all layers for a given species
      hab_metrics=cbind(stat_P,cent_coords)
      print(paste("Compiling output stats for ", names(rs_stk_PA[[i]]),sep=""))
      Class_Stats<-rbind(Class_Stats,hab_metrics)
    
      ## add a numeric marker for temporal order
      sorted=Class_Stats[order(Class_Stats$date),]
      sorted$TemporalOrder=seq(1:nrow(sorted))
      write.csv(sorted,file=paste(out_csv_dir,folder,"_HabitatMetrics.csv",sep=""))}
  }
}







