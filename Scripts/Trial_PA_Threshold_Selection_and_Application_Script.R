####### Quick script to evaluate PA cutoffs. 
#http://stackoverflow.com/questions/16347507/obtaining-threshold-values-from-a-roc-curve
#https://cran.r-project.org/web/packages/pROC/pROC.pdf

###########################################################################################################
# 1. Removes everything in the working environment

rm(list=ls()) 

###########################################################################################################
# 2. Set the primary working directories
#You'll want to adjust these based on where your rasters and species data are stored on your computer. 

primary_dir=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/Full_Run_PA_GAM_climatologicalpoints");setwd(primary_dir)# swap folder names if necessary
shared_git=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/CM26_Climate_Paper_Shared/CM26_Climate_Paper_Shared/")
spp_dir=paste(primary_dir,"/Species_CSVs",sep="");#dir.create(spp_dir) # swap folder name if necessary
spp_rasters=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/Full_Run_PA_GAM_climatologicalpoints/ALL_Species_Projections/Species_Projections_all/")
output_dir=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/Full_Run_PA_GAM_climatologicalpoints/ALL_Species_Projections/Species_Projections_stacked/");dir.create(output_dir)

###########################################################################################################
# 3. Installing and loading all relevant libraries

#install packages
install.packages("dplyr")
install.packages("doBy")
install.packages("dismo")
install.packages("mgcv")
install.packages("pROC")
install.packages("caTools")
install.packages("DiceEval")
install.packages("biomod2")
install.packages("ncdf4")

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
?evaluate

######################################################################
# 5. Loading species data
#IF data is already in multiple species CSVs, then bring in data using:
#set the directory
setwd(spp_dir)
#read in all csvs in the directory
temp = list.files(pattern=".csv$")  # fix when ready to run the whole dataset. Added a 0 to keep the numbers low initially.
for (i in 1:length(temp)) {assign(temp[i], read.csv(temp[i]))}

############################################################################################################  
# 6. Finding a threshold (note that I just modified our existing loop script to save time. Although I'm sure now there is a jazzier way to do this) 

#species levels
species<-ls(pattern=".csv")  #gather a list of all csvs in the environment space
#Running k-fold cross-validation
Thresh_output=as.data.frame(NULL)
for (spp in species){
  Spp_name<-file_path_sans_ext(spp)
  dat<-get(spp) 
  dat=dat[complete.cases(dat),]
  fit<-gam(p_a ~s(Depth,bs="ts")+s(Rugosity,bs="ts")+s(st,bs="ts")+s(SS,bs="ts")+s(bs,bs="ts")+s(bt,bs="ts")+s(sh,bs="ts"),family=binomial("logit"),data=dat,method="GCV.Cp")
  predict<-predict.gam(fit,type=c("response"),se.fit=TRUE)
  predict_out<-cbind(dat,predict)
  ROC<-roc(predict_out$p_a,predict_out$fit) #calculated ROC curve based on the original and the predicted values)
  Thresh<-coords(ROC, "best", ret = "threshold",best.method="youden") ## threshold selection that maximizes sensitivity and specificity. 
  Thresh_Coords<-cbind(Spp_name,Thresh)
  Thresh_output<-rbind(Thresh_output,Thresh_Coords)
  print(paste("Threshold calculation for ",spp," finished",sep=""))
}
setwd(shared_git)
write.csv(Thresh_output,"Thresh_out.csv")

##################################################################
# 7. Bringing in rasters and stacking them

#bringing in threshold spreadsheet
setwd(shared_git)
Thresh_output<-read.csv("Thresh_out.csv")


#bringing in rasters and then converting to PA based on the identified threshold, and saving them again
setwd(spp_rasters)
rs_folders<-list.files()

for (folder in rs_folders){
  print(paste("Starting ",folder,sep=""))
  setwd(paste(spp_rasters,"/",folder,sep=""))
  raster_data<-list.files() 
  stk<-stack(raster_data)
  print(paste(folder," Stacked",sep=""))
  names(stk)<-raster_data
  cutoff<-subset(Thresh_output,Thresh_output$Spp_name==folder)
  rc_fun <- function(x) {ifelse(x <=  cutoff$Thresh,0,ifelse(x >  cutoff$Thresh, 1, NA)) }
  stk_rc<-calc(stk, fun=rc_fun)
  print(paste("Cutoff used to reclassify raster for ",folder,sep=""))
  stk_rc<-stack(stk_rc)
  spp_output_dir=paste(output_dir,folder,sep="");dir.create(spp_output_dir)
  setwd(spp_output_dir)
  print(paste("Rewriting binary rasters",folder,sep=""))
  writeRaster(stk_rc,filename=names(stk_rc),bylayer=TRUE,datatype="INT2S",options="INTERLEAVE=BAND",proj=TRUE,overwrite=TRUE)
  print(paste(folder," finished",sep=""))
}




