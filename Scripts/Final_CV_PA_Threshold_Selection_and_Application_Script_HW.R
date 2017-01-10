####### Quick script to evaluate PA cutoffs. 
#http://stackoverflow.com/questions/16347507/obtaining-threshold-values-from-a-roc-curve
#https://cran.r-project.org/web/packages/pROC/pROC.pdf

###########################################################################################################
# 1. Removes everything in the working environment

rm(list=ls()) 

###########################################################################################################
# 2. Set the primary working directories
#You'll want to adjust these based on where your rasters and species data are stored on your computer. 

primary_dir=paste("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/");setwd(primary_dir)# swap folder names if necessary

#shared_git=paste("/Users/jmchenry/Dropbox/CM26_Climate_Paper_AnalysisCodeNonLACIE/")
shared_git_data=paste("/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/Data")

spp_rasters=paste(primary_dir,"Species_Projections_all/",sep="")
#spp_model_dir=paste(primary_dir,"/species_models/",sep=""); #dir.create(spp_model_dir)

output_dir=paste(primary_dir,"Species_Projections_all_PA/",sep="");dir.create(output_dir)


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
install.packages("caret")

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
library(caret)
library(plyr)
?evaluate
?createFolds

#bringing in threshold spreadsheet
setwd(shared_git_data)
Thresh_output<-read.csv("CV_AUC_Thresh_out_FINAL.csv")

setwd(spp_rasters)
rs_folders<-list.files() ##
for (folder in rs_folders){
  if(!file.exists(paste(output_dir,folder,sep=""))){ ## classifcation failed, restarting
    print(paste(folder," hasn't been classified yet",sep=""))
    print(paste("Starting ",folder,sep=""))
    setwd(paste(spp_rasters,"/",folder,sep=""))
    raster_data<-list.files(pattern="*.tif$") ##change to only grab tifs
    stk<-stack(raster_data)
    print(paste(folder," Stacked",sep=""))
    names(stk)<-raster_data
    saveRDS(stk,paste(folder,".rds",sep=""))
    cutoff<-subset(Thresh_output,Thresh_output$Spp_name==folder)
    rc_fun <- function(x) {ifelse(x <=  cutoff$test_Thresh_mean,NA,ifelse(x >  cutoff$test_Thresh_mean, 1, NA)) } ### training/testing threshold??
    stk_rc<-calc(stk, fun=rc_fun)
    print(paste("Cutoff used to reclassify raster for ",folder,sep=""))
    stk_rc<-stack(stk_rc)
    spp_output_dir=paste(output_dir,folder,sep="");dir.create(spp_output_dir)
    setwd(spp_output_dir)
    saveRDS(stk_rc,paste(folder,"_PA.rds",sep=""))
    print(paste("Rewriting binary rasters ",folder,sep=""))
    #writeRaster(stk_rc,filename=names(stk_rc),bylayer=TRUE,datatype="INT2S",options="INTERLEAVE=BAND",proj=TRUE,overwrite=TRUE)
    print(paste(folder," finished",sep=""))
  }
}

####### not all PA rasters have data
##### 1. get list of missing PA rasters
PA_raster_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/Species_Projections_all_PA/"
setwd(PA_raster_dir)
folders<-list.files();folders
blank=list()
for(folder in folders){
  rs_stack=readRDS(paste(PA_raster_dir,folder,"/",folder,"_PA.rds",sep=""))
  for(i in 1:nlayers(rs_stack)){
    name=names(rs_stack[[i]])
    if(is.na(maxValue(rs_stack[[i]]))==TRUE){
      blank=list(blank,name)
    }
  }
}


final_list=unlist(blank) ## list of blank rasters with path

### reclassify missing PA rasters    NEVERMIND these are empty because they don't have any habitat suitability above threshold

for(missing in final_list){
  species=unlist(strsplit(missing,"_"))
  species=species[1]
  path=paste(spp_rasters,species,"/",missing,sep = "")
  probraster=raster(path)
  cutoff<-subset(Thresh_output,Thresh_output$Spp_name==species)
  rc_fun <- function(x) {ifelse(x <=  cutoff$test_Thresh_mean,NA,ifelse(x >  cutoff$test_Thresh_mean, 1, NA)) }
  pa=calc(probraster,fun = rc_fun)
}
  
  