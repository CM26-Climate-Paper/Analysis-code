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
# primary_dir=paste("/Users/jmchenry/Dropbox/CM26_WorkingOverChristmas/Full_Run_PA_GAM_climatologicalpoints/");setwd(primary_dir)

# shared_git=paste("/Users/jmchenry/Dropbox/CM26_Climate_Paper_AnalysisCodeNonLACIE/")
shared_git_data=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/CM26_Climate_Paper_Shared/CM26_Climate_Paper_Shared/Data/")

spp_rasters=paste(primary_dir,"/ALL_Species_Projections/Species_Projections_all/",sep="")
spp_model_dir=paste(primary_dir,"/species_models/",sep=""); #dir.create(spp_model_dir)

output_dir=paste(primary_dir,"/ALL_Species_Projections/Species_Projections_stacked/",sep="");dir.create(output_dir)


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

######################################################################
# 4. Loading species data
#IF data is already in multiple species RDS files, then bring in data using:
#set directory
setwd(spp_model_dir)
#read in all csvs in the directory
temp = list.files(pattern=".rds")  # fix when ready to run the whole dataset. Added a 0 to keep the numbers low initially.
for (i in 1:length(temp)) {assign(temp[i], readRDS(temp[i]))}

#####################################################################
# 5. Finding a threshold (note that I just modified our existing loop script to save time. Although I'm sure now there is a jazzier way to do this) 
#species levels
species<-ls(pattern=".rds")  #gather a list of all csvs in the environment space
#Running k-fold cross-validation
Compiled_Thresh_CV_Output=as.data.frame(NULL)
for (spp in species){
  Spp_name<-file_path_sans_ext(spp)
  print(paste("Starting ",spp,sep=""))
  dat<-get(spp) 
  dat<-dat$model
  Thresh_CV_Output=as.data.frame(NULL)
  K_Fold<-1:10  # this can be jacked up as much as we want. It will take longer, but will ultimately smooth out the errors.    print(paste("cross validatin of ",spp," started",sep=""))
  for (k in K_Fold){
    print(paste("Starting k ",k," for ",Spp_name,sep=""))
    Fold_num=k
    trainIndex<-createDataPartition(dat$p_a,p=.5,list=FALSE)
    trainData<-dat[trainIndex,c(1:8)]
    testData<-dat[-trainIndex,c(1:8)]
    #Fit training model
    print(paste("Fit training model ",k," for ",Spp_name,sep=""))
    train_fit<-gam(p_a ~s(Depth,bs="ts")+s(Rugosity,bs="ts")+s(st,bs="ts")+s(SS,bs="ts")+s(bs,bs="ts")+s(bt,bs="ts")+s(sh,bs="ts"),family=binomial("logit"),data=trainData,method="GCV.Cp")
    train_predict<-predict.gam(train_fit,type=c("response"),se.fit=TRUE)
    train_output<-cbind(trainData,train_predict)
    train_ROC<-roc(train_output$p_a,train_output$fit) #calculated ROC curve based on the original and the predicted values)
    train_AUC<-train_ROC$auc
    #Fitting testing model and extracting CV stats
    print(paste("Fit testing model ",k," for ",Spp_name,sep=""))        
    test_predict<-predict.gam(train_fit,testData,type=c("response"),se.fit=TRUE)
    test_output<-cbind(testData,test_predict)
    test_ROC<-roc(test_output$p_a,test_output$fit) #calculated ROC curve based on the original and the predicted values)
    test_AUC<-test_ROC$auc
    test_Thresh<-coords(test_ROC, "best", ret = "threshold",best.method="youden") ## threshold selection that maximizes sensitivity and specificity. 
    Thresh_CV<-cbind(Fold_num,Spp_name,train_AUC,test_AUC,test_Thresh)
    Thresh_CV_Output<-rbind(Thresh_CV_Output,Thresh_CV)
    print(paste("Threshold calculation for k=",k," for ",Spp_name," finished",sep=""))
  }
Compiled_Thresh_CV_Output<-rbind(Thresh_CV_Output,Compiled_Thresh_CV_Output)
print(paste("cross validatin of ",Spp_name," finished",sep=""))
}

setwd(shared_git_data)
# write.csv(Compiled_Thresh_CV_Output,"CV_AUC_Thresh_out.csv")


# Gathering outputs of CV script into a useable data frame
Compiled_Thresh_CV_Output<-read.csv("CV_AUC_Thresh_out.csv")
names(Compiled_Thresh_CV_Output)
summary_data <- ddply(Compiled_Thresh_CV_Output, c("Spp_name"), summarise,
                      N    = length(Fold_num),
                      train_AUC_mean = mean(train_AUC),
                      train_AUC_se = sd(train_AUC)/sqrt(N),
                      test_AUC_mean = mean(test_AUC),
                      test_AUC_se = sd(test_AUC)/sqrt(N),
                      test_Thresh_mean = mean(test_Thresh),
                      test_Thresh_se = sd(test_Thresh)/sqrt(N))
head(summary_data)
write.csv(summary_data,file="CV_AUC_Thresh_out_FINAL.csv")


##################################################################
# 6. Bringing in rasters, saving them as RDS files, converting to PA based on the identified threshold, and saving them again as RDS files. 

### NEED TO FILL EMPTY RASTERS FIRST. THEN NEED TO 

#bringing in threshold spreadsheet
setwd(shared_git_data)
Thresh_output<-read.csv("CV_AUC_Thresh_out_FINAL.csv")


setwd(spp_rasters)
rs_folders<-list.files()
folder="bp1006"
for (folder in rs_folders){
  print(paste("Starting ",folder,sep=""))
  setwd(paste(spp_rasters,"/",folder,sep=""))
  raster_data<-list.files(pattern=".tif") 
  stk<-stack(raster_data)
  print(paste(folder," Stacked",sep=""))
  names(stk)<-raster_data
  saveRDS(stk,paste(folder,".rds",sep=""))
  cutoff<-subset(Thresh_output,Thresh_output$Spp_name==folder)
  rc_fun <- function(x) {ifelse(x <=  cutoff$Thresh,NA,ifelse(x >  cutoff$Thresh, 1, NA)) }
  stk_rc<-calc(stk, fun=rc_fun)
  print(paste("Cutoff used to reclassify raster for ",folder,sep=""))
  stk_rc<-stack(stk_rc)
  spp_output_dir=paste(output_dir,folder,sep="");dir.create(spp_output_dir)
  setwd(spp_output_dir)
  saveRDS(stk_rc,paste(folder,"_PA.rds",sep=""))
  print(paste("Rewriting binary rasters",folder,sep=""))
  writeRaster(stk_rc,filename=names(stk_rc),bylayer=TRUE,datatype="INT2S",options="INTERLEAVE=BAND",proj=TRUE,overwrite=TRUE)
  print(paste(folder," finished",sep=""))
}




