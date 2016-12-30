####### Script to output models as RDS Files
# https://www.r-bloggers.com/a-better-way-of-saving-and-loading-objects-in-r/https://www.r-bloggers.com/a-better-way-of-saving-and-loading-objects-in-r/

###########################################################################################################
# 1. Removes everything in the working environment

rm(list=ls()) 

###########################################################################################################
# 2. Set the primary working directories
#You'll want to adjust these based on where your rasters and species data are stored on your computer. 

# primary_dir=paste("/Volumes/LACIE_SHARE/PROJECTS/MidAtlantic_GOM_Work_NOAA/Climate_SDM_Paper/Full_Run_PA_GAM_climatologicalpoints");setwd(primary_dir)# swap folder names if necessary
primary_dir=paste("/Users/jmchenry/Dropbox/CM26_WorkingOverChristmas/Full_Run_PA_GAM_climatologicalpoints/");setwd(primary_dir)
spp_dir=paste(primary_dir,"/Species_CSVs",sep="");#dir.create(spp_dir) # swap folder name if necessary
spp_model_dir=paste(primary_dir,"/species_models/",sep="");dir.create(spp_model_dir)


###########################################################################################################
# 3. Installing and loading all relevant libraries

# #install packages

# install.packages("mgcv")


#libraries needed
library(mgcv)

######################################################################
# 4. Loading species data
#IF data is already in multiple species CSVs, then bring in data using:
#set the directory
setwd(spp_dir)
#read in all csvs in the directory
temp = list.files(pattern=".csv$")  # fix when ready to run the whole dataset. Added a 0 to keep the numbers low initially.
for (i in 1:length(temp)) {assign(temp[i], read.csv(temp[i]))}


############################################################################################################  
# 5. Fitting full models and saving RDS files
#set the directory
setwd(spp_dir)
#species levels
species<-ls(pattern=".csv")  #gather a list of all csvs in the environment space
for (spp in species){
  print(paste("Starting ",spp,sep=""))
  Spp_name<-file_path_sans_ext(spp)
  dat<-get(spp) 
  dat=dat[complete.cases(dat),]
  fit<-gam(p_a ~s(Depth,bs="ts")+s(Rugosity,bs="ts")+s(st,bs="ts")+s(SS,bs="ts")+s(bs,bs="ts")+s(bt,bs="ts")+s(sh,bs="ts"),family=binomial("logit"),data=dat,method="GCV.Cp")
  print(paste("Saving model for ",spp,sep=""))
  setwd(spp_model_dir)
  saveRDS(fit, paste(Spp_name,".rds",sep=""))
  print(paste("Finished ",spp,sep=""))
}
