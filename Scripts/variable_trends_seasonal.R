######### new script to display the temporal trends of predictors

###### code from Jenn ####
ggplot(FLpL, aes(x = Year, y = value)) +
  geom_point() +
  facet_wrap(~ variable, ncol = 12) +
  geom_smooth(method = lm) +
  xlab("") + ylab("Precipitation (in)") +
  theme(axis.text.x = element_text(angle = 45))

## https://lh3.googleusercontent.com/-406sBFCWwaY/WdPKAI0itJI/AAAAAAAAIjU/75-0aV0-3-sp_oMXLwFp3zUs3OsXGn3MgCL0BGAYYCw/h1505/2017-10-03.png
######

######### ----------------------------> define global objects ####
library(tidyverse)
library(raster)

######### ----------------------------> create contemporary rasters averaged by season ####
contempDir_monthly="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project" ## monthly current rasters
contempDir_seaonal="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/contemporary" #;dir.create(contempDir_seaonal) ## folder to house current rasters averaged by season

winter=c("m12","m01","m02")
spring=c("m03","m04","m05")
summer=c("m06","m07","m08")
fall=c("m09","m10","m11")

seasons=c("DJF","MAM","JJA","SON")

  for(season in seasons){
    print(season)
    a=list()
    assign(season,a)
  }

vars=c("bt","bs","st","SS","sh")

allfiles=list.files(contempDir_monthly,recursive = T,full.names = T)

for(file in allfiles){
  print(file)
  a=strsplit(file,"/")[[1]][9]
  b=strsplit(a,"_")
  month=b[[1]][1]
  year=b[[1]][2]
  
  ########## 20-40
  if(month %in% winter){
    DJF=unlist(list(file,DJF))
  }
  if(month %in% spring){
    MAM=unlist(list(file,MAM))
  }
  if(month %in% summer){
    JJA=unlist(list(file,JJA))
  }
  if(month %in% fall){
    SON=unlist(list(file,SON))
  }
}

master=list()
  for(season in seasons){
    print(season)
    master=unlist(list(master,season))
  }

for(mas in master){
  print(mas)
  a=paste0(contempDir_seaonal,"/contemp_",mas);dir.create(a)
  bs=grep("bs",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(bs,paste0(contempDir_seaonal,"/contemp_",mas,"/bs.tif"),format="GTiff")
  bt=grep("bt",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(bt,paste0(contempDir_seaonal,"/contemp_",mas,"/bt.tif"),format="GTiff")
  st=grep("st",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(st,paste0(contempDir_seaonal,"/contemp_",mas,"/st.tif"),format="GTiff")
  SS=grep("SS",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(SS,paste0(contempDir_seaonal,"/contemp_",mas,"/SS.tif"),format="GTiff")
  sh=grep("sh",get(mas),value = T)%>%stack(.)%>%calc(.,fun = mean);writeRaster(sh,paste0(contempDir_seaonal,"/contemp_",mas,"/sh.tif"),format="GTiff")
}
#########

######### ----------------------------> find spatial averages for all rasters in contemp and project ####
empty=data.frame(period=NA,season=NA,var=NA,s.mean=NA)
contemp=list.files(contempDir_seaonal,full.names = T,recursive = T)
proj=list.files("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/project",full.names = T,recursive = T)
layersList=unlist(list(contemp,proj))

a=grep("Rugosity",layersList)
b=grep("Depth",layersList)
remove=unlist(list(a,b))
layersList=layersList[-remove] ### getting rid of depth and rugosity

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

for(i in 1:length(layersList)){  ### extracting metrics for each layer and writing to empty
a=strsplit(layersList[i],"/")
var=gsub(".tif","",a[[1]][11])
season=substrRight(a[[1]][10],3)
period=substr(a[[1]][10],1,nchar(a[[1]][10])-4)
s.mean=raster(layersList[i])%>%cellStats(.,stat=mean)

empty[i,1]=period
empty[i,2]=season
empty[i,3]=var
empty[i,4]=s.mean
}

##### cleaning up dataframe
xLevels=c("contemp","av20_40", "av40_60","av60_80")
empty$period=as.factor(empty$period)
empty$season=as.factor(empty$season)
empty$var=as.factor(empty$var)
empty$period_ordered=factor(empty$period,levels=xLevels)
write.csv(empty,"/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/data/means.csv") ## cleaning names by hand, quicker

empty=read.csv("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/data/means.csv")
period_Levels=c("Contemporary","+20-40 years", "+40-60 years","+60-80 years")
season_Levels=c("Winter","Spring", "Summer","Fall")
var_Levels=c("Surface temperature","Bottom temperature","Surface salinity","Bottom salinity","Sea height")

empty$period_ordered=factor(empty$Period,levels=period_Levels)
empty$season_ordered=factor(empty$season,levels=season_Levels)
empty$var_ordered=factor(empty$var,levels=var_Levels)

######### ----------------------------> plotting 4x5 grid, each is variable by season, x = period ####

a=ggplot(empty,aes(period_ordered,s.mean,group=1))+geom_line()
a+facet_grid(var_ordered~season_ordered,scales = "free_y")+theme(strip.text.y = element_text(size=5))+
  theme(axis.text.x = element_text(hjust=1,vjust=1,angle = 45))+labs(x="Temporal period")+labs(y="Mean value")

pdf("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/data/grid.pdf")
  a=ggplot(empty,aes(period_ordered,s.mean,group=1))+geom_line()
  a+facet_grid(var_ordered~season_ordered,scales = "free_y")+theme(strip.text.y = element_text(size=5))+
    theme(axis.text.x = element_text(hjust=1,vjust=1,angle = 45))+labs(x="Temporal period")+labs(y="Mean value")
dev.off()  
