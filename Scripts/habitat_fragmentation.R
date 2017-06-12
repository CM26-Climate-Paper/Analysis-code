# habitat fragmentation

setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/time_series_clusters")

library(stlplus)

library(fpc) #http://www.statmethods.net/advstats/cluster.html ; www.uvm.edu/rsenr/gradgis/advanced/clusteranalysis.pptx
# contains pamk for determining the optimal number of cluster: https://www.rdocumentation.org/packages/fpc/versions/2.1-10/topics/pamk

library(cluster)
# https://cran.r-project.org/web/packages/cluster/cluster.pdf
#use pam or pamk to find number of clusters

##PDC distance measure
library(pdc) # https://cran.r-project.org/web/packages/pdc/pdc.pdf
# https://www.jstatsoft.org/article/view/v067i05

##Multiple distance measures
library(TSclust) # https://cran.r-project.org/web/packages/TSclust/TSclust.pdf

##hclust() https://stat.ethz.ch/R-manual/R-devel/library/stats/html/hclust.html
# can cluster a bunch of different distance matrices created in TSclust or pdc

library(ggplot2)
library(reshape2)
library(dplyr)
library(ggplot2)
library(reshape2)
library(plyr)
library(vegan)

########################### comparing year 1 (climatology) to year 81

##Number patches

setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/time_series_clusters")
raw=readRDS("array_raw.rds")
n_patches=as.data.frame(raw[,,1]) #first sheet is n_patches
row.names(n_patches)=c(1:972)

n_patches$year=rep(c(1:81),each=12)
n_patches=aggregate(n_patches[,1:147], list(n_patches$year), mean,na.rm=TRUE, na.action=NULL)
n_patches=n_patches[,2:ncol(n_patches)]
n_patches=t(n_patches)
colnames(n_patches)=c(1:81)

hab_frag=n_patches
hab_frag=hab_frag[,c(1,81)]
hab_frag=as.data.frame(hab_frag)

##Total area

n_patches=as.data.frame(raw[,,2]) #first sheet is n_patches
row.names(n_patches)=c(1:972)

## aggregating by year
n_patches$year=rep(c(1:81),each=12)
n_patches=aggregate(n_patches[,1:147], list(n_patches$year), mean,na.rm=TRUE, na.action=NULL)
n_patches=n_patches[,2:ncol(n_patches)]
n_patches=t(n_patches)
colnames(n_patches)=c(1:81)

hab_frag$Total_area_1=n_patches[,1]
hab_frag$Total_area_81=n_patches[,81]

##patch.cohesion.index

n_patches=as.data.frame(raw[,,8]) #first sheet is n_patches
row.names(n_patches)=c(1:972)

## aggregating by year
n_patches$year=rep(c(1:81),each=12)
n_patches=aggregate(n_patches[,1:147], list(n_patches$year), mean,na.rm=TRUE, na.action=NULL)
n_patches=n_patches[,2:ncol(n_patches)]
n_patches=t(n_patches)
colnames(n_patches)=c(1:81)

hab_frag$cohesion_1=n_patches[,1]
hab_frag$cohesion_81=n_patches[,81]

## Centroid_Latitude

n_patches=as.data.frame(raw[,,9]) #first sheet is n_patches
row.names(n_patches)=c(1:972)

## aggregating by year
n_patches$year=rep(c(1:81),each=12)
n_patches=aggregate(n_patches[,1:147], list(n_patches$year), mean,na.rm=TRUE, na.action=NULL)
n_patches=n_patches[,2:ncol(n_patches)]
n_patches=t(n_patches)
colnames(n_patches)=c(1:81)

hab_frag$Centroid_Latitude_1=n_patches[,1]
hab_frag$Centroid_Latitude_81=n_patches[,81]

setwd("/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/Scripts")
write.csv(hab_frag,"hab_frag.csv")

### trying cluster anlysis on deltas (1:81)

hab=read.csv("hab_fag_csv.csv")
hab$Species=gsub("_HabitatMetrics.csv","",hab$Species)
row.names(hab)=hab$Species
habt=as.data.frame(t(hab))
habt=habt[2:6,]
hab1=hab[,2:ncol(hab)]
nmds=metaMDS(hab1,distance="minkowski",trymax = 50,plot=TRUE)
nmds=metaMDS(habt,distance="euclidean",plot=TRUE)

d=dist(hab1,method="euclidean")
nmds=metaMDS(d,trymax = 50,plot=TRUE)

plot(nmds)

fit=princomp(hab1,cor=TRUE)
plot(fit,type="lines")
biplot(fit)

hab2=hab1[,1:4]
fit=princomp(hab2,cor=TRUE)
plot(fit,type="lines")
biplot(fit)

a=pamk(hab1)
clusplot(hab1,a$pamobject$clustering,lines=1)


########################### comparing year 1 (climatology) to average years 61-81

##Number patches (NP)

setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/time_series_clusters")
raw=readRDS("array_raw.rds")
n_patches=as.data.frame(raw[,,1]) #first sheet is n_patches
row.names(n_patches)=c(1:972)

n_patches$year=rep(c(1:81),each=12)
n_patches=aggregate(n_patches[,1:147], list(n_patches$year), mean,na.rm=TRUE, na.action=NULL)
n_patches=n_patches[,2:ncol(n_patches)]
n_patches=t(n_patches)
colnames(n_patches)=c(1:81)

hab_frag=as.data.frame(n_patches)
hab_frag$NP_clim=hab_frag[,1]
hab_frag$NP_60_80=rowMeans(hab_frag[,c(61:81)])
hab_frag=hab_frag[,c(82:83)]

##Total area (TA)

n_patches=as.data.frame(raw[,,2]) #first sheet is n_patches
row.names(n_patches)=c(1:972)

## aggregating by year
n_patches$year=rep(c(1:81),each=12)
n_patches=aggregate(n_patches[,1:147], list(n_patches$year), mean,na.rm=TRUE, na.action=NULL)
n_patches=n_patches[,2:ncol(n_patches)]
n_patches=t(n_patches)
colnames(n_patches)=c(1:81)

hab_frag$TA_clim=1e-06*n_patches[,1] ##m2 to km2
hab_frag$TA_60_80=1e-06*rowMeans(n_patches[,c(61:81)]) ##m2 to km2

##patch.cohesion.index (PC)

n_patches=as.data.frame(raw[,,8]) #first sheet is n_patches
row.names(n_patches)=c(1:972)

## aggregating by year
n_patches$year=rep(c(1:81),each=12)
n_patches=aggregate(n_patches[,1:147], list(n_patches$year), mean,na.rm=TRUE, na.action=NULL)
n_patches=n_patches[,2:ncol(n_patches)]
n_patches=t(n_patches)
colnames(n_patches)=c(1:81)

hab_frag$PC_clim=n_patches[,1]
hab_frag$PC_60_80=rowMeans(n_patches[,c(61:81)])

## Centroid_Latitude (CL)

n_patches=as.data.frame(raw[,,9]) #first sheet is n_patches
row.names(n_patches)=c(1:972)

## aggregating by year
n_patches$year=rep(c(1:81),each=12)
n_patches=aggregate(n_patches[,1:147], list(n_patches$year), mean,na.rm=TRUE, na.action=NULL)
n_patches=n_patches[,2:ncol(n_patches)]
n_patches=t(n_patches)
colnames(n_patches)=c(1:81)

hab_frag$CL_clim=n_patches[,1]
hab_frag$CL_60_80=rowMeans(n_patches[,c(61:81)])

## Averge Patch size (AP)
hab_frag$AP_clim=hab_frag$TA_clim/hab_frag$NP_clim
hab_frag$AP_60_80=hab_frag$TA_60_80/hab_frag$NP_60_80

## absolute change
hab_frag$NP_delta=hab_frag$NP_60_80-hab_frag$NP_clim
hab_frag$TA_delta=hab_frag$TA_60_80-hab_frag$TA_clim
hab_frag$PC_delta=hab_frag$PC_60_80-hab_frag$PC_clim
hab_frag$CL_delta=hab_frag$CL_60_80-hab_frag$CL_clim
hab_frag$AP_delta=hab_frag$AP_60_80-hab_frag$AP_clim

## % change
hab_frag$NP_PerChange=hab_frag$NP_delta/hab_frag$NP_clim*100
hab_frag$TA_PerChange=hab_frag$TA_delta/hab_frag$TA_clim*100
hab_frag$PC_PerChange=hab_frag$PC_delta/hab_frag$PC_clim*100
hab_frag$CL_PerChange=hab_frag$CL_delta/hab_frag$CL_clim*100
hab_frag$AP_PerChange=hab_frag$AP_delta/hab_frag$AP_clim*100

hab_frag$species=rownames(hab_frag)
hab_frag$species=gsub("_HabitatMetrics.csv","",hab_frag$species)
rownames(hab_frag)<-hab_frag$species

## standardized (both sign - e.g. increase in metric = incease in habitat frag, and in 0-1 range)
hab_frag$NP_delta_S=(hab_frag$NP_delta-min(hab_frag$NP_delta))/(max(hab_frag$NP_delta)-min(hab_frag$NP_delta))
hab_frag$TA_delta_S=1-(hab_frag$TA_delta-min(hab_frag$TA_delta))/(max(hab_frag$TA_delta)-min(hab_frag$TA_delta))
hab_frag$PC_delta_S=1-(hab_frag$PC_delta-min(hab_frag$PC_delta))/(max(hab_frag$PC_delta)-min(hab_frag$PC_delta))
#hab_frag$CL_delta_S=1-(hab_frag$CL_delta-min(hab_frag$CL_delta))/(max(hab_frag$CL_delta)-min(hab_frag$CL_delta))
hab_frag$AP_delta_S=1-(hab_frag$AP_delta-min(hab_frag$AP_delta))/(max(hab_frag$AP_delta)-min(hab_frag$AP_delta))

## calculating relative severitity of habitat fragmentation (higher values = higher severity of habitat fragmentation)
#hab_frag$severity=rowSums(hab_frag[,c(hab_frag$NP_delta_S,hab_frag$TA_delta_S,hab_frag$PC_delta_S,hab_frag$AP_delta_S)])
hab_frag$severity=rowSums(hab_frag[,c(22:25)])

## attach species information
setwd("/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/Scripts")
info=read.csv("sp_codes.csv")
joinn=join(hab_frag,info,by="species")

setwd("/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/Scripts")
write.csv(joinn,"hab_frag_clim_20_60_codes.csv")


#### severity bar plot
#install.packages("plotly")
library(plotly)
join2=joinn[with(joinn,order(guild,severity)),]
p=plot_ly(x=join2$severity,y=join2$Common.Name)

library(ggplot2)
library(reshape)
library(reshape2)
join1=join2[,c(26,30,32)]
join3=reshape2::melt(join1,id.var=c("guild","Common.Name"))
#md=melt(join2,id=c("guild","species","severity"))
#join4=join3[join3$variable=="severity",]
join4$value=as.numeric(join4$value)
join4$guild=factor(join4$guild,levels=join4$guild[order(join4$value)])
ggplot(join1,aes(x=Common.Name),y=severity))+
  geom_bar(stat="identity") #+
  #coord_flip()

# join1=melt(join2[,c(26,30,32)],id.vars = "Common.Name","guild")
# ggplot(join1,aes(x=guild,y=value))


####################   this shit works
join1=join2[,c(12,26,29,32)]
#ggplot(join1,aes(x=guild,y=value))

#join3=melt(join1,id.vars="guild","Common.Name")

join1$order=0
join1$order[join1$guild=="p"]<-1
join1$order[join1$guild=="m"]<-2
join1$order[join1$guild=="i"]<-3
join1$order[join1$guild=="e"]<-4
join1$order[join1$guild=="d"]<-5
join1$order[join1$guild=="bp"]<-6
join1$rank=c(1:147)

join1$area=""
join1$area[join1$TA_delta>0]<-"Increase in total available habitat"
join1$area[join1$TA_delta<0]<-"Decrease in total available habitat"

pdf("severity.jpg")
ggplot(join1,aes(x=reorder(Scientific.Name,rank),y=severity,color=as.factor(guild)))+
  geom_bar(stat="identity",fill="white")+
  coord_flip()
dev.off()


ggplot(join1,aes(x=reorder(Scientific.Name,rank),y=severity,color=area))+
  geom_bar(stat="identity",fill="white")+theme_minimal()+
  coord_flip()

ggplot(join1,aes(x=reorder(Scientific.Name,severity),y=severity,color=area))+
  geom_bar(stat="identity",fill="white")+theme_minimal()+
  coord_flip()

a=readRDS("d172_PA.rds")
b=a[[c(1,953:972)]]
c=mean(b)
g=calc(b,fun = mean,na.rm=FALSE)
plot(g)
f=calc(b,fun = mean,na.rm=TRUE)
plot(f)
d=a[[2]]
plot(d)

library(rgdal)
SA=readOGR(dsn="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/species_data/study_area","study_area")
plot(SA,add=TRUE)
plot(d,add=TRUE)
