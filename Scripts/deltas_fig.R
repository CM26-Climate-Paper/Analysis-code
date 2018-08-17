## deltas figure
library(tidyverse)
library(raster)
library(RColorBrewer)
library(ggmap)
library(grid)
library(lattice)
library(gridExtra)
library(maptools)
library(viridis) 
library(cowplot)
library(stringr)
library(DataCombine)

delta_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/CM2.6/rasters_point08deg";setwd(delta_dir)
total_change=list.files(pattern = "99",full.names = T)
bt=grep("bt_",total_change,value = T) %>% stack() %>% mean() %>% as(.,"SpatialPixelsDataFrame") %>% as.data.frame() 
bs=grep("bs_",total_change,value = T) %>% stack() %>% mean() %>% as(.,"SpatialPixelsDataFrame") %>% as.data.frame() 
st=grep("st_",total_change,value = T) %>% stack() %>% mean() %>% as(.,"SpatialPixelsDataFrame") %>% as.data.frame() 
SS=grep("SS_",total_change,value = T) %>% stack() %>% mean() %>% as(.,"SpatialPixelsDataFrame") %>% as.data.frame() 
sh=grep("sh_",total_change,value = T) %>% stack() %>% mean() %>% as(.,"SpatialPixelsDataFrame") %>% as.data.frame() 

map.US <- map_data(map="state")
map.world = map_data(map="world")

map=ggplot()+geom_map(data=map.US,aes(map_id=region),map=map.US)+coord_cartesian()
map=map+geom_raster(data=bt,aes(x=x,y=y,fill=layer),alpha=.8)+coord_cartesian()+scale_fill_gradient(low = "darkgoldenrod1", high = "blue",guide = "colourbar")
map=map+coord_cartesian(xlim=c(-76.5,-65.5),ylim=c(34,45),expand=F)
map=map+theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
map=map+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+labs(x="lon")+labs(y="lat") + guides(fill=guide_legend(title="Bottom temperature (°C)"))
map=map+theme(legend.position=c(.9,.5),legend.justification = c(.9,.9))+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5),legend.title=element_text(size=5))+ theme(legend.key=element_blank())+theme(legend.key.size = unit(.5,'lines'))
bt_m=map
bt_m

map=ggplot()+geom_map(data=map.US,aes(map_id=region),map=map.US)+coord_cartesian()
map=map+geom_raster(data=st,aes(x=x,y=y,fill=layer),alpha=.8)+coord_cartesian()+scale_fill_gradient(low = "darkgoldenrod1", high = "blue",guide = "colourbar")
map=map+coord_cartesian(xlim=c(-76.5,-65.5),ylim=c(34,45),expand=F)
map=map+theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
map=map+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+labs(x="lon")+labs(y="lat") + guides(fill=guide_legend(title="Surface temperature (°C)"))
map=map+theme(legend.position=c(.9,.5),legend.justification = c(.9,.9))+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5),legend.title=element_text(size=5))+ theme(legend.key=element_blank())+theme(legend.key.size = unit(.5,'lines'))
st_m=map
st_m

map=ggplot()+geom_map(data=map.US,aes(map_id=region),map=map.US)+coord_cartesian()
map=map+geom_raster(data=bs,aes(x=x,y=y,fill=layer),alpha=.8)+coord_cartesian()+scale_fill_gradient(low = "darkslategray1", high = "darkslategray",guide = "colourbar")
map=map+coord_cartesian(xlim=c(-76.5,-65.5),ylim=c(34,45),expand=F)
map=map+theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
map=map+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+labs(x="lon")+labs(y="lat") + guides(fill=guide_legend(title="Bottom salinity (psu)"))
map=map+theme(legend.position=c(.9,.5),legend.justification = c(.9,.9))+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5),legend.title=element_text(size=5))+ theme(legend.key=element_blank())+theme(legend.key.size = unit(.5,'lines'))
bs_m=map
bs_m

map=ggplot()+geom_map(data=map.US,aes(map_id=region),map=map.US)+coord_cartesian()
map=map+geom_raster(data=SS,aes(x=x,y=y,fill=layer),alpha=.8)+coord_cartesian()+scale_fill_gradient(low = "darkslategray1", high = "darkslategray",guide = "colourbar")
map=map+coord_cartesian(xlim=c(-76.5,-65.5),ylim=c(34,45),expand=F)
map=map+theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
map=map+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+labs(x="lon")+labs(y="lat") + guides(fill=guide_legend(title="Surface salinity (psu)"))
map=map+theme(legend.position=c(.9,.5),legend.justification = c(.9,.9))+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5),legend.title=element_text(size=5))+ theme(legend.key=element_blank())+theme(legend.key.size = unit(.5,'lines'))
ss_m=map
ss_m

map=ggplot()+geom_map(data=map.US,aes(map_id=region),map=map.US)+coord_cartesian()
map=map+geom_raster(data=sh,aes(x=x,y=y,fill=layer),alpha=.8)+coord_cartesian()+scale_fill_gradient(low = "white", high = "darkred",guide = "colourbar")
map=map+coord_cartesian(xlim=c(-76.5,-65.5),ylim=c(34,45),expand=F)
map=map+theme(axis.text = element_text(size=5),axis.title = element_text(size=5),plot.title = element_text(size=5))
map=map+theme(panel.background = element_blank())+ theme(panel.border = element_rect(colour = "black",fill=NA))+labs(x="lon")+labs(y="lat") + guides(fill=guide_legend(title="Sea surface height (m)"))
map=map+theme(legend.position=c(.9,.5),legend.justification = c(.9,.9))+theme(legend.background = element_blank())+theme(legend.text=element_text(size=5),legend.title=element_text(size=5))+ theme(legend.key=element_blank())+theme(legend.key.size = unit(.5,'lines'))
sh_m=map
sh_m

png("/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/figs/deltas.png",width=5, height=7, units="in", res=400)
par(ps=10)
par(mar=c(4,4,1,1))
par(cex=1)
plot_grid(st_m,bt_m,ss_m,bs_m,sh_m,ncol = 2,nrow = 3)
dev.off()
