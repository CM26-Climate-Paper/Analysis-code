#### some early code to plot trends from the habitat metrics

xx$name=as.character(xx$name)
xx$TemporalOrder=substrRight(xx$name, 8)
xx$TemporalOrder=gsub("m","",xx$TemporalOrder)
xx$TemporalOrder=gsub("_","-",xx$TemporalOrder)
xx$TemporalOrder=paste("01-",xx$TemporalOrder,sep="")
xx$TemporalOrder=as.Date(xx$TemporalOrder,format="%d-%m-%Y")
head(xx)
plot(xx$TemporalOrder,xx$n.patches)
par(new=TRUE)
library(ggplot2)
library(grid)
library(gridExtra)
install.packages("gridExtra")
jpeg('bp1006.jpg')
p1 = qplot(a$TemporalOrder,a$n.patches)
p2 = qplot(a$TemporalOrder,a$total.area)
p3 = qplot(a$TemporalOrder,a$patch.density)
p4 = qplot(a$TemporalOrder,a$largest.patch.index)
p5 = qplot(a$TemporalOrder,a$total.edge)
p6 = qplot(a$TemporalOrder,a$prop.landscape)
p7 = qplot(a$TemporalOrder,a$mean.perim.area.ratio)
p8 = qplot(a$TemporalOrder,a$mean.shape.index)
grid.arrange(p1+stat_smooth(), p2+stat_smooth(), p3+stat_smooth(), p4+stat_smooth(), p5+stat_smooth(), p6+stat_smooth(), p7+stat_smooth(), p8+stat_smooth(), ncol = 2, top = "bp1006 landscape metrics")
dev.off()

##https://anomaly.io/seasonal-trend-decomposition-in-r/
install.packages("fpp")
library(fpp)
plot(as.ts(a$total.area))
install.packages("forecast")
library(forecast)

##### addative
trend = ma(a$total.area, order = 4, centre = T)
plot(as.ts(a$total.area))
lines(trend)
plot(as.ts(trend),main="Quarterly")
detrend = a$total.area - trend
plot(as.ts(detrend))

##### multiplicative
trend = ma(a$total.area, order = 12, centre = T) ## 12 for monthly
plot(as.ts(a$total.area))
lines(trend)
plot(as.ts(trend),main="Annual")
detrend = a$total.area - trend
plot(as.ts(detrend))

trend2=ma(detrend,order=12,centre=T)
lines(trend2)
plot(as.ts(trend))

trend = ma(a$total.area, order = 120, centre = T) ## 12 for monthly
plot(as.ts(a$total.area))
lines(trend)
plot(as.ts(trend),main="Decadal")
detrend = a$total.area - trend
plot(as.ts(detrend))

trend2=ma(detrend,order=12,centre=T)
lines(trend2)
plot(as.ts(trend))

### picking this back up again, based on: http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/Habitat_Metrics")

library("TTR")

xx=read.csv("bp1006_HabitatMetrics.csv")
plot(xx$total.area)
a=ts(data=xx$total.area,start=c(1,1))
plot.ts(a)

smoothed=SMA(a,n=60)
plot.ts(smoothed)

a=ts(data=xx$total.area,start=c(1,1),frequency = 12)
b=decompose(a)
adjusted=a-b$seasonal
plot(adjusted)

eruption.lm = lm(TemporalOrder ~ n.patches, data=xx) 
summary(eruption.lm)$r.squared 
slope=coef(eruption.lm)
as.numeric(slope[2])

#### getting r2 values to use in a grouping analysis
empty=xx[0,]
empty=empty[,c(1,5:44)]
dataframes=list.files()
for(i in 1:length(dataframes)){
  csv=read.csv(dataframes[i])
  trimmed=csv[,c(5:44)]
  empty[i,1]=as.character(dataframes[i])
  for(ii in 1:ncol(trimmed)){
    model=lm(trimmed[[ii]] ~ TemporalOrder,data=trimmed)
    slope=coef(model)
    empty[i,ii+1]=as.numeric(slope[2])
  }
}

fit=princomp(empty[,c(3,6,9,10,12,27,39,40)],cor=FALSE)
summary(fit)
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit) 

library(FactoMineR)
result <- PCA(empty[,c(3,6,9,10,12,27,39,40)]) # graphs generated automatically 

library(ggplot2)
spcluster=kmeans(empty[,c(3,6,9,10,12,27,39,40)],10,nstart = 20.)

d <- dist(as.matrix(empty[,c(3,6,9,10,12,27,39,40)]))   # find distance matrix 
hc <- hclust(d)                # apply hirarchical clustering 
plot(hc)  

####trend decomposition
library(stlplus)
trendseries=stlplus(a,n.p = 12,s.window = "periodic")

library(pdc)
## first 2 dimensional (later 3D array) "total.area"
ta=as.data.frame(matrix(vector(), 972, 147))
#ta[,149]=xx$TemporalOrder

dataframes=list.files()
dataframes=dataframes[dataframes != "m1068_HabitatMetrics.csv"]

for(i in 1:ncol(ta)){
  csv=read.csv(dataframes[i])
  ta[,i]=csv$total.area
  colnames(ta)[i] <- as.character(dataframes[i])
}

#ta_complete=ta[complete.cases(ta),]
#ta_real=ta[,1:(ncol(ta)-1)]
pdc_test=pdclust(ta)

lables=colnames(ta)
lables=lapply(lables,function(x)gsub("_HabitatMetrics.csv","",x))
lables=unlist(lables)
plot(pdc_test,labels = lables)
plot(pdc_test,p.values = TRUE,cols=c(rep("red",10),rep("blue",99),rep("chartreuse1",12),rep("black",2),rep("cyan1",4),rep("darkgoldenrod1",17)))

mdsPlot(pdc_test,labels=lables,col="gray")
pairs(xx)

### build 3d array
empty3D=array(NA,dim=c(972,147,9))
dataframes=list.files()

dataframes=dataframes[dataframes != "m1068_HabitatMetrics.csv"]

for(i in 1:ncol(empty3D)){
  csv=read.csv(dataframes[i])
  empty3D[,i,1]=csv$n.patches
  empty3D[,i,2]=csv$total.area
  empty3D[,i,3]=csv$patch.density
  empty3D[,i,4]=csv$total.edge
  empty3D[,i,5]=csv$sd.patch.area
  empty3D[,i,6]=csv$mean.patch.core.area
  empty3D[,i,7]=csv$sd.patch.core.area
  empty3D[,i,8]=csv$patch.cohesion.index
  empty3D[,i,9]=csv$Centroid_Latitude
}

## name the array columns
colnames(empty3D)<- dataframes
dimnames(empty3D)[[3]] <- c("n.patches","total.area","patch.density","total.edge","sd.patch.area","mean.patch.core.area","sd.patch.core.area","patch.cohesion.index","Centroid_Latitude")

##cluster the time-series
pdc_test=pdclust(empty3D)
lables=dataframes
lables=lapply(lables,function(x)gsub("_HabitatMetrics.csv","",x))
lables=unlist(lables)
plot(pdc_test,labels = lables)
plot(pdc_test)
plot(pdc_test,p.values = TRUE,cols=c(rep("red",10),rep("blue",100),rep("chartreuse1",13),rep("black",2),rep("cyan1",4),rep("darkgoldenrod1",18)))

cb=codebook(as.numeric(csv[,1]))
barplot(cb)

## testing clustering by variable
## MAKING SOME OUTPUT PLOTS
dimnames(empty3D)[[3]]

setwd("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/time_series_clusters")

pdc_test=pdclust(empty3D[,,9]) 
plot(pdc_test,p.values = TRUE,cols=c(rep("red",10),rep("blue",100),rep("chartreuse1",13),rep("black",2),rep("cyan1",4),rep("darkgoldenrod1",18)),
     main=dimnames(empty3D)[[3]][9])
plot(pdc_test,labels=lables)

##idea, stick w 2d, find correct number of clusters for each variable, then unique combinations later
library(fpc)
#http://www.statmethods.net/advstats/cluster.html

clusterDF=as.data.frame(matrix(vector(), 147,9))
species=names(classification)
species=lapply(species,function(x)gsub("clustering.","",x))
species=unlist(lapply(species,function(x)gsub("_HabitatMetrics.csv","",x)))

row.names(clusterDF)=species
colss=as.list(dimnames(empty3D)[[3]])
colnames(clusterDF)=colss

for(i in 1:9){
variable=t(empty3D[,,i]) ## transpose, need species to be observations
variable_clus=pamk(variable)
classification=unlist(variable_clus[[1]][3])
clusterDF[,i]=unname(variable_clus[[1]][3])
}


### look at clusterDF
result <- PCA(clusterDF)

fit=princomp(clusterDF,cor=FALSE)
summary(fit)
loadings(fit) # pc loadings
plot(fit,type="lines") # scree plot
fit$scores # the principal components
biplot(fit) 


#cluster the clusterDF
all_vars=pamk(clusterDF)
plot(clusterDF, col = all_vars$pamobject$clustering)

library(cluster)
r=pam(clusterDF,10)
clusNUM=mean(silhouette(r)[, "sil_width"])
r=pam(clusterDF,5)

clusplot(pam(clusterDF,5))

## silloutte plot: http://scikit-learn.org/stable/auto_examples/cluster/plot_kmeans_silhouette_analysis.html
plot(pam(clusterDF, 5))



or(i in 1:length(dataframes)){
  csv=read.csv(dataframes[i])
  trimmed=csv[,c(5:44)]
  empty2[i,1]=as.character(dataframes[i])
  for(ii in 1:ncol(trimmed)){
    trendseries=stlplus(trimmed[[ii]],n.p=12,s.window = "periodic")
    trimmed$seasonal=trendseries$data$seasonal
    model=lm(seasonal ~ TemporalOrder,

