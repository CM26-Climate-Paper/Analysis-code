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

