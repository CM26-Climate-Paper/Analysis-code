### Comparison w Kleisner paper
#1 average species into summer-fall
# make plots

library(raster)
library(maps)

make_png=function(r,year,species,season){ ### does what it says
  if(year=="2021"){
    year2="Year 1"
    png(paste0("/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/keister_comparison/",species,"_",year2,"_",season,".png"), width=7, height=5, units="in", res=400)
  }
  
  if(year=="2041"){
    year2="Year 20"
    png(paste0("/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/keister_comparison/",species,"_",year2,"_",season,".png"), width=7, height=5, units="in", res=400)
  }
  
  if(year=="2061"){
    year2="Year 40"
    png(paste0("/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/keister_comparison/",species,"_",year2,"_",season,".png"), width=7, height=5, units="in", res=400)
  }
  
  if(year=="2000"){
    year2="Year 80"
    png(paste0("/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/keister_comparison/",species,"_",year2,"_",season,".png"), width=7, height=5, units="in", res=400)
  }
  
  par(ps=10) #settings before layout
  layout(matrix(c(1,2), nrow=2, ncol=1, byrow=TRUE), heights=c(4,1), widths=7)
  #layout.show(2) # run to see layout; comment out to prevent plotting during .pdf
  par(cex=1) # layout has the tendency change par()$cex, so this step is important for control
  
  par(mar=c(4,4,1,1)) # I usually set my margins before each plot
  #pal <- colorRampPalette(c("blue", "grey", "red"))
  pal <- colorRampPalette(c("darkblue","blue", "cyan", "yellow", "red","dark red"))
  #pal <- colorRampPalette(c("purple4", "white", "blue"))
  ncolors <- 200
  breaks <- seq(0,1,,ncolors+1)
  image(r, col=pal(ncolors), breaks=breaks)
  map("world", add=TRUE, lwd=2)
  #contour(r, add=TRUE, col="black",levels=c(-.75,-.5,.5,.75))
  box()
  
  par(mar=c(4,4,0,1)) # I usually set my margins before each plot
  levs <- breaks[-1] - diff(breaks)/2
  image(x=levs, y=1, z=as.matrix(levs), col=pal(ncolors), breaks=breaks, ylab="", xlab="", yaxt="n")
  
  if(species=="d193"){
    common_name="ocean pout"
  }
  
  if(species=="d171"){
    common_name="northern sea robin"
  }
  
  if(species=="d24"){
    common_name="clearnose skate"
  }
  
  if(species=="bp15"){
    common_name="spiny dogfish"
  }
  
  if(species=="d103"){
    common_name="summer flounder"
  }
  
  if(species=="d106"){
    common_name="winter flounder"
  }
  
  if(species=="d73"){
    common_name="atlantic cod"
  }
  
  if(species=="d74"){
    common_name="haddock"
  }
  
  if(species=="e301"){
    common_name="american lobster"
  }
  
  if(species=="bp502"){
    common_name="longfin squid"
  }
  
  if(species=="d141"){
    common_name="black sea bass"
  }
  
  if(species=="d143"){
    common_name="scup"
  }
  
  if(species=="p135"){
    common_name="bluefish"
  }
  
  if(species=="bp131"){
    common_name="butterfish"
  }
  
  
  mtext(paste0("Habitat suitability for ",common_name," ",season," ",year2), side=1, line=2.5)
  
  box()
  
  dev.off() # closes device
}


species=c("d193","d171","d24","bp15","d103","d106","d73","d74","e301",'bp502','d141',"d143","p135","bp131")
speciesdir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/Species_Projections_all/"
years=c("2041","2061","2000") #+20,+40,+80

for(sp in species){
  path=paste0(speciesdir,sp,"/")
  for(year in years){
    feb=paste0(path,sp,"_m02_",year,".tif")
    mar=paste0(path,sp,"_m03_",year,".tif")
    apr=paste0(path,sp,"_m04_",year,".tif")
    spring=list(feb,mar,apr)
    springR=stack(spring)
    springRmean=calc(springR,fun = mean)
    make_png(r=springRmean,year=year,species = sp,season = "spring")
    
    sep=paste0(path,sp,"_m09_",year,".tif")
    oct=paste0(path,sp,"_m10_",year,".tif")
    nov=paste0(path,sp,"_m11_",year,".tif")
    fall=list(sep,oct,nov)
    fallR=stack(fall)
    fallRmean=calc(fallR,fun = mean)
    make_png(r=fallRmean,year=year,species = sp,season = "fall")
  }
  
}
