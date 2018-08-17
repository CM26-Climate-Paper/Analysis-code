## averaging seasonal contemporary rasters into annual
# covariates were done by hand

spp_dir="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/Species_Projections_all"
a=list.files(spp_dir)
b=a[1]
c=list.files(paste0(spp_dir,"/",b),full.names = T) %>% grep("_2010",.,value=T)
d=stack(c) %>% mean()


proj_full="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/species_projections_full" #;dir.create(proj_full)
proj_partial="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/species_projections_partial" #;dir.create(proj_partial)
out_full="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/species_projections_full" #;dir.create(proj_full)
out_partial="/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs/species_projections_partial" #;dir.create(proj_partial)

a=list.files(proj_full)
for(i in 1:length(a)){
b=a[i]
print(b)
c=list.files(paste0(proj_full,"/",b),full.names = T) %>% grep("_contemp_",.,value=T)
d=stack(c) %>% mean()
writeRaster(d,paste0(out_full,"/",b,"/",b,"_contemp.tif"),overwrite=T)
}

a=list.files(proj_partial)
for(i in 1:length(a)){
  b=a[i]
  print(b)
  c=list.files(paste0(proj_partial,"/",b),full.names = T) %>% grep("_contemp_",.,value=T)
  d=stack(c) %>% mean()
  writeRaster(d,paste0(out_partial,"/",b,"/",b,"_contemp.tif"),overwrite=T)
}
