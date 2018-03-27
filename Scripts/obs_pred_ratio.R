### script to calculate obs::pred ratio

library(mgcv)
setwd("/Volumes/SeaGate/ClimatePaperCM2.6/species_models")
a=readRDS("p113.rds")
a$fitted.values %>% mean()
a$prior.weights %>% mean()
b=read.csv("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/species/p925.csv")

c=predict(a,type="response")
obspred=nrow(c)/sum(c)


partial_model_list=list.files("/Volumes/SeaGate/ClimatePaperCM2.6/species_models_partial",pattern = ".rds",full.names = T)
partial_sp=list.files("/Volumes/SeaGate/ClimatePaperCM2.6/species_models_partial",pattern = ".rds") %>% gsub(".rds","",.)
full_model_list=list.files("/Volumes/SeaGate/ClimatePaperCM2.6/species_models",pattern = ".rds",full.names = T)
full_sp=list.files("/Volumes/SeaGate/ClimatePaperCM2.6/species_models",pattern = ".rds") %>% gsub(".rds","",.)

master=data.frame(Species=NA,Full=NA,Patial=NA)
for(i in 1:length(partial_sp)){
#for(i in 1:10){
  print(partial_sp[i])
  full=readRDS(partial_model_list[i])
  part=readRDS(full_model_list[i])
  full_r=predict(full,type="response") %>% as.numeric()
  part_r=predict(part,type="response")
  # full_ratio=sum(full$y)/sum(full_r)
  # part_ratio=sum(part$y)/sum(part_r)
  
  # full_ratio=full$y/full_r 
  # full_ratio=full_ratio[1:sum(full$y)] %>% mean()
  
  full_ratio=sum(full$y)/sum(full_r[1:sum(full$y)])
  
  # part_ratio=part$y/part_r 
  # part_ratio=part_ratio[1:sum(part$y)] %>% mean()
  
  part_ratio=sum(part$y)/sum(part_r[1:sum(part$y)])
  
  master[i,1]=partial_sp[i]
  master[i,2]=full_ratio
  master[i,3]=part_ratio
}


master
write.csv(master,"/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/figs/pred_obs_ratio2.csv")

sp_names=read.csv("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/species_data/Final_Species_Records_7months_andAbove/List_of_Species_with_Records_Above_m07.csv")
master2=left_join(master,sp_names) %>% dplyr::select(Species,Full, Patial, Common.Name, Scientific.Name) %>% rename(Partial=Patial)
#write.csv(master2,"/Volumes/SeaGate/ClimatePaperCM2.6/WorkingNotes/figs/pred_obs_ratio_join2.csv")
master2=master2 %>% filter(Full<40) %>% filter(Partial<40)
master2=master2 %>% gather(Model_type,ratio,-c(Species,Common.Name,Scientific.Name))

bplot=ggplot(data=master2,aes(x=Model_type,y=ratio))+geom_boxplot()
bplot
