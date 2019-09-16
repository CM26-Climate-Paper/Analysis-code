##### Defining global objects####

###### 1. load libraries #####
library(tidyverse)
library(shiny)
library(rsconnect)
library(raster)
library(leaflet)
library(shinythemes)
library(RColorBrewer)
library(grDevices)
library(rgdal)
library(shinydashboard)
library(plotly)
library(fmsb)


###### 2. define global objects #####

### cleaning up variable importance for app (afterwards will read in csv) ####
# variable_importance=read.csv("data/Supplementary_Tables_S3_S4.csv")%>% mutate(name=tolower(paste(Common.Name,"-",Scientific.Name)))
# b=variable_importance  %>% dplyr::select(-c(Species.Groups,Common.Name,Scientific.Name,Full..Mean.,Full..SD.,Partial..Mean.,Partial..SD.,name))
# c=apply(b,2,function(x)as.numeric(gsub("%","",x))) %>% as.data.frame() %>% mutate(name=variable_importance$name)
# write.csv(c,"data/parallel_coordinates.csv")
#####

### finding common species between rasters, list of species with records above m07, and species in variable importance csv####
# species=read.csv("data/List_of_Species_with_Records_Above_m07.csv") %>% 
#   commonNames=species$name %>% as.character()
# variable_importance=read.csv("data/parallel_coordinates.csv") ### from above
# maps=list.files("/Volumes/SDM /Lacie backup October 2016/Lacie share/Climate_paper/GAM_1/project_20y_avs_seasonal/Analysis-code/Scripts/beyond_temperature/data/species_projections_full")
# 
# maps2=Reduce(intersect,list(species$Species,maps)) %>% as.data.frame() # species common between rasters and species records m07
# colnames(maps2)="Species"
# test=left_join(maps2,species)
# new=Reduce(intersect,list(test$name,variable_importance$name)) %>% as.data.frame() # species common between above and variable importance
# colnames(new)="name"
# new2=left_join(new,species) ### final species list
# write.csv(new2,"data/species_list_final.csv")
#####
species=read.csv("data/species_list_final.csv") 
commonNames=species$name %>% as.character()
variable_importance=read.csv("data/parallel_coordinates.csv")
d=variable_importance %>% gather(variable,value,-c(name,X))%>% mutate(value=as.numeric(value))
d$variable=factor(d$variable,levels=c("Bottom.Salinity","Surface.Salinity","Bottom.Temper.ature","Surface.Temper.ature","Rugosity","Depth","Sea.Surface.Height"))
#d$id=as.factor(d$species)
dd=with(d, d[order(variable),])
dd=dd %>% mutate(color="grey")


ui <- dashboardPage(skin="black",
                    dashboardHeader(disable=T
                      # title = tags$div(style="color:white","Projecting  marine  species  range  shifts  from  only  temperature  can  mask  climate  vulnerability"),
                      # titleWidth = "100%"
                    ),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(id = 'sidebarmenu',
                      selectInput("species","Select species",commonNames,width = "100%")


                      )),
                    
   dashboardBody(div(style="padding-top: 0px;border-radius: 0px;margin=-2em",
     fluidRow(width = 12,offset=0,
       div(style="padding:0px;height:15px",
              tags$div(style="text-align:center;",h4(tags$b("Projecting marine species range shifts from only temperature masks climate vulnerability"))))
     ),
     fluidRow(width = 12,offset=0,
                  tags$div(style="text-align:center;",h5(HTML(paste("Jennifer McHenry",tags$sup(1,"*"),", ","Heather Welch",tags$sup(3,4),", ","Sarah E. Lester",tags$sup(1),", ","Vincent S. Saba",tags$sup(2)))))
     ),
     fluidRow(width = 12,offset=0,
             column(h5("Full model"),width = 4,leafletOutput("full")),
             column(h5("Partial model"),width = 4,leafletOutput("partial")),
             column(h5("Hydrological covariates"),width = 4,leafletOutput("covariate")),
     fluidRow(width=12,offset=0,
             column(h5("Full model relative variable importance"),width = 8,plotlyOutput("test")),
             column(h5("Full model relative variable importance"),width = 4,plotOutput("test2")),
             column(h6(style="padding:0px;height:0px;","* - Corresponding author"),width = 12)),
             column(h6(style="padding:0px;height:0px;","1 - Department of Geography, Florida State University, Tallahassee, FL 32306"),width = 12),
             column(h6(style="padding:0px;height:0px;","2 - NOAA, NMFS, Northeast Fisheries Science Center, Geophysical Fluid Dynamics Laboratory, Princeton University Forrestal Campus, Princeton, NJ, 08540, USA"),width = 12),
             column(h6(style="padding:0px;height:0px;","3 - NOAA, NMFS, Southwest Fisheries Science Center, Monterey, CA, 93940, USA"),width = 12),
             column(h6(style="padding:0px;height:0px;","4 - Institute of Marine Sciences, University of California, Santa Cruz, CA, 95064, USA"),width = 12)
))
   ))


server <- shinyServer(function(input, output) {
  
  ##### ----> collect reactive elements ####
  speciesID=reactive({species[species$name==input$species,]%>%.[1,5]%>% as.character()})
  
  options(shiny.sanitize.errors = F)
  
  output$test=renderPlotly({
    Sys.setlocale(locale="C")
    dd$id=dd$name
   # pc=ggplot(dd, aes(x = variable, y = value, group = id)) +   # group = id is important!
      pc=ggplot()+geom_path(data=dd, aes(x = variable, y = value, group = id,color=name)) +theme(legend.position = "none")+scale_color_manual(values=setNames(dd$color,dd$id))
    # scale_size(breaks = NULL, range = c(0, 5))
    ee=dd %>% filter(name==input$species)
    ee=with(ee, ee[order(variable),])
    e=pc+geom_path(data=ee,aes(x = variable, y = value, group = id),color="red")+theme_bw()+theme(legend.position = "none")+ggtitle(input$species)+theme(axis.text.x = element_text(angle=30))
    e
  
     ggplotly(e,tooltip = c("y","colour"))

  })

  output$test2=renderPlot({
    a=variable_importance  %>% filter(name==input$species)%>% select(-c(X,name))
    maxmin=a
    maxmin[2,]=0
    maxmin[1,]=round(max(a),0)
    b=rbind(maxmin,a)
    
    
    
    a
    par(xpd = TRUE, mfrow = c(1, 1), mar = c(1, 1, 1, 1))
    radarchart(b,axistype=1,seg = 3, cglty = 1, cglwd=0.8,title=input$species,
               pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
               pty = 32, cglcol = "grey", axislabcol = "grey",vlcex=0.8 ,
               caxislabels = round(seq(from = min(a), to = max(a), length = 4),1))
  })
  
  ####

  

  

  
})


shinyApp(ui = ui, server = server)