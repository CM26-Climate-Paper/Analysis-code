#
# This is a Shiny web application. You can run the application by clicking

###### 1. load libraries #####
library(tidyverse)
library(shiny)
library(rsconnect)
library(raster)
library(leaflet)
library(shinythemes)
library(RColorBrewer)

###### 2. define global objects #####
contemp_vars_dir="data/contemporary/"
project_vars_dir="data/project/"
full_models_dir="data/species_projections_full/"
partial_models_dir="data/species_projections_partial/"
species=read.csv("data/List_of_Species_with_Records_Above_m07.csv")
commonNames=species$name
timeperiods=c("Contemporary","+20-40 years","+40-60 years","+60-80 years")
seasons=c("Winter","Spring","Summer","Fall")
env_vars=c("Surface temperature","Bottom temperature","Surface salinity","Bottom salinity","Sea height")


###### 3. UI #####

ui <- shinyUI(fluidPage(theme = shinytheme("superhero"),
                        #tags$style(type = "text/css", "html, body {width:1000px;height:750px;}"), 
                        fluidRow(
                          column(12, titlePanel(tags$div(h1(style="text-align:center;","Going beyond temperature: Forecasting climate-driven range shifts within the U.S. Northeast Continental Shelf")))),
                          column(12, titlePanel(tags$div(h3("Jennifer McHenry 1*, Heather Welch 2,3*^, Vince Saba 4")))),
                          column(6, selectInput("common",tags$div(h4(style="padding:8px;","Select species")),commonNames,width = 400))),
                        fluidRow(
                          column(4,titlePanel(tags$div(h4(style="text-align:center;","Full model"))),
                                 leafletOutput("full")),
                                  absolutePanel(top = 735, left = 14, height=100, width=551, fixed=TRUE,
                                       style = "text-align:center;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: black; ",
                                       div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("full_timeSelect", tags$div(tags$b(h4(style="color:white","Time Period"))), timeperiods)),
                                       div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("full_seasonSelect", tags$div(tags$b(h4(style="color:white","Season"))), seasons))),
                                       
                          column(4,titlePanel(tags$div(h4(style="text-align:center;","Partial model"))),
                                 leafletOutput("partial")),
                                  absolutePanel(top = 735, left = 594, height=100, width=551, fixed=TRUE,
                                        style = "text-align:center;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: black;",
                                        div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("part_timeSelect", tags$div(tags$b(h4(style="color:white","Time Period"))), timeperiods,width = 200)),
                                        div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("part_seasonSelect", tags$div(tags$b(h4(style="color:white","Season"))), seasons,width = 200))),
                          column(4,titlePanel(tags$div(h4(style="text-align:center;","Hydrological predictor variables"))),
                                 leafletOutput("variables")),
                                  absolutePanel(top = 735, left = 1173, height=100, width=551, fixed=TRUE,
                                        style = "text-align:center;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: black;",
                                        div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("env_vars_timeSelect", tags$div(tags$b(h4(style="color:white","Time Period"))), timeperiods,width = 200)),
                                        div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("env_vars_seasonSelect", tags$div(tags$b(h4(style="color:white","Season"))), seasons,width = 200)),
                                        div(style="text-align:center;display: inline-block;vertical-align:top; width: 180px;",selectInput("env_varsSelect", tags$div(tags$b(h4(style="color:white","Variables"))), env_vars,width = 200))))
                        
                        ##### ----> bunch of junk, figure out a better way to do this ####
                        # fluidRow(
                        #   column(12, titlePanel(tags$div(h6(style="padding:0px;",""))))),
                        # fluidRow(
                        #   column(12, titlePanel(tags$div(h6(style="padding:0px;",""))))),
                        # fluidRow(
                        #   column(12, titlePanel(tags$div(h6(style="padding:0px;",""))))),
                        # fluidRow(
                        #   column(12, titlePanel(tags$div(h6(style="padding:0px;",""))))),
                        # fluidRow(tags$style("padding:0px;height:0px;"),
                        #   column(12, titlePanel(tags$div(h6(style="padding:0px;height:0px;","1 - Department of Geography, Florida State University, Tallahassee, FL 32306;  
                        #                                     2 - Environmental Research Division, SWFSC, NMFS, NOAA, Monterey, CA 93940;  
                        #                                     3 - Institute of Marine Science, University of California Santa Cruz, Santa Cruz, CA 95064;  
                        #                                     4 - Geoplysical Fluid Dynamics Laboratory, Princeton University, Princeton, NJ 08540")))),
                        #   column(12, titlePanel(tags$div(h6(style="padding:0px;height:0px;","* - These authors contributed equally")))),
                        #   column(12, titlePanel(tags$div(h6(style="padding:0px;height:0px;","^ - Corresponding author. email: heather.welch@noaa.gov")))),
                        #   column(12, titlePanel(tags$div(h6(style="padding:0px;height:0px;","Shiny application created by HW.")))))
                        ##### ----> end bunch of junk ####
                  
                        
))
                                 
                      

server <- shinyServer(function(input, output) {
  
  ##### ----> collect reactive elements ####
  sp=reactive({species[species$name==input$common,]%>%.[1,5]})
  
  ##### ----> Spectral ####
  pal <- brewer.pal(11,"RdYlBu")
  vals <- NULL
  vals=c(0,1)
  palette2 <- colorNumeric(pal, vals,
                           na.color = "transparent") 
  
  output$full <- renderLeaflet({
    #### get time period ####
    if(input$full_timeSelect=="Contemporary"){
      full_time="_contemp_"
    }
    if(input$full_timeSelect=="+20-40 years"){
      full_time="_av20_40_"
    }
    if(input$full_timeSelect=="+40-60 years"){
      full_time="_av40_60_"
    }
    if(input$full_timeSelect=="+60-80 years"){
      full_time="_av60_80_"
    }
    #####
    
    #### get season ####
    if(input$full_seasonSelect=="Winter"){
      full_season="DJF"
    }
    if(input$full_seasonSelect=="Spring"){
      full_season="MAM"
    }
    if(input$full_seasonSelect=="Summer"){
      full_season="JJA"
    }
    if(input$full_seasonSelect=="Fall"){
      full_season="SON"
    }
    #####
    
    full=raster(paste0("data/species_projections_full/",sp(),"/",sp(),full_time,full_season,".tif"))
  lmap <- leaflet()
  lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
  lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
  lmap <- addLegend(lmap, "bottomright", values = vals,color=pal, labels=c("0: Highly unsuitable","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1: Highly suitable"),title = "Species habitat suitability") 
  })
  
  output$partial <- renderLeaflet({
    #### get time period ####
    if(input$part_timeSelect=="Contemporary"){
      part_time="_contemp_"
    }
    if(input$part_timeSelect=="+20-40 years"){
      part_time="_av20_40_"
    }
    if(input$part_timeSelect=="+40-60 years"){
      part_time="_av40_60_"
    }
    if(input$part_timeSelect=="+60-80 years"){
      part_time="_av60_80_"
    }
    #####
    
    #### get season ####
    if(input$part_seasonSelect=="Winter"){
      part_season="DJF"
    }
    if(input$part_seasonSelect=="Spring"){
      part_season="MAM"
    }
    if(input$part_seasonSelect=="Summer"){
      part_season="JJA"
    }
    if(input$part_seasonSelect=="Fall"){
      part_season="SON"
    }
    #####
    
    partial=raster(paste0("data/species_projections_partial/",sp(),"/",sp(),part_time,part_season,".tif"))
    lmap <- leaflet()
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(partial),colors=palette2)
  })
  
  output$variables <- renderLeaflet({
    #### get time period ####
    if(input$env_vars_timeSelect=="Contemporary"){
      env_vars_time="contemp_"
    }
    if(input$env_vars_timeSelect=="+20-40 years"){
      env_vars_time="av20_40_"
    }
    if(input$env_vars_timeSelect=="+40-60 years"){
      env_vars_time="av40_60_"
    }
    if(input$env_vars_timeSelect=="+60-80 years"){
      env_vars_time="av60_80_"
    }
    #####
    
    #### get season ####
    if(input$env_vars_seasonSelect=="Winter"){
      env_vars_season="DJF"
    }
    if(input$env_vars_seasonSelect=="Spring"){
      env_vars_season="MAM"
    }
    if(input$env_vars_seasonSelect=="Summer"){
      env_vars_season="JJA"
    }
    if(input$env_vars_seasonSelect=="Fall"){
      env_vars_season="SON"
    }
    
    #### get season ####
    if(input$env_varsSelect=="Surface temperature"){
      env_vars="st"
    }
    if(input$env_varsSelect=="Bottom temperature"){
      env_vars="bt"
    }
    if(input$env_varsSelect=="Surface salinity"){
      env_vars="SS"
    }
    if(input$env_varsSelect=="Bottom salinity"){
      env_vars="bs"
    }
    if(input$env_varsSelect=="Sea height"){
      env_vars="sh"
    }
    #####
    variable=raster(paste0("data/project/",env_vars_time,env_vars_season,"/",env_vars,".tif"))
    lmap <- leaflet()
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(variable))
  })
  
  
  
})

shinyApp(ui = ui, server = server)