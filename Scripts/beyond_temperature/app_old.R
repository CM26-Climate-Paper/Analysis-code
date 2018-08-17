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
library(grDevices)
library(rgdal)

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
                          column(12, absolutePanel(top = 15, left = 14, height=120, width=1714, fixed=TRUE,
                                  style = "text-align:center;border-radius: 3px; padding: 0px; opacity: 0.92;border: 1px solid; background: black; ",
                                  titlePanel(tags$div(h1(style="text-align:center;","Going beyond temperature: Forecasting climate-driven range shifts within the U.S. Northeast Continental Shelf"))))),
                          column(12,absolutePanel(top = 125, left = 14, height=50, width=1714, fixed=TRUE,
                                 style = "text-align:center;border-radius: 3px; padding: 0px; opacity: 0.92;", 
                                 titlePanel(tags$div(h4(HTML(paste("Jennifer McHenry",tags$sup(1,"*"),", ","Heather Welch, ",tags$sup(2,3,"*","°"),", ","Vincent S. Saba",tags$sup(4)))))))),
                          column(6, absolutePanel(top = 169, left = 15, height=141, width=399, fixed=TRUE,
                                    style = "text-align:center;border-radius: 0px; padding: 0px; opacity: 0.92;border: 1px solid; background: black; ", 
                                 selectInput("common",tags$div(h4(tags$b(style="padding:0px;","Select species"))),commonNames,width = 400,selectize = F,size=4)))),
                        fluidRow(
                          column(4,absolutePanel(top = 315, left = 14, height=800, width=551, fixed=TRUE,
                                   style = "text-align:center;border-radius: 3px; padding: 0px; opacity: 0.92;", 
                                 titlePanel(tags$div(h4(style="text-align:center;","Full model"))),
                                 leafletOutput("full"))),
                                  absolutePanel(top = 735, left = 14, height=100, width=551, fixed=TRUE,
                                       style = "text-align:center;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: black; ",
                                       div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("full_timeSelect", tags$div(tags$b(h4(style="color:white","Time Period"))), timeperiods)),
                                       div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("full_seasonSelect", tags$div(tags$b(h4(style="color:white","Season"))), seasons))),
                                       
                          column(4,absolutePanel(top = 315, left = 594, height=800, width=551, fixed=TRUE,
                                   style = "text-align:center;border-radius: 3px; padding: 0px; opacity: 0.92;",
                                 titlePanel(tags$div(h4(style="text-align:center;","Partial model"))),
                                 leafletOutput("partial"))),
                                  absolutePanel(top = 735, left = 594, height=100, width=551, fixed=TRUE,
                                        style = "text-align:center;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: black;",
                                        div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("part_timeSelect", tags$div(tags$b(h4(style="color:white","Time Period"))), timeperiods,width = 200)),
                                        div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("part_seasonSelect", tags$div(tags$b(h4(style="color:white","Season"))), seasons,width = 200))),
                          column(4,absolutePanel(top = 315, left = 1173, height=800, width=551, fixed=TRUE,
                                  style = "text-align:center;border-radius: 3px; padding: 0px; opacity: 0.92;",
                                 titlePanel(tags$div(h4(style="text-align:center;","Hydrological predictor variables"))),
                                 leafletOutput("variables"))),
                                  absolutePanel(top = 735, left = 1173, height=100, width=551, fixed=TRUE,
                                        style = "text-align:center;border-radius: 3px; padding: 8px; opacity: 0.92;border: 1px solid; background: black;",
                                        div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("env_vars_timeSelect", tags$div(tags$b(h4(style="color:white","Time Period"))), timeperiods,width = 200)),
                                        div(style="text-align:center;display: inline-block;vertical-align:top; width: 150px;",selectInput("env_vars_seasonSelect", tags$div(tags$b(h4(style="color:white","Season"))), seasons,width = 200)),
                                        div(style="text-align:center;display: inline-block;vertical-align:top; width: 180px;",selectInput("env_varsSelect", tags$div(tags$b(h4(style="color:white","Variables"))), env_vars,width = 200)))),
                        
                        fluidRow(
                          column(12,absolutePanel(top = 930, left = 14, height=800, width=551, fixed=TRUE,
                                  style = "border-radius: 0px; padding: 0px; opacity: 0.92;",
                                  titlePanel(tags$div(h6(style="padding:0px;height:0px;","1 - Department of Geography, Florida State University, Tallahassee, FL 32306"))),
                                  titlePanel(tags$div(h6(style="padding:0px;height:0px;","2 - Environmental Research Division, SWFSC, NMFS, NOAA, Monterey, CA 93940"))),
                                  titlePanel(tags$div(h6(style="padding:0px;height:0px;","3 - Institute of Marine Science, University of California Santa Cruz, Santa Cruz, CA 95064"))),
                                  titlePanel(tags$div(h6(style="padding:0px;height:0px;","4 - Geoplysical Fluid Dynamics Laboratory, Princeton University, Princeton, NJ 08540"))),
                                  titlePanel(tags$div(h6(style="padding:0px;height:0px;","* - These authors contributed equally"))),
                                  titlePanel(tags$div(h6(style="padding:0px;height:0px;","^ - Corresponding author. email: heather.welch@noaa.gov"))),
                                  titlePanel(tags$div(h6(style="padding:0px;height:0px;","Shiny application created by HW"))))
                                 ))
                        
                        
))
                                 
                      

server <- shinyServer(function(input, output) {
  
  ##### ----> collect reactive elements ####
  sp=reactive({species[species$name==input$common,]%>%.[1,5]})
  
  options(shiny.sanitize.errors = F)
  
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
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,1)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent") 
    
  full=raster(paste0("data/species_projections_full/",sp(),"/",sp(),full_time,full_season,".tif"))
  lmap <- leaflet()
  lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
  lmap <-addRasterImage(lmap, projectRasterForLeaflet(full),colors=palette2)
  #lmap <- addLegend(lmap, "bottomright", values = vals,color=pal, labels=c("0: Highly unsuitable","0.1","0.2","0.3","0.4","0.5","0.6","0.7","0.8","0.9","1: Highly suitable"),title = "Species habitat suitability") 
  lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Species habitat suitability")
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
    
    pal <- brewer.pal(11,"RdYlBu")
    vals <- NULL
    vals=c(0,1)
    palette2 <- colorNumeric(pal, vals,
                             na.color = "transparent") 
    
    partial=raster(paste0("data/species_projections_partial/",sp(),"/",sp(),part_time,part_season,".tif"))
    lmap <- leaflet()
    lmap <- addProviderTiles(lmap, "Esri.NatGeoWorldMap",options = providerTileOptions(noWrap = TRUE))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(partial),colors=palette2)
    lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Species habitat suitability")
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
    
    #### get variable ####
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
    
    # ### adding legend
    if(env_vars=="st" || env_vars=="bt"){
      vpal <- brewer.pal(11,"Spectral")
      vvals <- NULL
      vvals=c(variable@data@min,variable@data@max)
      vpalette2 <- colorNumeric(vpal, vvals,
                               na.color = "transparent")
      lmap <-addRasterImage(lmap, projectRasterForLeaflet(variable),colors=vpalette2)
      lmap <- addLegend(lmap, "bottomright", values = vvals,pal=vpalette2,title = "Temperature (°C)")
      lmap
    }

    else if(env_vars=="SS" | env_vars=="bs"){
      vpal <- brewer.pal(11,"PiYG")
      vvals <- NULL
      vvals=c(variable@data@min,variable@data@max)
      vpalette2 <- colorNumeric(vpal, vvals,
                                na.color = "transparent")
      lmap <-addRasterImage(lmap, projectRasterForLeaflet(variable),colors=vpalette2)
      lmap <- addLegend(lmap, "bottomright", values = vvals,pal=vpalette2,title = "Salinity (PSU)")
      lmap
    }

    else if(env_vars=="sh"){
      vpal <- brewer.pal(11,"BrBG")
      vvals <- NULL
      vvals=c(variable@data@min,variable@data@max)
      vpalette2 <- colorNumeric(vpal, vvals,
                                na.color = "transparent")
      lmap <-addRasterImage(lmap, projectRasterForLeaflet(variable),colors=vpalette2)
      lmap <- addLegend(lmap, "bottomright", values = vvals,pal=vpalette2,title = "Sea height (m)")
      lmap
    }
  })
  
  
  
})

shinyApp(ui = ui, server = server)