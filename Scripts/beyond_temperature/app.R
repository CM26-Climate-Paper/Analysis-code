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

###### 2. define global objects #####
contemp_vars_dir="data/contemporary/"
project_vars_dir="data/project/"
full_models_dir="data/species_projections_full/"
partial_models_dir="data/species_projections_partial/"
species=read.csv("data/List_of_Species_with_Records_Above_m07.csv")
commonNames=species$name %>% as.character()
timeperiods=c("Contemporary","Y20-40","Y40-60","Y60-80")
env_vars=c("Surface temperature","Bottom temperature","Surface salinity","Bottom salinity","Sea height")


ui <- dashboardPage(skin="black",
                    dashboardHeader(disable=T
                      # title = tags$div(style="color:white","Projecting  marine  species  range  shifts  from  only  temperature  can  mask  climate  vulnerability"),
                      # titleWidth = "100%"
                    ),
                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(id = 'sidebarmenu',
                      selectInput("species","Select species",commonNames,width = "100%"),
                      selectInput("timePeriod","Select projection period",timeperiods,width = "100%"),
                      selectInput("variable","Select hydrological covariate",env_vars,width = "100%")
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
             column(h6(style="padding:0px;height:0px;","* - Corresponding author"),width = 12),
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
  
  timePeriodID=reactive({
    #### get time period
    if(input$timePeriod=="Contemporary"){
      timePeriodID="_contemp"
    }
    if(input$timePeriod=="Y20-40"){
      timePeriodID="_av20_40"
    }
    if(input$timePeriod=="Y40-60"){
      timePeriodID="_av40_60"
    }
    if(input$timePeriod=="Y60-80"){
      timePeriodID="_av60_80"
    }
    return(timePeriodID)
  })

  variableID=reactive({
  if(input$variable=="Surface temperature"){
    variableID="st"
  }
  if(input$variable=="Bottom temperature"){
    variableID="bt"
  }
  if(input$variable=="Surface salinity"){
    variableID="SS"
  }
  if(input$variable=="Bottom salinity"){
    variableID="bs"
  }
  if(input$variable=="Sea height"){
    variableID="sh"
  }
  return(variableID)
  })
  ####
  
  #### ----> colors ####
  pal <- rev(brewer.pal(11,"RdYlBu"))
  vals <- NULL
  vals=c(0,1)
  palette2 <- colorNumeric(pal, vals,
                           na.color = "transparent") 
  ####

  
  output$full <- renderLeaflet({
    display=raster(paste0("data/species_projections_full/",speciesID(),"/",speciesID(),timePeriodID(),".tif"))
    lmap <- leaflet()
    lmap <- addProviderTiles(lmap, "CartoDB.Positron",options = providerTileOptions(noWrap = TRUE))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(display,method="bilinear"),colors=palette2)
    lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Species habitat suitability")
    })
  
  output$partial <- renderLeaflet({
    display=raster(paste0("data/species_projections_partial/",speciesID(),"/",speciesID(),timePeriodID(),".tif"))
    lmap <- leaflet()
    lmap <- addProviderTiles(lmap, "CartoDB.Positron",options = providerTileOptions(noWrap = TRUE))
    lmap <-addRasterImage(lmap, projectRasterForLeaflet(display,method="bilinear"),colors=palette2)
    lmap <- addLegend(lmap, "bottomright", values = vals,pal=palette2,title = "Species habitat suitability")
  })
  
  output$covariate <- renderLeaflet({
    variable=raster(paste0("data/project/",substring(timePeriodID(),2),"/",variableID(),".tif"))
    lmap <- leaflet()
    lmap <- addProviderTiles(lmap, "CartoDB.Positron",options = providerTileOptions(noWrap = TRUE))
    
    # ### adding legend
    if(variableID()=="st" || variableID()=="bt"){
      vpal <- rev(brewer.pal(11,"Spectral"))
      vvals <- NULL
      vvals=c(variable@data@min,variable@data@max)
      vpalette2 <- colorNumeric(vpal, vvals,
                                na.color = "transparent")
      lmap <-addRasterImage(lmap, projectRasterForLeaflet(variable,method="bilinear"),colors=vpalette2)
      lmap <- addLegend(lmap, "bottomright", values = vvals,pal=vpalette2,title = "Temperature (°C)")
      lmap
    }
    
    else if(variableID()=="SS" | variableID()=="bs"){
      vpal <- brewer.pal(11,"PiYG")
      vvals <- NULL
      vvals=c(variable@data@min,variable@data@max)
      vpalette2 <- colorNumeric(vpal, vvals,
                                na.color = "transparent")
      lmap <-addRasterImage(lmap, projectRasterForLeaflet(variable,method="bilinear"),colors=vpalette2)
      lmap <- addLegend(lmap, "bottomright", values = vvals,pal=vpalette2,title = "Salinity (PSU)")
      lmap
    }
    
    else if(variableID()=="sh"){
      vpal <- brewer.pal(11,"BrBG")
      vvals <- NULL
      vvals=c(variable@data@min,variable@data@max)
      vpalette2 <- colorNumeric(vpal, vvals,
                                na.color = "transparent")
      lmap <-addRasterImage(lmap, projectRasterForLeaflet(variable,method="bilinear"),colors=vpalette2)
      lmap <- addLegend(lmap, "bottomright", values = vvals,pal=vpalette2,title = "Sea height (m)")
      lmap
    }
  })
  
})


shinyApp(ui = ui, server = server)