# census tract demographics dashboard: https://data.census.gov/map?layer=VT_2021_140_00_PY_D1&basemap=detailed&loc=39.6082,-104.9944,z11.7176

library(shiny)
library(shinyWidgets)

library(tidyverse)
library(reticulate)

library(leaflet)
library(osrm)
library(sf)

walkability <- readRDS("walkabilityData.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Commute Time + Walkability Index + Zillow Listings"),
  shinybusy::add_busy_spinner(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
                 textInput(inputId = "commuteLocation",
                           label = "Commute Location Lat, Long (comma-separated)",
                           value = "39.402709529019255, -104.87974538465836",
                           placeholder = "lat, long"),
                 numericInput(inputId = "commuteTime",label = "Max. Commute Time (Minutes)",min = 1,value = 35),
                 checkboxInput(inputId = "showZillow",label = "Show Zillow listings in area",value = FALSE),
                 conditionalPanel(condition = "input.showZillow==1",
                                  textInput(inputId = "searchTerm",label = "Zillow Search Term",value = "colorado"),
                                  shinyWidgets::numericRangeInput(inputId = "price",label = "Price",value = c(200000,500000)),
                                  shinyWidgets::numericRangeInput(inputId = "beds",label = "Beds",value = c(2,10)),
                                  shinyWidgets::numericRangeInput(inputId = "baths",label = "Baths",value = c(1.5,10)),
                                  checkboxInput(inputId = "noTownhomes",label = "Filter-out Townhomes",value = FALSE))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 9,
              tags$style(type = "text/css", "#commuteMap {height: calc(100vh - 80px) !important;}"),
              leaflet::leafletOutput(outputId = "commuteMap")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$commuteMap <- renderLeaflet({
    
    req(input$commuteLocation)
    req(input$commuteTime)
    
    latLong <- input$commuteLocation %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      map_dbl(as.numeric)
    
    req(length(latLong) == 2 & all(is.numeric(latLong)))
    
    iso <- osrm::osrmIsochrone(loc = c(latLong[2],latLong[1]),breaks = input$commuteTime) %>%
      st_as_sf() %>%
      st_transform(crs = 4326)
    
    iso_bbox <- sf::st_bbox(iso)
    
    sf_use_s2(FALSE)
    walkability_plt <- walkability %>%
      st_intersection(iso)
    
    pal <- colorNumeric(
      palette = "Purples",
      domain = walkability$NatWalkInd[walkability$NatWalkInd > 11],
      na.color = "white"
    )
    
    ret <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addMarkers(lng = latLong[2],lat = latLong[1]) %>%
      addPolygons(data = iso,
                  weight = 2,color = "#013220",
                  fill = FALSE) %>%
      addPolygons(data = walkability_plt,
                  stroke = TRUE,weight = .3,color="black",
                  fillOpacity = .6,fill = TRUE,
                  fillColor = ~pal(NatWalkInd),
                  popup = paste0(walkability_plt$NAME,", Census Tract ",walkability_plt$tract,"<br>Walk Index: ",round(walkability_plt$NatWalkInd,2),"<br><a href='",walkability_plt$censusLink,"' target='blank'>Census Info</a>")) 
    
    if(input$showZillow){
      pyZillow <- reticulate::py_run_file("searchZillow.py")
      
      listings <- pyZillow$searchZillow(mapBounds = iso_bbox,price = input$price,beds = input$beds,baths = input$baths)
      
      colnames(listings[[1]]) <- listings[[2]]
      
      zillowDat <- listings[[1]] %>%
        as_tibble() %>%
        select(zpid,price,beds,baths,area,latLong,statusText,imgSrc,detailUrl,address,timeOnZillow
               # ,hdpData,variableData,info3String,brokerName,streetViewMetadataURL,streetViewURL,info1String
        ) %>%
        mutate(price = map_dbl(price,~ as.numeric(str_remove_all(str_remove_all(.,"\\$"),","))),
               across(c(beds,baths,area,timeOnZillow),as.numeric),
               across(c(zpid,statusText,imgSrc,detailUrl,address),as.character),
               lat = map_dbl(latLong,~ as.numeric(.$latitude)),
               long = map_dbl(latLong, ~ as.numeric(.$longitude))) %>%
        select(zpid,lat,long,price,statusText,beds,baths,timeOnZillow,area,address,imgSrc,detailUrl) %>%
        sf::st_as_sf(coords = c("long","lat"),crs = 4326) %>%
        sf::st_intersection(iso) %>%
        mutate(longlat = sf::st_coordinates(.))
      
      if(input$noTownhomes){
        zillowDat <- zillowDat %>%
          filter(!str_detect(tolower(statusText),"townho"))
      }
      
      markerLabels <- ifelse(is.na(zillowDat$price),"$???",paste0("$",round(zillowDat$price/1000),"K"))
      
      ret <- ret %>%
        # addCircleMarkers(lng = zillowDat$longlat[,1],lat = zillowDat$longlat[,2],
        #                  radius = 13,
        #                  stroke=FALSE,
        #                  fillColor = "#a3000b",
        #                  fillOpacity = 1,
        #                  options = markerOptions(riseOnHover = TRUE),
        #                  label = markerLabels,
        #                  labelOptions = labelOptions(noHide=TRUE, noWrap=TRUE,direction = "center",opacity = 1,textOnly = TRUE,zoomAnimation = FALSE,
        #                                              style = list("color" = "white","font-size"="9px")),
        #                  popup = paste0("<img src='",zillowDat$imgSrc,"' height='150px' width='150px'/><br>",
        #                                    "$",zillowDat$price," ",zillowDat$statusText,"<br>",
      #                                    zillowDat$beds," beds, ",zillowDat$baths,
      #                                    " baths<br><a href='https://www.zillow.com/",zillowDat$detailUrl,"' target='_blank'>Zillow Link</a>"))
      addMarkers(lng = zillowDat$longlat[,1],lat = zillowDat$longlat[,2],
                 popup = paste0("<img src='",zillowDat$imgSrc,"' height='150px' width='150px'/><br>",
                                "$",zillowDat$price," ",zillowDat$statusText,"<br>",
                                zillowDat$beds," beds, ",zillowDat$baths," baths<br>",
                                zillowDat$address,"<br>",
                                "<a href='https://www.zillow.com/",zillowDat$detailUrl,"' target='blank'>Zillow Link</a>"))
    }
    
    return(ret)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
