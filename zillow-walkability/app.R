# census tract demographics dashboard: https://data.census.gov/map?layer=VT_2021_140_00_PY_D1&basemap=detailed&loc=39.6082,-104.9944,z11.7176

# TODO:
# - add interactivity between map and data table. When a row is clicked, highlight the corresponding pin and vice versa

library(shiny)
library(shinyWidgets)
library(shinyjs)

library(tidyverse)
library(reticulate)
library(rvest)
library(DT)

library(leaflet)
library(osrm)
library(sf)

walkability <- readRDS("walkabilityData.rds")
pal <- colorNumeric(
  palette = "Purples",
  domain = walkability$NatWalkInd[walkability$NatWalkInd > 11],
  na.color = "white"
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # Application title
  titlePanel("Drive Time + Walkability Index + Home Listings"),
  shinybusy::add_busy_spinner(),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(width = 3,
                 textInput(inputId = "commuteLocation",
                           label = "Travel Location Lat, Long (click on map to change)",
                           value = "39.402709529019255, -104.87974538465836",
                           placeholder = "lat, long"),
                 numericInput(inputId = "commuteTime",label = "Max. Commute Time (Minutes)",min = 1,value = 35),
                 checkboxInput(inputId = "showZillow",label = "Show home listings in area",value = FALSE),
                 conditionalPanel(condition = "input.showZillow==1",
                                  # textInput(inputId = "searchTerm",label = "Zillow Search Term",value = "colorado"),
                                  shinyWidgets::numericRangeInput(inputId = "price",label = "Price ($)",value = c(200000,500000),step = 50000),
                                  shinyWidgets::numericRangeInput(inputId = "beds",label = "Beds",value = c(2,10),step = 1),
                                  shinyWidgets::numericRangeInput(inputId = "baths",label = "Baths",value = c(1.5,10),step = .5)
                                  # ,checkboxInput(inputId = "noTownhomes",label = "Filter-out Townhomes",value = FALSE)
                 ),
                 actionButton(inputId = "renderPlt",label = "Let's Go"),
                 br(),
                 br(),
                 shinyjs::hidden(actionButton(inputId = "reset",label = "Reset Map"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(width = 9,
              tags$style(type = "text/css", "#commuteMap {height: calc(100vh - 300px) !important;}"),
              leaflet::leafletOutput(outputId = "commuteMap"),
              DT::dataTableOutput(outputId = "listingTable")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  observeEvent(input$commuteMap_click,{
    updateTextInput(session = session,
                    inputId = "commuteLocation",
                    value = paste0(input$commuteMap_click$lat,", ",input$commuteMap_click$lng))
  })
  
  latLong <- reactive({
    
    input$commuteLocation %>%
      stringr::str_split(",") %>%
      .[[1]] %>%
      map_dbl(as.numeric)
    
  })
  
  iso <- reactive({
    req(input$commuteLocation)
    req(input$commuteTime)
    
    req(length(latLong()) == 2 & all(is.numeric(latLong())))
    
    osrm::osrmIsochrone(loc = c(latLong()[2],latLong()[1]),breaks = input$commuteTime) %>%
      st_as_sf() %>%
      st_transform(crs = 4326)
  })
  
  walkability_plt <- reactive({
    
    iso_bbox <- sf::st_bbox(iso())
    
    sf_use_s2(FALSE)
    walkability %>%
      st_intersection(iso())
    
  })
  
  zillowDat <- reactive({
    if(input$showZillow){
      
      ret <- map_dfr(c("Denver","Jefferson","Arapahoe"),
                     function(county){
                       
                       # browser()
                       
                       script_txt <-
                         rvest::read_html(paste0("https://www.redfin.com/county/377/CO/",
                                                 county,
                                                 "-County/filter/property-type=house,min-beds=",round(input$beds[1]),",max-beds=",round(input$beds[2]),
                                                 ",min-baths=",round(input$baths[1]),",max-baths=",round(input$baths[2]),
                                                 ",min-price=",format(input$price[1],scientific = FALSE),",max-price=",format(input$price[2],scientific = FALSE))) %>%
                         rvest::html_nodes(xpath=".//script[contains(., 'ServerState.InitialContext')]") %>%
                         html_text() %>% 
                         stringi::stri_split_lines() %>% 
                         flatten_chr()  %>% 
                         keep(stringi::stri_detect_regex, "^root.__reactServerState.InitialContext")
                       
                       script_txt <- sub('^[^\\{]*\\{', '{', script_txt)
                       
                       script_json <- jsonlite::fromJSON(str_extract(script_txt,'^.*"webViewAppName":null\\}'))
                       
                       searchResults_ind <- which(str_detect(names(script_json[[1]]$dataCache),"include_nearby_homes") & 
                                                    str_detect(names(script_json[[1]]$dataCache),"max_price=") &
                                                    !str_detect(names(script_json[[1]]$dataCache),"gis-cluster"))[1]
                       
                       searchResults <- script_json[[1]]$dataCache[[searchResults_ind]]$res$text
                       
                       homes_df <- searchResults %>%
                         str_extract('\\{\\"homes.*\\}$') %>%
                         str_sub(1,-2) %>%
                         jsonlite::fromJSON() %>%
                         .$homes  %>%
                         select(mlsId,mlsStatus,price,beds,baths,stories,latLong,streetLine,state,zip,
                                # photos,alternatePhotosInfo,
                                url,listingRemarks) %>%
                         unnest(c(mlsId,price,latLong,streetLine),
                                names_sep = "_") %>%
                         unnest(latLong_value) %>%
                         select(-contains("_level")) %>%
                         set_names(str_remove_all(names(.),"_value")) %>%
                         mutate(url = paste0("https://www.redfin.com",url),
                                county = county,
                                address = paste0(streetLine,", ",state," ",zip)) %>%
                         select(price,beds,baths,stories,latitude,longitude,address,zip,url,mlsId,mlsStatus,listingRemarks) %>%
                         filter((mlsStatus %in% c("Active","Coming Soon")))
                       
                     }) %>%
        distinct() %>%
        sf::st_as_sf(coords = c("longitude","latitude"),crs = 4326) %>%
        sf::st_intersection(iso()) %>%
        mutate(longlat = sf::st_coordinates(.))
    }
    else{
      ret <- data.frame()
    }
    
    return(ret)
  })
  
  
  
  
  # plt <- reactiveVal()
  
  observeEvent(input$renderPlt,{
    
    leafletProxy(mapId = "commuteMap")  %>%
      clearMarkers() %>%
      addPolygons(data = iso(),
                  weight = 2,color = "#013220",
                  fill = FALSE) %>%
      addPolygons(data = walkability_plt(),
                  stroke = TRUE,weight = .3,color="black",
                  fillOpacity = .6,fill = TRUE,
                  fillColor = ~pal(NatWalkInd),
                  popup = paste0(walkability_plt()$NAME,", Census Tract ",walkability_plt()$tract,"<br>Walk Index: ",round(walkability_plt()$NatWalkInd,2),"<br><a href='",walkability_plt()$censusLink,"' target='blank'>Census Info</a>")) %>%
      addCircleMarkers(lng = latLong()[2],lat = latLong()[1],fill = TRUE,fillOpacity = .8,opacity = .8)
    
    if(nrow(zillowDat()) > 0){
      leafletProxy(mapId = "commuteMap") %>%
        addMarkers(lng = zillowDat()$longlat[,1],lat = zillowDat()$longlat[,2],
                   popup = paste0(#"<img src='",zillowDat$imgSrc,"' height='150px' width='150px'/><br>",
                     zillowDat()$address,"<br>",             
                     "$",zillowDat()$price,", ",zillowDat()$mlsStatus,"<br>",#zillowDat$statusText,"<br>",
                     zillowDat()$beds," beds, ",zillowDat()$baths," baths, ",zillowDat()$stories," stories<br>"
                     ,"<a href='",zillowDat()$url,"' target='_blank'>Redfin Link</a><br>",
                     "<a href='https://www.google.com/maps/place/",str_replace_all(zillowDat()$address," ","+"),"' target='_blank'>Google Maps</a><br>"
                     # ,"<a href='https://www.zillow.com/",zillowDat$detailUrl,"' target='blank'>Zillow Link</a>"
                   )
        )
    }
    
    output$listingTable <- DT::renderDataTable({
      req(isTRUE(input$showZillow))
      req(nrow(zillowDat()) > 0)
      
      zillowDat() %>%
        st_drop_geometry() %>%
        distinct() %>%
        arrange(price,beds,baths) %>%
        mutate(redfin = paste0("<a href='",url,"' target='_blank'>Link</a><br>")) %>%
        select(address,price,beds,baths,stories,zip,mlsId,redfin,listingRemarks)
    },escape = FALSE)
    shinyjs::show(id = "listingTable")
    
    shinyjs::show(id = "reset")
    
    iso_bbox <- sf::st_bbox(iso())
    leafletProxy(mapId = "commuteMap") %>%
       fitBounds(iso_bbox[1],iso_bbox[2],iso_bbox[3],iso_bbox[4])
    
  })
  
  output$commuteMap <- renderLeaflet({ 
    
    # if(is.null(plt())){
      
      walkability_bbox <- sf::st_bbox(walkability$geometry)
      names(walkability_bbox) <- NULL
      
      return({leaflet() %>%
          addProviderTiles(providers$Esri.WorldStreetMap) %>%
          fitBounds(walkability_bbox[1],walkability_bbox[2],walkability_bbox[3],walkability_bbox[4])})
    # }
    # else{return(plt())}
    
  })
  
  observe({
    leafletProxy(mapId = "commuteMap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(lng = latLong()[2],lat = latLong()[1],fill = TRUE,fillOpacity = .8,opacity = .8)
  })
  
  observeEvent(input$reset,{
    
    walkability_bbox <- sf::st_bbox(walkability$geometry)
    names(walkability_bbox) <- NULL
    
    leafletProxy(mapId = "commuteMap") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addCircleMarkers(lng = latLong()[2],lat = latLong()[1],fill = TRUE,fillOpacity = .8,opacity = .8) %>%
      fitBounds(walkability_bbox[1],walkability_bbox[2],walkability_bbox[3],walkability_bbox[4])
    
    shinyjs::hide(id = "reset")
    shinyjs::hide(id = "listingTable")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
