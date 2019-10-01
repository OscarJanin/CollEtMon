library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)



#Create T0New data
lat <- c(49.823, 49.823, 58.478, 57.478, 45.823)
lng <- c(-10.854,-10.854,-10.854,2.021,2.02)
date_start_min <- c(123,125,135,168,149)
T0New <- data.frame(lat,lng)

#Create T1New data
lat <- c(48.956, 56.356, 57.445, 45.253)
lng <- c(-9.762,-8.884,1.971,2.17)
T1New <- data.frame(lat,lng)

ui <- fluidPage(
  
  leafletOutput("map", height = "50vh"),
  plotOutput("distribPlot", height = "47vh",
             brush = brushOpts(id = "distribPlot_brush", direction = "x", resetOnNew = FALSE))
)


server <- function(input, output, session) {
  
  
  
  #filtrer les données par attribut du graphique
  filteredGraphData <- reactive({
    
    noSelection <- TRUE
    currentlyFiltered <- T0New
    
    if(!is.null(input$distribPlot_brush)){
      thisSel <- input$distribPlot_brush
      currentlyFiltered <- currentlyFiltered %>% 
        filter(date_start_min >= thisSel$xmin, date_start_min <= thisSel$xmax)
      noSelection <- FALSE
    }
    if(!noSelection){
      return(currentlyFiltered)
    }
  })
  
  #Sortie map
  output$map <- renderLeaflet({
    leaflet()%>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = "T1New",
        options = layersControlOptions(collapsed = F)
      ) %>% 
      hideGroup("T1New") %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addMarkers(
        data = T0New,
        lat = T0New$lat,
        lng = T0New$lng,
        group = 'A',
        clusterId = "mycluster",
        clusterOptions = markerClusterOptions(maxClusterRadius =0
                                                  )
      )%>%
      addCircleMarkers(
        lat = T1New$lat,
        lng = T1New$lng,
        radius = 5,
        color = 'blue',
        stroke = FALSE,
        fillOpacity = 1,
        group = "T1New"
      )
  })
  
  observe({
    if(length(filteredGraphData()) > 1){
      mapData <- filteredGraphData()
      mapProxy <- leafletProxy("map", session = session, data = c(mapData, T0New))
      mapProxy %>%
        clearGroup('A') %>% 
        addCircleMarkers(
          data = T0New,
          lat = T0New$lat,
          lng = T0New$lng,
          radius = 1,
          color = 'black',
          stroke = FALSE,
          fillOpacity = 1,
          group = 'A'
        ) %>%
        addMarkers(
          data = mapData,
          lat = mapData$lat,
          lng = mapData$lng,
          group = 'A',
          clusterId = "mycluster",
          clusterOptions = markerClusterOptions(maxClusterRadius =0
                                                  )
        )
    }else{
      mapProxy <- leafletProxy("map", session = session, data = T0New)
      mapProxy %>%
        clearGroup('A') %>% 
        addMarkers(
          data = T0New,
          lat = T0New$lat,
          lng = T0New$lng,
          group = 'A',
          clusterId = "mycluster",
          clusterOptions = markerClusterOptions(maxClusterRadius =0
                                                  )
        )
    }
  })
  
  #Sortie graph
  output$distribPlot <- renderPlot({
    
    distribPlot <- ggplot(T0New,aes(date_start_min)) +
      geom_density(col = "#053144", fill = "#43a2ca", alpha = 0.3, adjust = 0.75)
    
    return(distribPlot)
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)