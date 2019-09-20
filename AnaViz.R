library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(sf)
# 
# ChefLieu <- readOGR("Diocese/chefs_lieux_diocèse_1317L93.shp", encoding = "system")
# ChefLieu <- st_as_sf(ChefLieu)
# ChefLieu <- st_as_sf( ChefLieu, coords = c("long", "lat") ) %>%
#   st_set_crs( 2154 ) %>%   #set coordinate system used
#   st_transform( 4326 )     #transform coordinates to WGS84 coordinates
# ChefLieu <- ChefLieu %>%
#   mutate(long = unlist(map(ChefLieu$geometry,1)),
#          lat = unlist(map(ChefLieu$geometry,2)))
# write_rds(ChefLieu,"ChefLieu1317.Rds")

ChefLieu1317 <- readRDS("ChefLieu1317.Rds")

Diocese1317 <- readRDS("Diocese1317.Rds")


T0New <- readRDS("T0New.Rds"  )
T0Impl <- readRDS("T0impl.Rds")

#liste input
listObs <- list("Coutumiers",
                "Règles")

listOrdr <- list("Bénédictins",
                 "Chanoines réguliers",
                 "Monachisme érémitique",
                 "Hospitaliers et militaires",
                 "Mendiants",
                 "Clercs réguliers")

listStat <- list("Régulier",
                  "Séculier",
                  "Séculier communautaire",
                  "Autre")

listEcol <- list("École dépendante",
                 "École capitulaire",
                 "École du monastère")

listComm <- list("Masculine",
                 "Double",
                 "Féminine")

ui <- fluidPage(
  
  tags$head(tags$style(".checkbox{
                       margin-top:2px;
                       margin-bottom:2px
  }"
  )),

  # Sidebar layout with input and output definitions ----
  sidebarLayout( 
    position = "right",
    
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      width = 3,
      style = "height: 97vh; overflow: auto;",
      
      checkboxInput("Stat",tags$b("Statuts")),
      checkboxGroupInput("Statc",
                         label = NULL,
                         choices = listStat,
                         selected = ""),
      checkboxInput("Ordr",tags$b("Ordres")),
      checkboxGroupInput("Ordrc",
                         label = NULL,
                         choices = listOrdr,
                         selected = ""),
      
      checkboxInput("Obs",tags$b("Observance")),
      checkboxGroupInput("Obsc",
                         label = NULL,
                         choices = listObs,
                         selected = ""),
      checkboxInput("Comm",tags$b("Type de communauté")),
      checkboxGroupInput("Commc",
                         label = NULL,
                         choices = listComm,
                         selected = ""),
      checkboxInput("Ecol",tags$b("École")),
      checkboxGroupInput("Ecolc",
                         label = NULL,
                         choices = listEcol,
                         selected = "")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(width = 9,
      leafletOutput("map", height = "60vh"),
      plotOutput("distribPlot", height = "37vh",
                 brush = brushOpts(id = "distribPlot_brush", direction = "x", resetOnNew = FALSE))
    )
  )
)


server <- function(input, output, session) {
  
  #select all on checkbox
  observe({
    updateCheckboxGroupInput(
      session,
      'Obsc',
      choices = listObs,
      selected = if (input$Obs) listObs
    )
  })

  observe({
    updateCheckboxGroupInput(
      session,
      'Ordrc',
      choices = listOrdr,
      selected = if (input$Ordr) listOrdr
    )
  })

  observe({
    updateCheckboxGroupInput(
      session,
      'Statc',
      choices = listStat,
      selected = if (input$Stat) listStat
    )
  })

  observe({
    updateCheckboxGroupInput(
      session,
      'Ecolc',
      choices = listEcol,
      selected = if (input$Ecol) listEcol
    )
  })

  observe({
    updateCheckboxGroupInput(
      session,
      'Commc',
      choices = listComm,
      selected = if (input$Comm) listComm
    )
  })
  
  
  #filtrer les points par les input (carac et modalités)
  points <- reactive({
    temp <- select(T0New, lng, lat)
      temp <- filter(T0New, modAgreg %in% c(input$Obsc,
                                            input$Ordrc,
                                            input$Statc,
                                            input$Ecolc,
                                            input$Commc))
    temp
  })
  
  #filtrer les données par attribut du graphique
  filteredGraphData <- reactive({

    noSelection <- TRUE
    currentlyFiltered <- points()
    
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
  
  #filtrer les données par fenêtre spatiale
  filteredSpatialData <- reactive({
    
    currentlyFilteredMap <- points()
    
    if(length(input$map_draw_all_features$features) > 0){
      coordSelBox <- unlist(input$map_draw_all_features$features[[1]]$geometry$coordinates)[c(1,2,4,5)]
      currentlyFilteredMap <- currentlyFilteredMap %>% 
        filter(lng >= coordSelBox[1], lng <= coordSelBox[4]) %>% 
        filter(lat >= coordSelBox[2], lat <= coordSelBox[3])
      return(currentlyFilteredMap)
    }
  })
  

  
  #Sortie map
  output$map <- renderLeaflet({
    
    T0New <- points()
    
    leaflet()%>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Diocèse", "Chefs lieux de Diocèse"),
        options = layersControlOptions(collapsed = F)
      ) %>% 
      addMapPane("PaneDiocese", zIndex = 410) %>%  # Level 1
      addMapPane("PaneT0NewBlack", zIndex = 420) %>%  # Level 2
      addMapPane("PaneT0New", zIndex = 430) %>%  # Level 3
      addMapPane("PaneChefsLieux", zIndex = 440) %>%  # Level 4
      hideGroup("Diocèse") %>% 
      hideGroup("Chefs lieux de Diocèse") %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(
        data = T0New,
        radius = 4,
        color = 'red',
        stroke = FALSE,
        fillOpacity = 1,
        popup = ~paste("Nom : ", usual_name),
        group = 'reactive',
        options = pathOptions(pane = "PaneT0New")
      )%>%
      addPolygons(
        data = st_transform(Diocese1317, crs = 4326),
        stroke = TRUE,
        weight = 1,
        opacity = 1,
        color = "grey",
        fill= FALSE,
        fillColor = "grey",
        fillOpacity = 0,
        group = "Diocèse",
        options = pathOptions(pane = "PaneDiocese")
      )%>% 
      addCircleMarkers(
        data = ChefLieu1317,
        lat = ChefLieu1317$lat,
        lng = ChefLieu1317$long,
        radius = 5,
        color = 'blue',
        stroke = FALSE,
        fillOpacity = 1,
        popup = ~paste("Nom : ", Nom_Commun),
        group = "Chefs lieux de Diocèse",
        options = pathOptions(pane = "PaneChefsLieux")
      )%>% 
      addDrawToolbar(polylineOptions = FALSE, polygonOptions = FALSE, circleOptions = FALSE,
                     markerOptions = FALSE, singleFeature = TRUE, circleMarkerOptions = FALSE,
                     editOptions = editToolbarOptions(edit = FALSE, remove = TRUE)
                     )
  })
  
  observe({
    if(length(filteredGraphData()) > 1){
      T0New <- points()
      mapData <- filteredGraphData()
      mapProxy <- leafletProxy("map", session = session)
      mapProxy %>%
        clearGroup('reactive') %>% 
        addCircleMarkers(
          data = T0New,
          lat = T0New$lat,
          lng = T0New$lng,
          radius = 1,
          color = 'black',
          stroke = FALSE,
          fillOpacity = 1,
          group = 'reactive',
          options = pathOptions(pane = "PaneT0NewBlack")
        ) %>%
        addCircleMarkers(
          data = mapData,
          lat = mapData$lat,
          lng = mapData$lng,
          radius = 4,
          color = 'red',
          stroke = FALSE,
          fillOpacity = 1,
          popup = ~paste("<strong>Nom :</strong>", usual_name,"</br>",
                         "<strong>Diocèse :</strong>", Diocese,"</br>",
                         "<strong>D1 :</strong>", date_start_min,"</br>",
                         "<strong>D2 :</strong>", date_start_max,"</br>",
                         "<strong>D3 :</strong>", date_stop_min,"</br>",
                         "<strong>D4 :</strong>", date_stop_max),
          group = 'reactive',
          options = pathOptions(pane = "PaneT0New")
        )
    } else {
      mapData <- points()
      mapProxy <- leafletProxy("map", session = session)
      mapProxy %>%
        clearGroup('reactive') %>% 
        addCircleMarkers(
          data = mapData,
          lat = mapData$lat,
          lng = mapData$lng,
          radius = 4,
          color = 'red',
          stroke = FALSE,
          fillOpacity = 1,
          popup = ~paste("<strong>Nom :</strong>", usual_name,"</br>",
                         "<strong>Diocèse :</strong>", Diocese,"</br>",
                         "<strong>D1 :</strong>", date_start_min,"</br>",
                         "<strong>D2 :</strong>", date_start_max,"</br>",
                         "<strong>D3 :</strong>", date_stop_min,"</br>",
                         "<strong>D4 :</strong>", date_stop_max),
          group = 'reactive',
          options = pathOptions(pane = "PaneT0New")
        )
    }
  })
  
  #Sortie graph
  output$distribPlot <- renderPlot({
    
    T0New <- points()
    
    distribPlot <- ggplot(T0New,aes(date_start_min)) +
      geom_density(col = "#053144", fill = "#43a2ca", alpha = 0.3, adjust = 0.75)
    
    
      if(!is.null(nrow(filteredSpatialData())) && nrow(filteredSpatialData())> 1){

        print(nrow(filteredSpatialData()))
        
        mapSeldistribData <- filteredSpatialData() %>%
          group_by(date_start_min) %>%
          summarise(Nb = n()) %>%
          mutate(Freq = Nb / sum(Nb))
        
        rangeY <- layer_scales(distribPlot)$y$range$range[2]

        distribPlot <- distribPlot +
          geom_density(data = mapSeldistribData, aes(date_start_min),col = "red", fill = "red", alpha = 0.3, adjust = 0.75)
         }
    if(!is.null(nrow(filteredSpatialData())) && nrow(filteredSpatialData())==1){
      
        print(nrow(filteredSpatialData()))

        mapSeldistribData <- filteredSpatialData() %>%
          group_by(date_start_min) %>%
          summarise(Nb = n()) %>%
          mutate(Freq = Nb / sum(Nb))

        rangeY <- layer_scales(distribPlot)$y$range$range[2]

        distribPlot <- distribPlot +
          geom_col(data = mapSeldistribData, aes(date_start_min, rangeY),fill = "red", alpha = 0.3, col = "red", width = 1)

      }
    
    
    return(distribPlot)
  })
 
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
