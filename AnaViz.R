library(shiny)
library(leaflet)
library(leaflet.extras)
library(tidyverse)

T0New <- readRDS("C:/Users/Oscar/Desktop/Coll&Mon/Projet R/T0New.Rds")
T0Impl <- readRDS("C:/Users/Oscar/Desktop/Coll&Mon/Projet R/T0impl.Rds")


write_csv(T0New, "C:/Users/Oscar/Desktop/Coll&Mon/Projet R/T0New.csv")
write_csv(T0Impl, "C:/Users/Oscar/Desktop/Coll&Mon/Projet R/T0Impl.csv")

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
      leafletOutput("map", height = "50vh"),
      plotOutput("distribPlot", height = "47vh",
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
    
    leaflet(data = T0New) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(
        radius = 4,
        color = 'red',
        stroke = FALSE,
        fillOpacity = 1,
        popup = ~paste("Nom : ", usual_name)
      )%>%
      addDrawToolbar(polylineOptions = FALSE, polygonOptions = FALSE, circleOptions = FALSE,
                     markerOptions = FALSE, singleFeature = TRUE, circleMarkerOptions = FALSE,
                     editOptions = editToolbarOptions(edit = FALSE, remove = TRUE))
  })
  
  observe({
    if(length(filteredGraphData()) > 1){
      T0New <- points()
      mapData <- filteredGraphData() 
      mapProxy <- leafletProxy("map", session = session, data = c(mapData, T0New))
      mapProxy %>% 
        clearMarkers()  %>%
        addCircleMarkers(
          lat = T0New$lat,
          lng = T0New$lng,
          radius = 1,
          color = 'black',
          stroke = FALSE,
          fillOpacity = 1
        ) %>%
        addCircleMarkers(
          lat = mapData$lat,
          lng = mapData$lng,
          radius = 4,
          color = 'red',
          stroke = FALSE,
          fillOpacity = 1,
          popup = ~paste("Nom : ", usual_name)
        )
    } else {
      mapData <- points()
      mapProxy <- leafletProxy("map", session = session, data = mapData)
      mapProxy %>% 
        clearMarkers() %>%
        addCircleMarkers(
          radius = 4,
          color = 'red',
          stroke = FALSE,
          fillOpacity = 1,
          popup = ~paste("<strong>Nom :</strong>", usual_name)
        )
    }
  })
  
  #Sortie graph
  output$distribPlot <- renderPlot({
    
    T0New <- points()
    
    # distribData <- T0New %>% 
    #   group_by(date_start_min) %>% 
    #   summarise(Nb = n()) %>% 
    #   mutate(Freq = Nb / sum(Nb)) %>% 
    #   na.omit()
    # 
    # distribPlot <- ggplot(distribData, aes(date_start_min, Freq)) +
    #   geom_col(fill = "#43a2ca", alpha = .35, col = "#053144",width = 0.9)+
    #   theme_minimal()
    
    distribPlot <- ggplot(T0New,aes(date_start_min)) +
      geom_density(col = "#053144", fill = "#43a2ca", alpha = 0.3, adjust = 0.75)
    
      
      if(length(filteredSpatialData())> 1){
        mapSeldistribData <- filteredSpatialData() %>%
          group_by(date_start_min) %>%
          summarise(Nb = n()) %>%
          mutate(Freq = Nb / sum(Nb)) %>%
          na.omit()

        distribPlot <- distribPlot +
          geom_col(data = mapSeldistribData, aes(date_start_min, Freq),fill = "#43a2ca", alpha = .35, col = "red", width = 0.9)

      }
    
    return(distribPlot)
  })
 
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
