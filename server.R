source("global.R")

shinyServer(function(input, output, session) {
  
  #filtrer les factoides par les input ----
  points <- reactive({
    T0NewSel <- filter(T0New, modAgreg %in% c(input$Obsc,
                                              input$Ordrc,
                                              input$Statc,
                                              input$Ecolc,
                                              input$Commc,
                                              input$Relc))

    # T0NewSel <- filter(T0NewSel, TypeEntiete != "École")
    idImplSel <- unique(T0NewSel$idimplantation)
    T0NewFiltre <- filter(T0New, idimplantation %in% idImplSel)
    T0NewAffiche <- filter(T0NewFiltre, caracNew %in% input$conf)
    if(input$etat == "État final"){
      T0NewAfficheF <- as.data.frame(T0NewAffiche %>%  group_by(idimplantation) %>%  filter(date_stop_max == max(date_stop_max)))
    }else if(input$etat == "État initial"){
      T0NewAfficheF <- as.data.frame(T0NewAffiche %>%  group_by(idimplantation) %>%  filter(date_start_min == min(date_start_min)))
    }else if(input$etat == "État dominant"){
      T0NewAfficheF <- as.data.frame(T0NewAffiche %>%  group_by(idimplantation) %>%  filter(DureeFact == max(DureeFact)))
    }
    return(T0NewAfficheF)
  })
  
  #filtrer les factoides par le graphique ----
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
  
  #filtrer les factoides par fenÃªtre spatiale ----
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
  
  #filtrer les liens par les input ----
  lignes <- reactive({
    LiensSel <- filter(Liens, modAgreg == input$Relc)
    
    idImplSel <- unique(LiensSel$idimplantation)
    LiensFiltre <- filter(Liens, idimplantation %in% idImplSel)
    if(input$etat == "État final"){
      LiensAfficheF <- LiensFiltre %>%  group_by(idimplantation) %>%  filter(date_stop_max == max(date_stop_max))
    }else if(input$etat == "État initial"){
      LiensAfficheF <- LiensFiltre %>%  group_by(idimplantation) %>%  filter(date_start_min == min(date_start_min))
    }else if(input$etat == "État dominant"){
      LiensAfficheF <- LiensFiltre %>%  group_by(idimplantation) %>%  filter(DureeFact == max(DureeFact))
    }
    return(LiensAfficheF)
  })
  
  #filtrer les liens par le graphique ----
  filteredGraphLines <- reactive({
    
    currentlyFiltered <- lignes()
    
    if(!is.null(input$distribPlot_brush)){
      thisSel <- input$distribPlot_brush
      currentlyFiltered <- currentlyFiltered %>% 
        filter(date_start_min >= thisSel$xmin, date_start_min <= thisSel$xmax)
    }
    return(currentlyFiltered)
  })
  
  #filtrer les ecoles par les input ----
  ecole <- reactive({
    ecoleSel <- filter(Ecole, modAgreg == input$Ecolc)
    
    idImplSel <- unique(ecoleSel$idimplantation)
    ecoleFiltre <- filter(Ecole, idimplantation %in% idImplSel)
    if(input$etat == "État final"){
      ecoleAfficheF <- ecoleFiltre %>%  group_by(idimplantation) %>%  filter(date_stop_max == max(date_stop_max))
    }else if(input$etat == "État initial"){
      ecoleAfficheF <- ecoleFiltre %>%  group_by(idimplantation) %>%  filter(date_start_min == min(date_start_min))
    }else if(input$etat == "État dominant"){
      ecoleAfficheF <- ecoleFiltre %>%  group_by(idimplantation) %>%  filter(DureeFact == max(DureeFact))
    }
    return(ecoleAfficheF)
  })
  
  #filtrer les lignes par le graphique ----
  filteredGraphEcole <- reactive({
    
    currentlyFiltered <- ecole()
    
    if(!is.null(input$distribPlot_brush)){
      thisSel <- input$distribPlot_brush
      currentlyFiltered <- currentlyFiltered %>% 
        filter(date_start_min >= thisSel$xmin, date_start_min <= thisSel$xmax)
    }
    return(currentlyFiltered)
  })
  
  
  #Sortie map ----
  output$map <- renderLeaflet({
    leaflet()%>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Diocèse", "Chefs lieux de Diocèse","hors-selection"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      addMapPane("PaneDiocese", zIndex = 410) %>%  # Level 1
      addMapPane("PaneT0NewBlack", zIndex = 420) %>%  # Level 2
      addMapPane("paneEcole", zIndex = 430) %>%   # Level 3
      addMapPane("PaneT0New", zIndex = 440) %>%  # Level 4
      addMapPane("PaneChefsLieux", zIndex = 450) %>%  # Level 5
      hideGroup("Diocèse") %>% 
      hideGroup("Chefs lieux de Diocèse") %>%
      addProviderTiles(providers$CartoDB.Positron) %>% 
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
        radius = 6,
        color = '#424242',
        stroke = FALSE,
        fillOpacity = 1,
        popup = ~paste("Nom : ", Nom_Commun),
        group = "Chefs lieux de Diocèse",
        options = pathOptions(pane = "PaneChefsLieux")
      )%>%
      addCircleMarkers(
        data = T0Impl,
        lat = T0Impl$lat,
        lng = T0Impl$lng,
        radius = 1,
        color = "black",
        stroke = FALSE,
        fillOpacity = 1,
        group = 'hors-selection',
        options = pathOptions(pane = "PaneT0NewBlack")
      ) %>%
      addDrawToolbar(rectangleOptions = list(shapeOptions = drawShapeOptions(color = colorSpat,
                                                                        fillOpacity = 0.5,
                                                                        fillColor = colorSpat,
                                                                        weight = 1)),
                     polylineOptions = F, polygonOptions = F, circleOptions = F,
                     markerOptions = F, singleFeature = T, circleMarkerOptions = F,
                     editOptions = editToolbarOptions(edit = F, remove = T)
      )
  })
  
  #Affichage factoides filtré par le graph ----
  observe({
    if(length(filteredGraphData()) > 1){
      mapData <- filteredGraphData()
      mapProxy <- leafletProxy("map", session = session)
      mapProxy %>%
        clearGroup('reactive') %>% 
        addCircleMarkers(
          data = mapData,
          lat = mapData$lat,
          lng = mapData$lng,
          radius = 3,
          color = ~modaNiv1_Color,
          stroke = FALSE,
          fillOpacity = 1,
          popup = ~paste("<strong>Nom :</strong>", usual_name,"</br>",
                         "<strong>Diocèse :</strong>", Diocese,"</br>",
                         "<strong>D1 :</strong>", date_start_min,"</br>",
                         "<strong>D2 :</strong>", date_start_max,"</br>",
                         "<strong>D3 :</strong>", date_stop_min,"</br>",
                         "<strong>D4 :</strong>", date_stop_max,"</br>",
                         "<strong>caracNew :</strong>",caracNew,"</br>",
                         "<strong>Modalité :</strong>",modAgreg,"</br>",
                         "<strong>link :</strong>",linked_implantation_name),
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
          radius = 3,
          color = ~modaNiv1_Color,
          stroke = F,
          fillOpacity = 1,
          popup = ~paste("<strong>Nom :</strong>", usual_name,"</br>",
                         "<strong>Diocèse :</strong>", Diocese,"</br>",
                         "<strong>D1 :</strong>", date_start_min,"</br>",
                         "<strong>D2 :</strong>", date_start_max,"</br>",
                         "<strong>D3 :</strong>", date_stop_min,"</br>",
                         "<strong>D4 :</strong>", date_stop_max,"</br>",
                         "<strong>caracNew :</strong>",caracNew,"</br>",
                         "<strong>Modalité :</strong>",modAgreg,"</br>",
                         "<strong>link :</strong>",linked_implantation_name),
          group = 'reactive',
          options = pathOptions(pane = "PaneT0New")
        )
    }
  })
  
  #Affichage des liens ----
  observe({
    if(input$RelR==T){
      Liens <- filteredGraphLines()
      mapProxy <- leafletProxy("map", session = session)
      mapProxy %>%
        clearGroup('links') %>%
        addPolylines(data = Liens,
                     color = ~liensPal(modAgreg),
                     opacity = 1,
                     weight = 1.5,
                     stroke = T,
                     group = 'links',
                     popup = ~paste("<strong>Nom :</strong>", usual_name,"</br>",
                                    "<strong>Diocèse :</strong>", Diocese,"</br>",
                                    "<strong>D1 :</strong>", date_start_min,"</br>",
                                    "<strong>D2 :</strong>", date_start_max,"</br>",
                                    "<strong>D3 :</strong>", date_stop_min,"</br>",
                                    "<strong>D4 :</strong>", date_stop_max,"</br>",
                                    "<strong>caracNew :</strong>",caracNew,"</br>",
                                    "<strong>Modalité :</strong>",modAgreg)
        )
      show(id ='lineLegend')
    }else if (input$RelR==F){
      mapProxy <- leafletProxy("map", session = session)
      mapProxy %>%
        clearGroup('links')
      hide(id = 'lineLegend')
    }
  })
  
  #Affichage des ecoles ----
  observe({
    if(input$EcolR==T){
      Ecole <- filteredGraphEcole()
      mapProxy <- leafletProxy("map", session = session)
      mapProxy %>%
        clearGroup('ecol') %>% 
        addCircleMarkers(
          data = Ecole,
          lat = Ecole$lat,
          lng = Ecole$lng,
          radius = 5,
          color = "#333333",
          stroke = F,
          fillOpacity = 1,
          popup = ~paste("<strong>Nom :</strong>", usual_name,"</br>",
                         "<strong>Diocèse :</strong>", Diocese,"</br>",
                         "<strong>D1 :</strong>", date_start_min,"</br>",
                         "<strong>D2 :</strong>", date_start_max,"</br>",
                         "<strong>D3 :</strong>", date_stop_min,"</br>",
                         "<strong>D4 :</strong>", date_stop_max,"</br>",
                         "<strong>caracNew :</strong>",caracNew,"</br>",
                         "<strong>Modalité :</strong>",modAgreg,"</br>",
                         "<strong>link :</strong>",linked_implantation_name),
          group = 'ecol',
          options = pathOptions(pane = "paneEcole")
        )
      show(id ='ecoleLegend')
    }else if (input$EcolR==F){
      mapProxy <- leafletProxy("map", session = session)
      mapProxy %>%
        clearGroup('ecol')
      hide(id = 'ecoleLegend')
    }
  })
  
  #Affichage legende section affichage
  # if(input$conf=="Statuts"){
  #   show(id = 'statLegend')
  # }else{
  #   hide(id = 'statLegend')
  # }

  
  
  #Sortie graph----
  output$distribPlot <- renderPlot({
    
    T0New <- points()
    
    distribPlot <- ggplot(T0New,aes(date_start_min)) +
      geom_density(col = colorAttr, fill = colorAttr, alpha = 0.3, adjust = 0.75)
    
    
    if(!is.null(nrow(filteredSpatialData())) && nrow(filteredSpatialData())> 1){
      
      print(nrow(filteredSpatialData()))
      
      mapSeldistribData <- filteredSpatialData() %>%
        group_by(date_start_min) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      rangeY <- layer_scales(distribPlot)$y$range$range[2]
      
      distribPlot <- distribPlot +
        geom_density(data = mapSeldistribData, aes(date_start_min),col = colorSpat, fill = colorSpat, alpha = 0.3, adjust = 0.75)
    }
    if(!is.null(nrow(filteredSpatialData())) && nrow(filteredSpatialData())==1){
      
      print(nrow(filteredSpatialData()))
      
      mapSeldistribData <- filteredSpatialData() %>%
        group_by(date_start_min) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      rangeY <- layer_scales(distribPlot)$y$range$range[2]
      
      distribPlot <- distribPlot +
        geom_col(data = mapSeldistribData, aes(date_start_min, rangeY),fill = colorSpat, alpha = 0.3, col = colorSpat, width = 1)
      
    }
    
    
    #Nombre d'implantation affiché-----
    output$selAttr <- renderText({ 
        paste("Selection attributaire : ","<font color=\"#9C7B36\"><b>", count(points()), "</b></font>")
    })
    output$selTemp <- renderText({ 
      if(is.null(nrow(filteredGraphData()))){
        paste("Selection temporelle : 0")
      }else{
        paste("Selection temporelle : ","<font color=\"#294B59\"><b>", count(filteredGraphData()), "</b></font>")
      }
    })
    output$selSpat <- renderText({ 
      if(is.null(nrow(filteredSpatialData()))){
        paste("Selection spatiale : 0")
      }else{
        paste("Selection spatiale : ","<font color=\"#9B372F\"><b>", count(filteredSpatialData()), "</b></font>")
      }
    })
    
    return(distribPlot)
  })
  
})