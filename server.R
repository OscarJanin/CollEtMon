source("global.R")

shinyServer(function(input, output, session) {
  
  # filtrer les données par attributs
  checkboxFilter <- function(DATA){
    
    if (input$Régulier == TRUE) RegulierV <- "Régulier" else RegulierV <- ""
    if (input$Séculier == TRUE) SeculierV <- "Séculier" else SeculierV <- ""
    if (input$Séculiercommunautaire == TRUE) SeculierCommuV <- "Séculier communautaire" else SeculierCommuV <- ""
    if (input$Autres == TRUE) AutreV <- "Autre" else AutreV <- ""
    
    if (input$Bénédictins == TRUE) BenedictinsV <- "Bénédictins" else BenedictinsV <- ""
    if (input$Chanoinesréguliers == TRUE) ChanoinesregV <- "Chanoines réguliers" else ChanoinesregV <- ""
    if (input$Monachismeérémitique == TRUE) MonachismeV <- "Monachisme érémitique" else MonachismeV <- ""
    if (input$Hospitalieretmilitaire == TRUE) HospitalierV <- "Hospitaliers et militaires" else HospitalierV <- ""
    if (input$Mendiants == TRUE) MendiantsV <- "Mendiants" else MendiantsV <- ""
    if (input$Clercsrégulier == TRUE) ClercsV <- "Clercs réguliers" else ClercsV <- ""
    
    if (input$Coutumiers == TRUE) CoutumiersV <- "Coutumiers" else CoutumiersV <- ""
    if (input$Règles == TRUE) ReglesV <- "Règles" else ReglesV <- ""
    
    if (input$Masculine == TRUE) MasculineV <- "Masculine" else MasculineV <- ""
    if (input$Double == TRUE) DoubleV <- "Double" else DoubleV <- ""
    if (input$Féminine == TRUE) FeminineV <- "Féminine" else FeminineV <- ""
    
    currentlyFiltered <- filter(DATA, modAgreg %in% c(RegulierV,
                                                      SeculierV,
                                                      SeculierCommuV,
                                                      AutreV,
                                                      BenedictinsV,
                                                      ChanoinesregV,
                                                      MonachismeV,
                                                      HospitalierV,
                                                      MendiantsV,
                                                      ClercsV,
                                                      CoutumiersV,
                                                      ReglesV,
                                                      MasculineV,
                                                      DoubleV,
                                                      FeminineV))
    return(currentlyFiltered)
  }
  
  #Affichage legende section affichage ----
  onclick(id = "statToggle", c(toggleClass(id = "statLegend", class = "show")), add = FALSE)
  
  observe({
    toggleClass(id = "statLegend", class = "hide",
                condition = input$conf=="Statuts")
    toggleClass(id = "statLegendBoxes", class = "show",
                condition = input$conf=="Statuts")
  })
  
  onclick(id = "ordrToggle", c(toggleClass(id = "ordrLegend", class = "show")), add = FALSE)
  
  observe({
    toggleClass(id = "ordrLegend", class = "hide",
                condition = input$conf=="Ordres")
    toggleClass(id = "ordrLegendBoxes", class = "show",
                condition = input$conf=="Ordres")
  })
  
  onclick(id = "obsToggle", c(toggleClass(id = "obsLegend", class = "show")), add = FALSE)
  
  observe({
    toggleClass(id = "obsLegend", class = "hide",
                condition = input$conf=="Observance")
    toggleClass(id = "obsLegendBoxes", class = "show",
                condition = input$conf=="Observance")
  })
  
  onclick(id = "commToggle", c(toggleClass(id = "commLegend", class = "show")), add = FALSE)
  
  observe({
    toggleClass(id = "commLegend", class = "hide",
                condition = input$conf=="Type de communauté")
    toggleClass(id = "commLegendBoxes", class = "show",
                condition = input$conf=="Type de communauté")
  })
  
  #filtrer les données par les différents filtres attributaire et temporels ----
  filteredData <- reactive({
    
    T0NewSel <- filter(T0New, modAgreg %in% c(input$Obsc,
                                              input$Ordrc,
                                              input$Statc,
                                              input$Ecolc,
                                              input$Commc,
                                              input$Relc))
    
    idImplSel <- unique(T0NewSel$idimplantation)
    T0NewSel <- filter(T0New, idimplantation %in% idImplSel)
    
    currentlyFiltered <- checkboxFilter(T0NewSel)
    
    currentlyFiltered <- filter(currentlyFiltered, caracNew %in% input$conf)
    
    if(!is.null(input$distribPlot_brush)){
      thisSel <- input$distribPlot_brush
      currentlyFiltered <- currentlyFiltered %>% 
      filter(!(date_stop_max<= thisSel$xmin | date_start_min>=thisSel$xmax))
    }
    
    if(input$etat == "État final"){
      T0NewEtat <- as.data.frame(currentlyFiltered %>%  group_by(idimplantation) %>%  slice(which.max(date_stop_max)))
    }else if(input$etat == "État initial"){
      T0NewEtat <- as.data.frame(currentlyFiltered %>%  group_by(idimplantation) %>%  slice(which.max(date_start_min)))
    }else if(input$etat == "État dominant"){
      T0NewEtat <- as.data.frame(currentlyFiltered %>%  group_by(idimplantation) %>%  slice(which.max(DureeFact)))
    }
    
    return(T0NewEtat)
  })
  
  
  #filtrer les données par les différents filtres attributaire (trsp)----
  filteredTRSPData <- reactive({
    
    currentlyFiltered <- checkboxFilter(T0New)
    
    currentlyFiltered <- filter(currentlyFiltered, caracNew %in% input$conf)
    
    if(!is.null(input$distribPlot_brush)){
      thisSel <- input$distribPlot_brush
      currentlyFiltered <- currentlyFiltered %>% 
        filter((date_stop_max<= thisSel$xmin | date_start_min>=thisSel$xmax))
    }
    
    return(currentlyFiltered)
  })
  
  #filtrer les factoides par fenetre spatiale ----
  filteredSpatialData <- reactive({
    
    currentlyFilteredMap <- filter(T0New, caracNew %in% input$conf)
    
    if(length(input$map_draw_all_features$features) > 0){
      coordSelBox <- unlist(input$map_draw_all_features$features[[1]]$geometry$coordinates)[c(1,2,4,5)]
      currentlyFilteredMap <- currentlyFilteredMap %>% 
        filter(lng >= coordSelBox[1], lng <= coordSelBox[4]) %>% 
        filter(lat >= coordSelBox[2], lat <= coordSelBox[3])
      return(currentlyFilteredMap)
    }
  })
  
  #filtrer les liens par les input ----
  filterLiens <- reactive({
    LiensSel <- filter(Liens, modAgreg == input$Relc)

    idImplSel <- unique(LiensSel$idimplantation)
    LiensFiltre <- filter(Liens, idimplantation %in% idImplSel)
    if(input$etat == "État final"){
      liensCurrentlyFiltered <- LiensFiltre %>%  group_by(idimplantation) %>%  filter(date_stop_max == max(date_stop_max))
    }else if(input$etat == "État initial"){
      liensCurrentlyFiltered <- LiensFiltre %>%  group_by(idimplantation) %>%  filter(date_start_min == min(date_start_min))
    }else if(input$etat == "État dominant"){
      liensCurrentlyFiltered <- LiensFiltre %>%  group_by(idimplantation) %>%  filter(DureeFact == max(DureeFact))
    }
    
    if(!is.null(input$distribPlot_brush)){
      thisSel <- input$distribPlot_brush
      liensCurrentlyFiltered <- liensCurrentlyFiltered %>%
        filter(!(date_stop_max<= thisSel$xmin | date_start_min>=thisSel$xmax))
    }
    return(liensCurrentlyFiltered)
  })


  #filtrer les ecoles par les input ----
  filterEcole <- reactive({
    ecoleSel <- filter(Ecole, modAgreg == input$Ecolc)

    idImplSel <- unique(ecoleSel$idimplantation)
    ecoleFiltre <- filter(Ecole, idimplantation %in% idImplSel)
    if(input$etat == "État final"){
      ecoleCurrentlyFiltered <- ecoleFiltre %>%  group_by(idimplantation) %>%  filter(date_stop_max == max(date_stop_max))
    }else if(input$etat == "État initial"){
      ecoleCurrentlyFiltered <- ecoleFiltre %>%  group_by(idimplantation) %>%  filter(date_start_min == min(date_start_min))
    }else if(input$etat == "État dominant"){
      ecoleCurrentlyFiltered <- ecoleFiltre %>%  group_by(idimplantation) %>%  filter(DureeFact == max(DureeFact))
    }
    
    if(!is.null(input$distribPlot_brush)){
      thisSel <- input$distribPlot_brush
      ecoleCurrentlyFiltered <- ecoleCurrentlyFiltered %>%
        filter(!(date_stop_max<= thisSel$xmin | date_start_min>=thisSel$xmax))
    }
    return(ecoleCurrentlyFiltered)
  })
  
  
  #Sortie map ----
  output$map <- renderLeaflet({
    leaflet()%>%
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Diocèse", "Chefs lieux de Diocèse","hors-selection"),
        options = layersControlOptions(collapsed = T)
      ) %>% 
      addMapPane("PaneFondTerrain", zIndex = 410) %>% 
      addMapPane("PaneFond", zIndex = 420) %>% 
      addMapPane("PaneDiocese", zIndex = 430) %>%  # Level 1
      addMapPane("PaneT0NewBlack", zIndex = 440) %>%  # Level 2
      addMapPane("paneTrsp", zIndex = 450) %>%   # Level 3
      addMapPane("paneLine", zIndex = 460) %>%   # Level 4
      addMapPane("paneEcole", zIndex = 470) %>%   # Level 5
      addMapPane("PaneT0New", zIndex = 480) %>%  # Level 6
      addMapPane("PaneChefsLieux", zIndex = 490) %>%  # Level 7
      hideGroup("hors-selection") %>% 
      hideGroup("Diocèse") %>% 
      hideGroup("Chefs lieux de Diocèse") %>%
      addProviderTiles(providers$CartoDB.Positron, options =  pathOptions(pane = "PaneFond",opacity = 0.7)) %>% 
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
        color = "grey",
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
  
  #Masque le fond de carte "world terrain" arrivé à un certain niveau de zoom
  observeEvent(eventExpr = input$map_zoom, {
      if(input$map_zoom < 9){
          leafletProxy("map", session = session) %>% 
            addProviderTiles(providers$Esri.WorldTerrain, layerId = "terrain", options = pathOptions(pane = "PaneFondTerrain"))
      }else{
        leafletProxy("map", session = session) %>% 
          removeTiles(layerId= "terrain")
      }
    }
  )
  
  #Affichage factoides filtré par le graph ----
  observe({
      mapData <- filteredData()
      labelContent <- lapply(seq(nrow(mapData)), function(i) {
        paste( "<b>Nom :</b>", mapData[i, "usual_name"], "</br>", 
                "<b>Diocèse :</b>",mapData[i, "Diocese"], "</br>", 
               "<b>Modalité :</b>",mapData[i, "modalite"]
                ) 
      })
      mapProxy <- leafletProxy("map", session = session)
      mapProxy %>%
        clearGroup('reactive') %>% 
        addCircleMarkers(
          layerId=~idimplantation,
          data = mapData,
          lat = ~lat,
          lng = ~lng,
          radius = 3,
          color = 'white',
          opacity = 1,
          fillColor = ~modaNiv1_Color,
          stroke = T,
          weight = 1,
          fillOpacity = 1,
          label = lapply(labelContent, htmltools::HTML),
          group = 'reactive',
          options = pathOptions(pane = "PaneT0New") 
        )
  })
  
  #Affichage des données non filtrés par le graph (trsp) ----
  observe({
    if(!is.null(input$distribPlot_brush)){
      trspData <- filteredTRSPData()
      labelContent <- lapply(seq(nrow(trspData)), function(i) {
        paste( "<b>Nom :</b>", trspData[i, "usual_name"], "</br>", 
               "<b>Diocèse :</b>",trspData[i, "Diocese"], "</br>", 
               "<b>Modalité :</b>",trspData[i, "modalite"]
        ) 
      })
      mapProxy <- leafletProxy("map", session = session, data = trspData)
      mapProxy %>%
        clearGroup('trsp')%>%
        addCircleMarkers(
          layerId=~idimplantation,
          lat = ~lat,
          lng = ~lng,
          radius = 3,
          color = 'white',
          opacity = 1,
          fillColor = ~modaNiv1_Color,
          stroke = T,
          weight = 1,
          fillOpacity = 0.1,
          label = lapply(labelContent, htmltools::HTML),
          group = 'trsp',
          options = pathOptions(pane = "paneTrsp") 
        )
    }
  })
  
  #Affichage des liens ----
  observe({
    if(input$RelR==T){
      Liens <- filterLiens()
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
                                    "<strong>Diocèse :</strong>", Diocese),
                     options = pathOptions(pane = "paneLine")
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
      Ecole <- filterEcole()
      labelContent <- lapply(seq(nrow(Ecole)), function(i) {
        paste( "<b>Nom :</b>", Ecole[i, "usual_name"], "</br>", 
               "<b>Diocèse :</b>",Ecole[i, "Diocese"], "</br>", 
               "<b>Modalité :</b>",Ecole[i, "modalite"]
        ) 
      })
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
          label = lapply(labelContent, htmltools::HTML),
          group = 'ecol',
          options = c(popupOptions(interactive = F ),pathOptions(pane = "paneEcole"))
        )
      show(id ='ecoleLegend')
    }else if (input$EcolR==F){
      mapProxy <- leafletProxy("map", session = session)
      mapProxy %>%
        clearGroup('ecol')
      hide(id = 'ecoleLegend')
    }
  })
  
  
  #Sortie graph----
  output$distribPlot <- renderPlot({
    
    T0NewSel <- filter(T0New, modAgreg %in% c(input$Obsc,
                                              input$Ordrc,
                                              input$Statc,
                                              input$Ecolc,
                                              input$Commc,
                                              input$Relc))
    
    idImplSel <- unique(T0NewSel$idimplantation)
    T0NewSel <- filter(T0New, idimplantation %in% idImplSel)
    
    currentlyFiltered <- checkboxFilter(T0NewSel)
    
    # distribPlot <- ggplot(T0New,aes(date_start_min)) +
    #   geom_density(col = colorAttr, fill = colorAttr, alpha = 0.3, adjust = 0.75)
    distribPlot <- graphique_tapis(carac = input$conf, T0New = currentlyFiltered)
    
    
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
    
            nbConf <- filter(T0New, caracNew %in% input$conf)
            
            if(input$etat == "État final"){
              nbEtat <- as.data.frame(nbConf %>%  group_by(idimplantation) %>%  slice(which.max(date_stop_max)))
            }else if(input$etat == "État initial"){
              nbEtat <- as.data.frame(nbConf %>%  group_by(idimplantation) %>%  slice(which.max(date_start_min)))
            }else if(input$etat == "État dominant"){
              nbEtat <- as.data.frame(nbConf %>%  group_by(idimplantation) %>%  slice(which.max(DureeFact)))
            }
    
    
    #Nombre d'implantation affiché----
      output$selAttr <- renderText({ 
        paste("Selection attributaire : ","<font color=\"black\"><b>", nrow(nbEtat), "</b></font>")
      })
      output$selTemp <- renderText({ 
        if(is.null(input$distribPlot_brush)){
          paste("Selection temporelle : <b>0</b>")
        }else{
          paste("Selection temporelle : ","<font color=\"black\"><b>", nrow(filteredData()), "</b></font>")
        }
      })
      output$selSpat <- renderText({ 
        if(is.null(nrow(filteredSpatialData()))){
          paste("Selection spatiale : <b>0</b>")
        }else{
          paste("Selection spatiale : ","<font color=\"black\"><b>", count(filteredSpatialData()), "</b></font>")
        }
      })
      
    return(distribPlot)
  })
  
  
  #Popup Chronogramme ----
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showPopup(event$id, event$lat, event$lng)
    })
  })
  
  
  
})