source("global.R")

shinyServer(function(input, output, session) {
  
  output$filterContentUI <- renderUI({
    
    lapply(1:length(unique(donnee[[carac]])),function(i){
      
      checkboxGroupInput(substr(unique(donnee[[carac]]),0,4)[i],
                         label = unique(donnee[[carac]])[i],
                         choices = unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]]),
                         selected = unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]]))
    })
    
    
  })
  
  
  output$affichageUI <- renderUI({
    
    lapply(1:length(unique(donnee[[carac]])),function(i){
      
      
      if(i == 1){checked = "checked"} else{checked = ""}
      
      HTML(paste0('<input type="radio" id="',unique(donnee[[carac]])[i],'" name="conf" value="',unique(donnee[[carac]])[i],'"/>
                  <label for="',unique(donnee[[carac]])[i],'">',unique(donnee[[carac]])[i],'</label>
                  
                  <svg height="10" width="10">
                  <polygon points="0 0, 10 0, 10 10, 0 10" id="',substr(unique(donnee[[carac]]),0,4)[i],'Toggle" class = "Toggle"/>
                  </svg>
                  
                  <br/>
                  <div id = "',substr(unique(donnee[[carac]]),0,4)[i],'Legend" class = "Legend" style = " display: none;" >
                  ',paste(unlist(lapply(1:length(unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]])),function(j){
                    
                    HTML(paste0('<svg height="10" width="10"><circle cx="5" cy="5" r="5" fill=',na.omit(unique(donnee[donnee[[modalite]] == unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]])[j],][[color]]))[1],' /></svg>
                                <p>',unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]])[j],'</p>
                                </br>'))
                    
    })), collapse=''),'
        </div>
    
    <div id = "',substr(unique(donnee[[carac]]),0,4)[i],'LegendBoxes" class = "LegendBoxes" style = " display: none;" >',
    paste(unlist(lapply(1:length(unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]])),function(j){
      
      HTML(paste0('<input type="checkbox" id="',gsub(" ", "",(unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]])[j]), fixed = TRUE),'" name="',unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]])[j],'" checked>
                  <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill=',na.omit(unique(donnee[donnee[[modalite]] == unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]])[j],][[color]]))[1],' /></svg>
                  <label for="',unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]])[j],'">',unique(donnee[donnee[[carac]] == unique(donnee[[carac]])[i],][[modalite]])[j],'</label>
                  </br>'))
      
    })), collapse=''),'
        </div>'))
  })
    
    
    
    
  })
  
  # output$output <-renderUI({
  #   paste(leafletOutput("map", height = "60vh", width = "100%"),
  #   HTML('
  #        <div id="distribPlot" 
  #        class="shiny-plot-output" 
  #        style="width: 100% ; height: 40vh" 
  #        data-brush-id="distribPlot_brush" 
  #        data-brush-fill="black" 
  #        data-brush-stroke="#036" 
  #        data-brush-opacity="0.25" 
  #        data-brush-delay="300" 
  #        data-brush-delay-type="debounce" 
  #        data-brush-clip="TRUE" 
  #        data-brush-direction="x" 
  #        data-brush-reset-on-new="FALSE">
  #        </div>'))
  # })
  
  
  # filtrer les données par attributs
  checkboxFilter <- function(DATA){
    
    req(input$conf) # si pas d'input pas de fonction (renderUI)
    
    listeVal <- c()
    
    Valor <-lapply(1:length(unique(donnee[[modalite]])),function(i){
      
      Value <- gsub(" ", "",unique(donnee[[modalite]])[i], fixed=T) 
      if (input[[Value]] == TRUE) Value2 <- unique(donnee[[modalite]])[i] else Value2 <- ""
      listeVal <- c(listeVal,Value2)
      return(listeVal)
    })
    
    currentlyFiltered <- DATA[DATA[[modalite]] %in% unlist(Valor), ]
    # currentlyFiltered <- filter(DATA, modAgreg %in% unlist(Valor))
    # currentlyFiltered <- filter(DATA, modaNiv2 %in% unlist(Valor))
    
    return(currentlyFiltered)
  }
  
  # updateRadioButtons ----
  updateRadioButtons(session, "conf", selected = unique(donnee[[carac]])[1])
  
  
  #Affichage legende section affichage ----
  
  lapply(1:length(unique(donnee[[carac]])),function(i){
    
    onclick(id = paste0(substr(unique(donnee[[carac]]),0,4)[i],"Toggle"), c(toggleClass(id = paste0(substr(unique(donnee[[carac]]),0,4)[i],"Legend"), class = "show")), add = FALSE)
    
    observe({
      toggleClass(id = paste0(substr(unique(donnee[[carac]]),0,4)[i],"Legend"), class = "hide",
                  condition = input$conf==unique(donnee[[carac]])[i])
      toggleClass(id = paste0(substr(unique(donnee[[carac]]),0,4)[i],"LegendBoxes"), class = "show",
                  condition = input$conf==unique(donnee[[carac]])[i])
    })
    
  })
  
  
  substr(unique(donnee[[carac]]),0,4)[1]
  
  #filtrer les données par les différents filtres attributaire et temporels ----
  filteredData <- reactive({
    
    listeFiltre <- c()
    
    Filtre <-lapply(1:length(unique(donnee[[carac]])),function(i){
      
      Value <- substr(unique(donnee[[carac]]),0,4)[i]
      
      listeFiltre <- c(listeFiltre,input[[Value]])
      return(listeFiltre)
    })
    
    T0NewSel <- filter(donnee, modalite %in% unlist(Filtre))
    
    idImplSel <- unique(T0NewSel$idimplantation)
    T0NewSel <- filter(donnee, idimplantation %in% idImplSel)
    
    currentlyFiltered <- checkboxFilter(T0NewSel)
    
    currentlyFiltered <- filter(currentlyFiltered, currentlyFiltered[[carac]] %in% input$conf)
    
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
    
    currentlyFiltered <- checkboxFilter(donnee)
    
    currentlyFiltered <- filter(currentlyFiltered, currentlyFiltered[[carac]] %in% input$conf)
    
    if(!is.null(input$distribPlot_brush)){
      thisSel <- input$distribPlot_brush
      currentlyFiltered <- currentlyFiltered %>% 
        filter((date_stop_max<= thisSel$xmin | date_start_min>=thisSel$xmax))
    }
    
    return(currentlyFiltered)
  })
  
  #filtrer les factoides par fenetre spatiale ----
  filteredSpatialData <- reactive({
    
    currentlyFilteredMap <- filter(donnee, donnee[[carac]] %in% input$conf)
    
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
    # LiensSel <- filter(Liens, modAgreg == input$Rela)
    # 
    # idImplSel <- unique(LiensSel$idimplantation)
    # LiensFiltre <- filter(Liens, idimplantation %in% idImplSel)
    if(input$etat == "État final"){
      liensCurrentlyFiltered <- Liens %>%  group_by(idimplantation) %>%  filter(date_stop_max == max(date_stop_max))
    }else if(input$etat == "État initial"){
      liensCurrentlyFiltered <- Liens %>%  group_by(idimplantation) %>%  filter(date_start_min == min(date_start_min))
    }else if(input$etat == "État dominant"){
      liensCurrentlyFiltered <- Liens %>%  group_by(idimplantation) %>%  filter(DureeFact == max(DureeFact))
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
    # ecoleSel <- filter(Ecole, modAgreg == input$Écol)
    # ecoleSel <- filter(Ecole, Ecole[[modalite]] == input$Écol)
    
    # idImplSel <- unique(ecoleSel$idimplantation)
    # ecoleFiltre <- filter(Ecole, idimplantation %in% idImplSel)
    if(input$etat == "État final"){
      ecoleCurrentlyFiltered <- Ecole %>%  group_by(idimplantation) %>%  filter(date_stop_max == max(date_stop_max))
    }else if(input$etat == "État initial"){
      ecoleCurrentlyFiltered <- Ecole %>%  group_by(idimplantation) %>%  filter(date_start_min == min(date_start_min))
    }else if(input$etat == "État dominant"){
      ecoleCurrentlyFiltered <- Ecole %>%  group_by(idimplantation) %>%  filter(DureeFact == max(DureeFact))
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
        radius = 5,
        color = 'white',
        opacity = 1,
        fillColor = ~mapData[[color]],
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
    # if(!is.null(input$distribPlot_brush)){
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
        radius = 5,
        color = 'white',
        opacity = 1,
        fillColor = ~trspData[[color]],
        stroke = T,
        weight = 1,
        fillOpacity = 0.1,
        label = lapply(labelContent, htmltools::HTML),
        group = 'trsp',
        options = pathOptions(pane = "paneTrsp") 
      )
    # }
  })
  
  #Affichage des liens ----
  observe({
    # req(input$RelR) # si pas d'input pas de fonction (renderUI)
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
    # req(input$EcolR) # si pas d'input pas de fonction (renderUI)
    
    if(input$EcolR==TRUE){
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
          radius = 7,
          color = "#333333",
          stroke = F,
          fillOpacity = 1,
          label = lapply(labelContent, htmltools::HTML),
          group = 'ecol',
          options = c(popupOptions(interactive = F ),pathOptions(pane = "paneEcole"))
        )
      show(id ='ecoleLegend')
    }else if (input$EcolR==FALSE){
      mapProxy <- leafletProxy("map", session = session)
      mapProxy %>%
        clearGroup('ecol')
      hide(id = 'ecoleLegend')
    }
  })
  
  
  #Sortie graph----
  output$distribPlot <- renderPlot({
    
    listeFiltre <- c()
    
    Filtre <-lapply(1:length(unique(donnee[[carac]])),function(i){
      
      Value <- substr(unique(donnee[[carac]]),0,4)[i]
      
      listeFiltre <- c(listeFiltre,input[[Value]])
      return(listeFiltre)
    })
    
    T0NewSel <- donnee[donnee[[modalite]] %in%  unlist(Filtre), ]
    # T0NewSel <- filter(donnee, modAgreg %in% unlist(Filtre))
    # T0NewSel <- filter(donnee, modaNiv1 %in% unlist(Filtre))
    
    idImplSel <- unique(T0NewSel$idimplantation)
    T0NewSel <- filter(donnee, idimplantation %in% idImplSel)
    
    currentlyFiltered <- checkboxFilter(T0NewSel)
    
    # distribPlot <- ggplot(T0New,aes(date_start_min)) +
    #   geom_density(col = colorAttr, fill = colorAttr, alpha = 0.3, adjust = 0.75)
    distribPlot <- graphique_tapis(Pdv = input$conf, donnee = currentlyFiltered)
    
    
    if(!is.null(nrow(filteredSpatialData())) && nrow(filteredSpatialData())> 1){
      
      
      mapSeldistribData <- filteredSpatialData() %>%
        group_by(date_start_min) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      rangeY <- layer_scales(distribPlot)$y$range$range[2]
      
      distribPlot <- distribPlot +
        geom_density(data = mapSeldistribData, aes(date_start_min),col = "#9B372F", fill = "#9B372F", alpha = 0.3, adjust = 0.75)
    }
    if(!is.null(nrow(filteredSpatialData())) && nrow(filteredSpatialData())==1){
      
      
      mapSeldistribData <- filteredSpatialData() %>%
        group_by(date_start_min) %>%
        summarise(Nb = n()) %>%
        mutate(Freq = Nb / sum(Nb))
      
      rangeY <- layer_scales(distribPlot)$y$range$range[2]
      
      distribPlot <- distribPlot +
        geom_col(data = mapSeldistribData, aes(date_start_min, rangeY),fill = "#9B372F", alpha = 0.3, col = "#9B372F", width = 1)
      
    }
    
    nbConf <- filter(donnee, donnee[[carac]] %in% input$conf)
    
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
        paste("Selection temporelle : <b>",nrow(filteredData()),"</b>")
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
  
  #affichage date filtre tempo ----
  observeEvent(input$distribPlot_brush, {
    brush <- input$distribPlot_brush
    if (!is.null(brush)) {
      updateTextInput(session, "dateDebut", value=round(input$distribPlot_brush$xmin))
      updateTextInput(session, "dateFin", value=round(input$distribPlot_brush$xmax))
    }
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