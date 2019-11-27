source("global.R")

shinyUI(fluidPage(
  useShinyjs(),  # Set up shinyjs
  
  tags$head(tags$style("
                       body {
                       font-family: candara;
                       }
                       .checkbox{
                       margin-top:2px;
                       margin-bottom:2px
                       }
                       .form-group {
                       margin:0px
                       }
                       .panel-body {
                       padding: 5px;
                       }
                       p{
                       display:contents 
                       }

                      #affichage{
                       height: 100vh; 
                       overflow: auto;
                       background-color:#737778;
                       width: 15%;
                       float: left;
                       padding: 0px 10px;
                       overflow-wrap: break-word;
                      }

                       #statLegend{
                       background-color: #ECEDE7;
                       padding: 10px;
                       margin: 5px;
                       }
                       #ordrLegend{
                       background-color: #ECEDE7;
                       padding: 10px;
                       margin: 5px;
                       }
                       #obsLegend{
                       background-color: #ECEDE7;
                       padding: 10px;
                       margin: 5px;
                       }
                       #commLegend{
                       background-color: #ECEDE7;
                       padding: 10px;
                       margin: 5px;
                       }
                       #statLegendBoxes{
                       background-color: #ECEDE7;
                       padding: 10px;
                       margin: 5px;
                       }
                       #ordrLegendBoxes{
                       background-color: #ECEDE7;
                       padding: 10px;
                       margin: 5px;
                       }
                       #obsLegendBoxes{
                       background-color: #ECEDE7;
                       padding: 10px;
                       margin: 5px;
                       }
                       #commLegendBoxes{
                       background-color: #ECEDE7;
                       padding: 10px;
                       margin: 5px;
                       }
                       .show{
                       display: block 
                       }
                       .hide{
                       display: none !important
                       }
                       #statToggle{
                       fill: #FB7E47;
                       cursor:pointer
                       }
                       #ordrToggle{
                       fill: #FB7E47;
                       cursor:pointer
                       }
                       #obsToggle{
                       fill: #FB7E47;
                       cursor:pointer
                       }
                       #commToggle{
                       fill: #FB7E47;
                       cursor:pointer
                       }
                      
                       #statLegendBoxes label{
                       font-weight: 100;
                       margin: 0
                       }
                       #ordrLegendBoxes label{
                       font-weight: 100;
                       margin: 0
                       }
                       #obsLegendBoxes label{
                       font-weight: 100;
                       margin: 0
                       }
                       #commLegendBoxes label{
                       font-weight: 100;
                       margin: 0
                       }
                       input[type=checkbox]{
                       margin: 0
                       }


                       #lineLegend{
                          background-color: #ECEDE7;
                          padding: 10px;
                          margin: 5px;
                       }
                       #ecoleLegend{
                          background-color: #ECEDE7;
                          padding: 10px;
                          margin: 5px;
                       }

                       #filter{
                          height: 100vh; 
                          overflow: auto;
                          background-color:#ECEDE7;
                          direction: rtl; 
                          padding: 0px 10px;
                          width: 2%;
                          float: left;
                          transition:all .5s;
                       }
                       #arrowFilter{
                          padding: 0px;
                          width: 15%;
                          top: 50%;
                          position: absolute;
                          cursor : pointer;
                          transition:all .5s;
                       }

                       #panelEtat{
                          bottom : 39%;
                          right : 72%;
                          left : 18%;
                          padding : 0 10px;
                          z-index: 1;
                          transition:all .5s;
                          overflow-wrap: break-word;
                       }

                       /* CSS concatenate filter*/
                       #filterContent{
                          direction: ltr; 
                          padding: 0px;
                          height: 100vh;
                          overflow-y:hidden;
                          width : 0;
                          float: left;
                          transition:all .5s;
                          overflow-wrap: break-word;
                       }

                       #filter.filterDisplayed #filterContent{
                          width : 85%;
                          overflow-y: visible;
                       }
                       #filter.filterDisplayed {
                          width : 15%
                       }

                       #content.filterDisplayed {
                          width : 70%
                       }

                       #distribPlot img{
                          width : 100%
                       }

                       #panelEtat.filterDisplayed{
                          right : 59%;
                          left : 31%;
                       }

                       #content{
                            width : 82%;
                            float : left;
                            transition:all .5s;
                       }

                       /* Leaflet Widget */
                       .leaflet-draw-draw-rectangle{
                          background-color: #FB7E47 !important
                       }


                       "
  )),
  
  #Fonction concatenate filter
  tags$script('
     $(document).ready(function(){
      $("#arrowFilter").click(function(e){
         e
         $("#filter").toggleClass("filterDisplayed");
         $("#content").toggleClass("filterDisplayed");
         $("#panelEtat").toggleClass("filterDisplayed");
         $("#distribPlot_brush").trigger("resize");
         });
      });
     '),
  
  # Sidebar layout with input and output definitions ----
  fluidRow( 
    
    
    # Sidebar panel for inputs ----
        HTML(
          '
            <div id="filter">
              <div id="filterContent">
                <h2>Filtres</h2>'
          ),
             bsCollapse(multiple = T, 
                        open = c("Statuts","Ordres","Observance","Type de communauté","École","Relations"),
                        bsCollapsePanel("Statuts",
                                        checkboxGroupInput("Statc",
                                                           label = NULL,
                                                           choices = listStat,
                                                           selected = listStat)
                                        , style = "default"),
                        bsCollapsePanel("Ordres",
                                        checkboxGroupInput("Ordrc",
                                                           label = NULL,
                                                           choices = listOrdr,
                                                           selected = listOrdr)
                                        , style = "default"),
                        bsCollapsePanel("Observance",
                                        checkboxGroupInput("Obsc",
                                                           label = NULL,
                                                           choices = listObs,
                                                           selected = listObs)
                                        , style = "default"),
                        bsCollapsePanel("Type de communauté",
                                        checkboxGroupInput("Commc",
                                                           label = NULL,
                                                           choices = listComm,
                                                           selected = listComm)
                                        , style = "default"),
                        bsCollapsePanel("École",
                                        checkboxGroupInput("Ecolc",
                                                           label = NULL,
                                                           choices = listEcol,
                                                           selected = listEcol)
                                        , style = "default"),
                        bsCollapsePanel("Relations",
                                        checkboxGroupInput("Relc",
                                                           label = NULL,
                                                           choices = listRel,
                                                           selected = listRel)
                                        , style = "default")
             
               
            ),
    HTML('</div>
        <div id="arrowFilter">
             <svg height="60" width="10" fill= #FB7E47 >
             <polygon points="10 64, 10 0, 0 32" id="statArrowOn" class style = " display: block;"/>
             </svg>
          </div>
        </div>
          <div id = "affichage">
          <h2>Affichage</h2>
              <div class="attr-col shiny-input-radiogroup" id="conf">
              
                   <input type="radio" id="Statuts" name="conf" value="Statuts" checked= "checked"/>
                   <label for="Statuts">Statuts</label>
                   
                   <svg height="10" width="10">
                        <polygon points="0 0, 10 0, 10 10, 0 10" id="statToggle"/>
                   </svg>
                   
                   <br/>
                   
                   <div id = "statLegend" class style = " display: none;" >
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#a50f15" /></svg>
                        <p>Régulier </p>
                        </br>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#377eb8" /></svg>
                        <p> Séculier </p>
                        </br>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#8da0cb" /></svg>
                        <p> Séculier-communautaire </p>
                        </br>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#999999" /></svg>
                        <p> Autres </p>
                   </div>
                   
                   <div id = "statLegendBoxes" class style = " display: none;" >
                        <input type="checkbox" id="Régulier" name="Régulier" checked>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#a50f15" /></svg> 
                        <label for="Régulier">Régulier</label>
                        </br>
                        <input type="checkbox" id="Séculier" name="Séculier" checked>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#377eb8" /></svg>
                        <label for="Séculier">Séculier</label>
                        </br>
                        <input type="checkbox" id="Séculiercommunautaire" name="Séculiercommunautaire" checked>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#8da0cb" /></svg>
                        <label for="Séculiercommunautaire">Séculier-communautaire</label>
                        </br>
                        <input type="checkbox" id="Autres" name="Autres" checked>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#999999" /></svg>
                        <label for="Autres">Autres</label>
                   </div>
                   
                   <input type="radio" id="Ordres" name="conf" value="Ordres"  />
                   <label for="Ordres">Ordres</label>
                   
                   <svg height="10" width="10">
                        <polygon points="0 0, 10 0, 10 10, 0 10" id="ordrToggle" />
                   </svg>
                   <br/>
                   
                   <div id = "ordrLegend" class style = " display: none;" >
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#ffffb3" /></svg>
                        <p> Bénédictins </p>
                        </br>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#fb8072" /></svg>
                        <p> Chanoines-réguliers </p>
                        </br>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#fdb462" /></svg>
                        <p> Monachisme-érémitique </p>
                        </br>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#80b1d3" /></svg>
                        <p> Hospitalier et militaire </p>
                        </br>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#8dd3c7" /></svg>
                        <p> Mendiants </p>
                        </br>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#bebada" /></svg>
                        <p> Clercs-régulier </p>
                   </div>
                   
                   <div id = "ordrLegendBoxes" class style = " display: none;" >
                         <input type="checkbox" id="Bénédictins" name="Bénédictins"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#ffffb3" /></svg>
                         <label for="Bénédictins">Bénédictins</label>
                         </br>
                         <input type="checkbox" id="Chanoinesréguliers" name="Chanoinesréguliers"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#fb8072" /></svg>
                         <label for="Chanoinesréguliers">Chanoines-réguliers</label>
                         </br>
                         <input type="checkbox" id="Monachismeérémitique" name="Monachismeérémitique"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#fdb462" /></svg>
                         <label for="Monachismeérémitique">Monachisme-érémitique</label>
                         </br>
                         <input type="checkbox" id="Hospitalieretmilitaire" name="Hospitalieretmilitaire"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#80b1d3" /></svg>
                         <label for="Hospitalieretmilitaire">Hospitalier et militaire</label>
                         </br>
                         <input type="checkbox" id="Mendiants" name="Mendiants"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#8dd3c7" /></svg>
                         <label for="Mendiants">Mendiants</label>
                         </br>
                         <input type="checkbox" id="Clercsrégulier" name="Clercsrégulier"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#bebada" /></svg>
                         <label for="Clercsrégulier">Clercs-régulier</label>
                   </div>
                   
                   
                   <input type="radio" id="Observance" name="conf" value="Observance" />
                   <label for="Observance">Observance</label>
                   <svg height="10" width="10">
                         <polygon points="0 0, 10 0, 10 10, 0 10" id="obsToggle" />
                   </svg>
                   <br/>
                   
                   <div id = "obsLegend" class style = " display: none;" >
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#883799" /></svg>
                         <p> Coutumiers </p>
                         </br>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#7A3B1A" /></svg>
                         <p> Règles </p>
                   </div>
                   
                   <div id = "obsLegendBoxes" class style = " display: none;" >
                         <input type="checkbox" id="Coutumiers" name="Coutumiers"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#883799" /></svg>
                         <label for="Coutumiers">Coutumiers</label>
                         </br>
                         <input type="checkbox" id="Règles" name="Règles"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="#7A3B1A" /></svg>
                         <label for="Règles">Règles</label>
                   </div>
                   
                   <input type="radio" id="Type de communauté" name="conf" value="Type de communauté" />
                   <label for="Type de communauté">Type de communauté</label>
                   <svg height="10" width="10">
                         <polygon points="0 0, 10 0, 10 10, 0 10" id="commToggle" />
                   </svg>
                   </br>
                   
                   <div id = "commLegend" class style = " display: none;" >
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="turquoise" /></svg>
                         <p> Masculine </p>
                         </br>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="thistle" /></svg>
                         <p> Double </p>
                         </br>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="violet" /></svg>
                         <p> Féminine </p>
                   </div>
                   
                   <div id = "commLegendBoxes" class style = " display: none;" >
                         <input type="checkbox" id="Masculine" name="Masculine"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="turquoise" /></svg>
                         <label for="Masculine">Masculine</label>
                         </br>
                         <input type="checkbox" id="Double" name="Double"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="thistle" /></svg>
                         <label for="Double">Double</label>
                         </br>
                         <input type="checkbox" id="Féminine" name="Féminine"checked>
                         <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="violet" /></svg>
                         <label for="Féminine">Féminine</label>
                   </div>
              
              </div>
              
              <input type="checkbox" id="EcolR" name="École" >
              <b>École</b>
              <div id = "ecoleLegend" class style = " display: none;"  >
                   <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="grey" /></svg>
                   <p>École</p>
              </div> 
              </br>
              <input type="checkbox" id="RelR" name="Relations" >
              <b>Relations</b>
              <div id = "lineLegend" class style = " display: none;"  >
                    <svg height="10" width="30"><line x1="0" y1="0" x2="30" y2="0" style="stroke:rgb(137,118,181);stroke-width:12" /></svg>
                    <p>A</p>
                    </br>
                    <svg height="10" width="30"><line x1="0" y1="0" x2="30" y2="0" style="stroke:rgb(207, 101, 41);stroke-width:12" /></svg>
                    <p>D</p>
                    </br>
                    <svg height="10" width="30"><line x1="0" y1="0" x2="30" y2="0" style="stroke:rgb(92, 168, 102);stroke-width:12" /></svg>
                    <p>H</p>
                    </br>
                    <svg height="10" width="30"><line x1="0" y1="0" x2="30" y2="0" style="stroke:rgb(105, 88, 62);stroke-width:12" /></svg>
                    <p>X</p>
              </div>
         </div>
     '
    ),
    # Main panel for displaying outputs ----
    HTML('<div id= "content">'),
           absolutePanel(id = "panelEtat",
                         class = "panel panel-default",
                         radioButtons("etat","",
                                      choiceNames = list(
                                        HTML("<b>État final</b>"),
                                        HTML("<b>État initial</b>"),
                                        HTML("<b>État dominant</b>")
                                      ),
                                      choiceValues = list(
                                        "État final",
                                        "État initial",
                                        "État dominant"))),
           absolutePanel(top = "1%",
                         right = "1%",
                         left = "87%",
                         class = "panel panel-default",
                         style = "padding : 0 10px; overflow-wrap: break-word;
                         z-index: 1;",
                         htmlOutput("selAttr"),
                         htmlOutput("selTemp"),
                         htmlOutput("selSpat")),
           leafletOutput("map", height = "60vh", width = "100%"),
           # plotOutput("distribPlot", 
           #            height = "37vh",
           #            brush = brushOpts(id = "distribPlot_brush", direction = "x", resetOnNew = FALSE, fill = colorTempo, stroke = colorTempo)),
           plotOutput("distribPlot", height = "40vh", width = "100%",
                      brush = brushOpts(id = "distribPlot_brush", direction = "x", resetOnNew = FALSE,fill = "black")),
                  # HTML('<div id="distribPlot" class="shiny-plot-output"
                  #       style="width: 100%; height: 40vh"></div>'),
        HTML('</div>')
              )
            )
        )

