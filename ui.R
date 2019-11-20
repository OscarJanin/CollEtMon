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
                       .imgLineLegend{
                       height: 5px;
                       width: 30px;
                       }
                       #lineLegend{
                       background-color: lightgrey;
                       padding: 10px;
                       margin: 5px;
                       }
                       #ecoleLegend{
                       background-color: lightgrey;
                       padding: 10px;
                       margin: 5px;
                       }
                       #statLegend{
                       background-color: lightgrey;
                       padding: 10px;
                       margin: 5px;
                       }
                       #ordrLegend{
                       background-color: lightgrey;
                       padding: 10px;
                       margin: 5px;
                       }
                       #obsLegend{
                       background-color: lightgrey;
                       padding: 10px;
                       margin: 5px;
                       }
                       #commLegend{
                       background-color: lightgrey;
                       padding: 10px;
                       margin: 5px;
                       }
                       #statLegendBoxes{
                       background-color: lightgrey;
                       padding: 10px;
                       margin: 5px;
                       }
                       #ordrLegendBoxes{
                       background-color: lightgrey;
                       padding: 10px;
                       margin: 5px;
                       }
                       #obsLegendBoxes{
                       background-color: lightgrey;
                       padding: 10px;
                       margin: 5px;
                       }
                       #commLegendBoxes{
                       background-color: lightgrey;
                       padding: 10px;
                       margin: 5px;
                       }
                       #imgEcole{
                       width: 10px;
                       height: 10px;
                       -webkit-border-radius: 25px;
                       -moz-border-radius: 25px;
                       border-radius: 25px;
                       background: #333333;
                       display: inline-block
                       }
                       .show{
                       display: block 
                       }
                       .hide{
                       display: none !important
                       }
                       #statToggle{
                       fill: orange;
                       cursor:pointer
                       }
                       #ordrToggle{
                       fill: orange;
                       cursor:pointer
                       }
                       #obsToggle{
                       fill: orange;
                       cursor:pointer
                       }
                       #commToggle{
                       fill: orange;
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
                       "
  )),
  
  # Sidebar layout with input and output definitions ----
  fluidRow( 
    
    
    # Sidebar panel for inputs ----
    column(
      width = 2,
      style = "height: 97vh; overflow: auto;background-color:#C2C2C2",
      titlePanel("Filtres"),
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
      )
    ),
    column(
      width = 2,
      style = "height: 97vh; overflow: auto;background-color:#8C8C8C",
      titlePanel("Affichage"),
      HTML('
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
           '
      ),
      checkboxInput("EcolR",tags$b("École")),
      hidden(
        div(id = "ecoleLegend",
            div( id = "imgEcole"),
            p('  École')
        )
      ),
      checkboxInput("RelR",tags$b("Relations")),
      hidden(
        div(id = "lineLegend",
            img( class = "imgLineLegend", src = "ligneViolet.png"),
            p('  A'),
            br(),
            img( class = "imgLineLegend", src = "ligneOrange.png"),
            p('  D'),
            br(),
            img( class = "imgLineLegend", src = "ligneVert.png"),
            p('  H'),
            br(),
            img( class = "imgLineLegend", src = "ligneMarron.png"),
            p('  X')
        )
      )
            ),
    # Main panel for displaying outputs ----
    column(width = 8,
           absolutePanel(bottom = "37%",
                         right = "80%",
                         left = "3%",
                         class = "panel panel-default",
                         style = "padding : 0 0 0 10px;
                                  z-index: 1;",
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
                         right = "3%",
                         left = "80%",
                         class = "panel panel-default",
                         style = "padding : 0 0 0 10px;
                         z-index: 1;",
                         htmlOutput("selAttr"),
                         htmlOutput("selTemp"),
                         htmlOutput("selSpat")),
           leafletOutput("map", height = "60vh"),
           # plotOutput("distribPlot", 
           #            height = "37vh",
           #            brush = brushOpts(id = "distribPlot_brush", direction = "x", resetOnNew = FALSE, fill = colorTempo, stroke = colorTempo)),
           plotOutput("distribPlot", height = "37vh",
                      brush = brushOpts(id = "distribPlot_brush", direction = "x", resetOnNew = FALSE,fill = "black"))
    )
        )
            )
        )

