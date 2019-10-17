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
                       #imgEcole{
                          width: 10px;
                          height: 10px;
                          -webkit-border-radius: 25px;
                          -moz-border-radius: 25px;
                          border-radius: 25px;
                          background: #333333;
                          display: inline-block
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
      radioButtons("conf","",
                   c("Statuts"="Statuts",
                     "Ordres"="Ordres",
                     "Observance"="Observance",
                     "Type de communauté"="Type de communauté",
                     "Relations"="Relations")),
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
           plotOutput("distribPlot", 
                      height = "37vh",
                      brush = brushOpts(id = "distribPlot_brush", direction = "x", resetOnNew = FALSE, fill = colorTempo, stroke = colorTempo))
    )
  )
  )
)



