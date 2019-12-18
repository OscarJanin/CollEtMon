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
                       width: 22%;
                       float: left;
                       padding: 0px 10px;
                       overflow-wrap: break-word;
                      }

                       .Legend{
                       background-color: #ECEDE7;
                       padding: 10px;
                       margin: 5px;
                       }
                       
                       .LegendBoxes{
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
                       .flex{
                       display: flex !important
                       }

                       .Toggle{
                       fill: #FB7E47;
                       cursor:pointer
                       }
                      
                       .LegendBoxes label{
                       font-weight: 100;
                       margin: 0
                       }
                       input[type=checkbox]{
                       margin: 0
                       }


                       .Legend{
                          background-color: #ECEDE7;
                          padding: 5px;
                          margin: 0px;
                          max-width: 75%;
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
                          right: 64%;
                          left: 25%;
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
                              width: 20%;
                       }

                       #content.filterDisplayed {
                          width: 58%;
                       }

                       #distribPlot img{
                          width : 100%
                       }

                       #panelEtat.filterDisplayed{
                          right: 46%;
                          left: 43%;
                       }

                       #content{
                            width : 75%;
                            float : left;
                            transition:all .5s;
                       }

                       /* Leaflet Widget */
                       .leaflet-draw-draw-rectangle{
                          background-color: #FB7E47 !important
                       }

                       hr{
                         margin-top: 7px;
                         margin-bottom: 7px;
                         border: 0;
                         border-top: 1px solid #5f5f5f;
                       }

                       .radio {
                          position: relative;
                          display: block;
                          margin-top: 0px;
                          margin-bottom: 5px;
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
    HTML('
        <div id="filter">
          <div id="filterContent">
            <h2>Filtres</h2>
    '),
    uiOutput("filterContentUI"),

    HTML('
          </div>
          <div id="arrowFilter">
             <svg height="60" width="10" fill= #FB7E47 >
             <polygon points="10 64, 10 0, 0 32" id="statArrowOn" class style = " display: block;"/>
             </svg>
          </div>
        </div>

<!--2nd panel for filtering display-->
        <div id = "affichage">
          <h2>Affichage</h2>
          <div class="attr-col shiny-input-radiogroup" id="conf">
                    '),
    uiOutput("affichageUI"),
    
        HTML('
             </div> 
             <hr>
             <input type="checkbox" id="EcolR" class="Legend" name="École" >
             <b>École</b>
             <div id = "ecoleLegend" class = "Legend" style = " display: none;"  >
                <svg height="10" width="10"><circle cx="5" cy="5" r="5" fill="grey" /></svg>
                <p>École</p>
             </div> 
             <hr>
              <input type="checkbox" id="RelR" name="Relations" >
              <b>Relations</b>
              <div id = "lineLegend" class="Legend" style = " display: none;"  >
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
             <hr>
             <input type="checkbox" id="cercles" name="Cercles proportionnels" >
             <b>Cercles proportionnels</b>
             <div id="prop" class="form-group shiny-input-radiogroup shiny-input-container Legend" style = " display: none;">
                <label class="control-label" for="prop"></label>
                <div class="shiny-options-group">
                    <div class="radio">
                        <label>
                            <input type="radio" name="prop" value="turb" checked="checked"/>
                            <span>Turbulence</span>
                        </label>
                    </div>
                    <div id ="legendTurb" style="display: none;" > 
                        <div style= "display: grid; margin: 0px 10px;">
                        <p>6<p>
                        <svg height="35" width="35"><circle cx="15" cy="15" r="10" stroke="black" stroke-width="1" fill="none"></circle></svg>
                        </div>
                        <div style= "display: grid; margin: 0px 10px;">
                        <p>4<p>
                        <svg height="25" width="25"><circle cx="12" cy="12" r="8" stroke="black" stroke-width="1" fill="none"></circle></svg>
                        </div>
                        <div style= "display: grid; margin: 0px 10px;">
                        <p>2<p>
                        <svg height="20" width="20"><circle cx="10" cy="10" r="6" stroke="black" stroke-width="1" fill="none"></circle></svg>
                        </div>
                        <div style= "display: grid; margin: 0px 10px;">
                        <p>1<p>
                        <svg height="15" width="15"><circle cx="7" cy="7" r="5" stroke="black" stroke-width="1" fill="none"></circle></svg>
                        </div>
                    </div>
                    <div class="radio">
                        <label>
                            <input type="radio" name="prop" value="repr"/>
                            <span>Représentativité de l état dominant</span>
                        </label>
                    </div>
                    <div id ="legendRpr" style="display: none;" > 
                        <div style= "display: grid; margin: 0px 10px;">
                        <p>100%<p>
                        <svg height="35" width="35"><circle cx="15" cy="15" r="10" stroke="black" stroke-width="1" fill="none"></circle></svg>
                        </div>
                        <div style= "display: grid; margin: 0px 10px;">
                        <p>75%<p>
                        <svg height="25" width="25"><circle cx="10" cy="10" r="7" stroke="black" stroke-width="1" fill="none"></circle></svg>
                        </div>
                        <div style= "display: grid; margin: 0px 10px;">
                        <p>50%<p>
                        <svg height="15" width="15"><circle cx="7" cy="7" r="5" stroke="black" stroke-width="1" fill="none"></circle></svg>
                        </div>
                        <div style= "display: grid; margin: 0px 10px;">
                        <p>25%<p>
                        <svg height="10" width="10"><circle cx="5" cy="5" r="2.5" stroke="black" stroke-width="1" fill="none"></circle></svg>
                        </div>
                    </div>
                </div>
             </div>
         </div>

<!--Main panel for displaying outputs-->

         <div id= "content">
              <div style="position:absolute;cursor:inherit;" id="panelEtat" class="panel panel-default">
                <div id="etat" class="form-group shiny-input-radiogroup shiny-input-container">
                  <label class="control-label" for="etat"></label>
                  <div class="shiny-options-group">
                    <div class="radio">
                      <label>
                        <input type="radio" name="etat" value="État final" checked="checked"/>
                        <span><b>État final</b></span>
                      </label>
                    </div>
                    <div class="radio">
                      <label>
                        <input type="radio" name="etat" value="État initial"/>
                        <span><b>État initial</b></span>
                      </label>
                    </div>
                    <div class="radio">
                      <label>
                        <input type="radio" name="etat" value="État le plus long"/>
                        <span><b>État le plus long</b></span>
                      </label>
                    </div>
                    <div class="radio">
                      <label>
                        <input type="radio" name="etat" value="État majoritaire"/>
                        <span><b>État majoritaire</b></span>
                      </label>
                    </div>
                  </div>
                </div>
              </div>
              <div class="panel panel-default" style="top:1%;left:87%;right:1%;position:absolute;cursor:inherit; padding : 0 10px; overflow-wrap: break-word;&#10;                         z-index: 1;">
                <div id="selAttr" class="shiny-html-output"></div>
                <div id="selTemp" class="shiny-html-output"></div>
                <div id="selSpat" class="shiny-html-output"></div>
              </div>
             '),
        leafletOutput("map", height = "60vh", width = "100%"),
        HTML('
              <div id="distribPlot" 
                   class="shiny-plot-output" 
                   style="width: 100% ; height: 36vh" 
                   data-brush-id="distribPlot_brush" 
                   data-brush-fill="black" 
                   data-brush-stroke="#036" 
                   data-brush-opacity="0.25" 
                   data-brush-delay="300" 
                   data-brush-delay-type="debounce" 
                   data-brush-clip="TRUE" 
                   data-brush-direction="x" 
                   data-brush-reset-on-new="FALSE">
              </div>
              <input id="dateDebut" type="text" value="" />
              <input id="dateFin" type="text" value="" />
             </div>')
              )
            )
        )

