library(shiny)
library(plotly)
library(leaflet)
library(shinydashboard)
library(shinyWidgets)

mobileDetect <- function(inputId, value = 0) {
  tagList(
    singleton(tags$head(tags$script(src = "js/mobile.js"))),
    tags$input(id = inputId,
               class = "mobile-element",
               type = "hidden")
  )
}

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    mobileDetect('isMobile'),
    ### For Style and Format
    
    tags$head(includeHTML(("google-analytics.html"))),
    
    tags$head(
        HTML('<meta property="og:title" content="Simulaciones 2da Vuelta Presidencial 2021">
    <meta property="og:image" content = "https://raw.githubusercontent.com/maibennett/Presidenciales2daVuelta/main/images/chart.png">
    <meta property="og:description" content="Visualizacion y Simulaciones para la 2do Vuelta Presidencial 2021">')),
    
    tags$head(tags$link(rel="shortcut icon", href="https://raw.githubusercontent.com/maibennett/Presidenciales2daVuelta/main/images/chart.png")),
    
    tags$head(HTML('<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css">')),
    
    tags$head(
        
        tags$style("
      .myFooter{height:80px;}
      .mySpace{height:10px;
                margin-top: 20px}
      .mySummary{height:70px;
                margin-bottom:20px;}
      .myBarChart{min-height:400px;}
      .myLineChart{height:600px;}
      p.big {line-height: 1.8;
              color: #6D6F73}
      p.medium {line-height: 1.3;
                color: #6D6F73}
      ul.big {line-height: 1.3;
            color: #6D6F73}
      ul.big2 {line-height: 1.5;
            color: #6D6F73}
      p.note {
               color: #6D6F73;
               font-size: 12px;
               font-family: 'Roboto Condensed', sans-serif;
               font-weight: 100;
               margin-bottom: 10px
      }
    .center {
      display: block;
      margin-left: auto;
      margin-right: auto;
      width: 50%;
    }"
        ),
        
        tags$style("#spanish{
                  font-size: 22px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 400;
                  }"),
        
        tags$style("#tab{color: #6D6F73;
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 300;
                  }"),
        
        tags$style("#tab1{color: #6D6F73;
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 300;
                  }"),
        
        tags$style("#tab2{color: #6D6F73;
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 300;
                  }"),
        
        tags$style("#tab3{color: #6D6F73;
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 300;
                  }"),
        
        tags$style("#tab4{color: #6D6F73;
                  font-size: 25px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 500;
                  }"),
        
        tags$style("#note{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#note2{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#note3{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update{color: #FFFFFF;
                  font-size: 30px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#update1{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update2{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update3{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update4{color: #6D6F73;
                  font-size: 12px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 100;
                  }"),
        
        tags$style("#update5{color: #6D6F73;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 600;
                  }"),
        
        tags$style("#TotalCases{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        tags$style("#NewCases{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#NewDeaths{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#TotalDeaths{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#TotalRecovered{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#NewTests{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#TotalHosp{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style("#NewICU{color: #FFFFFF;
                  font-size: 40px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 700;
                  }"),
        
        tags$style(HTML("

  
                .box.box-solid.box-primary {
                    color:#ffffff;
                    border: 2px solid #ffffff
                }
                
                .box.box-solid.box-info {
                    color:#ffffff;
                    border: 2px solid #ffffff
                }
                
                .box.box-solid.box-success {
                    color:#ffffff;
                    border: 2px solid #ffffff
                }
                
                .box.box-solid.box-primary>.box-header {
                    color:#ffffff;
                    background:#ee204d;
                    text-align: center;

                }
  
                .box.box-solid.box-primary>.box-body{
                    color:#ffffff;
                    background:#fe7d84;
                    border: 5px solid #ee204d
                }
                
                .box.box-solid.box-info>.box-header {
                    color:#ffffff;
                    background:#4198b6;
                    text-align: center;

                }
                .box.box-solid.box-info>.box-body{
                    color:#ffffff;
                    background:#6ab2ca;
                    border: 5px solid #4198b6
                }
                
                .box.box-solid.box-success>.box-header {
                    color:#ffffff;
                    background:#bcd25b;
                    text-align: center;

                }
                .box.box-solid.box-success>.box-body{
                    color:#ffffff;
                    background:#c9df67;
                    border: 5px solid #bcd25b
                }

                ")),
        
        tags$style(HTML("
      #separator {
          border-top: 5px dashed #6D6F73;
      }
    ")),
        
        tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@700&display=swap');
      
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@300&display=swap');
      
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@500&display=swap');
      
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@400&display=swap');
      
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@100&display=swap');
      
      @import url('https://fonts.googleapis.com/css?family=Fira+Code:700&display=swap');
      
      h1 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 700;
        font-size: 30px;
      };
      
      h2 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 600;
        font-size: 25px;
      };
      
      #total{color: #6D6F73;
                    font-size: 20px;
                    font-family: 'Roboto Condensed', sans-serif;
        font-weight: 700;
                    };
              
      h3 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;
        font-size: 17px;
      };
      
      h4 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 400;
        font-size: 16px;
      };
      
      h5 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 300;
        font-size: 14px;
      };
      
      h6 {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 100;
        font-size: 12px;
      };
      
      ol {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;
      };
      
      ul {
        font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;
      }
      
    "))
    ),
    ### Actual setup starts here:
    
    useShinydashboard(),
    
    titlePanel(h1("2da Vuelta Presidencial Chile 2021"),
               windowTitle = "2da Vuelta Presidencial Chile 2021"),
    
    navbarPage(h4("Menu",
                  style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 300;"),id="panels",
               
               tabPanel(h4("Simulaciones a Nivel País",
                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 300;"),value="residence",
              
                        fluidRow(column(12,HTML("<p>","Autora: Magdalena Bennett "," &nbsp","<a href='https://magdalenabennett.com'><i class='fa fa-desktop' role='presentation'></i></a>&nbsp",
                                                   "<a href='https://twitter.com/maibennett'><i class='fa fa-twitter' role='presentation'></i></a>&nbsp",
                                                "<a href='https://github.com/maibennett'><i class='fa fa-github' role='presentation'></i></a>&nbsp","</p>"),
                                           style = "font-family: 'Roboto Condensed', sans-serif; font-weight: bold;")),
              
                                  
              fluidRow(column(12,
                                        h4(HTML("<p class='big'>Simulaciones para la elección Presidencial 2021 en base a distintos parámetros:<br>
                          <ul class='big'><li> Escenarios según participación y distribución de votos.</li>
                          <li> Usuario incorpora sus predicciones en términos de abstención y distribución de votantes de ex-candidatos</li></ul><br></p>")
                                        ),tags$head(tags$style("h4{
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 200;
                  }")))),
              
              
              fluidRow(column(4,""),
                       column(4,box(title=span(icon("check"),HTML("<b>","Resultados Simulaciones","</b>"),
                                               style = "font-size: 30px; color: #ffffff"),
                                    solidHeader = TRUE,
                                    collapsible = FALSE,uiOutput("update"), width = NULL,
                                    status = "info")),
                       column(4,"")),

              
              fluidRow(column(12,
                              h2("Simulación 2da vuelta 2021",
                                 style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),          
              fluidRow(column(12,div(plotlyOutput("results_all",width="100%", height = "100%"), align = "center"))
              ),
              fluidRow(column(12, HTML(paste0("<p class='note'>","Nota: Resultados basados en la primera vuelta y 
                                                        parametros entregados por el usuario","</p>")))),
              
              fluidRow(column(12,
                              h2("Parámetros",
                                 style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
              
                        fluidRow(column(12,
                                        h3("Participación General:",
                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        
                        fluidRow(
                          column(2,""),
                            column(
                                width=3, 
                                sliderInput(
                                    "part_abs", label=h5("Personas que NO votaron en 1era vuelta y votarán en 2da vuelta:",
                                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 5, min=0, max=100, step = 0.2,width="90%", post="%")),
                            
                            column(width=1, h5("")),
                            
                            column(
                              width=3, 
                              sliderInput(
                                "div_part_abs", label=h5("Entre estos nuevos participantes, ¿cómo se dividen los votos? (% votos a Boric, el resto va a Kast)",
                                                       style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 60, min=0, max=100, step = 1,width="90%", post="%")),
              column(3,"")
                        ),
                        
                        fluidRow(column(12,
                                        h2("",
                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        
                        fluidRow(column(12,
                                        h3("Participacion por Ex-Candidato",
                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        
                        fluidRow(column(12,
                                        h4("Parisi:",
                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        
                        fluidRow(
                          column(2,""),
                          column(
                            width=3, 
                            sliderInput(
                              "abst_parisi", label=h5("Abstención Votantes Parisi:",
                                                     style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 50, min=0, max=100, step = 1,width="90%", post="%")),
                          
                          column(width=1, h5("")),
                          
                          column(
                            width=3, 
                            sliderInput(
                              "div_part_parisi", label=h5("De los votantes de Parisi que participen, ¿cómo se dividen los votos? (% votos a Boric, el resto va a Kast)",
                                                         style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 30, min=0, max=100, step = 1,width="90%", post="%")),
    column(3,"")
                        ),
                        
                        fluidRow(column(12,
                                        h4("Provoste:",
                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        
                        fluidRow(
                          column(2,""),
                          column(
                            width=3, 
                            sliderInput(
                              "abst_provoste", label=h5("Abstención Votantes Provoste:",
                                                      style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 5, min=0, max=100, step = 1,width="90%", post="%")),
                          
                          column(width=1, h5("")),
                          
                          column(
                            width=3, 
                            sliderInput(
                              "div_part_provoste", label=h5("De los votantes de Provoste que participen, ¿cómo se dividen los votos? (% votos a Boric, el resto va a Kast)",
                                                          style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 90, min=0, max=100, step = 1,width="90%", post="%")),
                          column(3,"")
                        ),
                        
                        fluidRow(column(12,
                                        h4("Sichel:",
                                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        
                        fluidRow(
                          column(2,""),
                          column(
                            width=3, 
                            sliderInput(
                              "abst_sichel", label=h5("Abstención Votantes Sichel:",
                                                      style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 5, min=0, max=100, step = 1,width="90%", post="%")),
                          
                          column(width=1, h5("")),
                          
                          column(
                            width=3, 
                            sliderInput(
                              "div_part_sichel", label=h5("De los votantes de Sichel que participen, ¿cómo se dividen los votos? (% votos a Boric, el resto va a Kast)",
                                                          style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 10, min=0, max=100, step = 1,width="90%", post="%")),
                          column(3,"")
                        ),
    column(width=1, h5("")),
    
    fluidRow(column(12,
                    h4("Artés:",
                       style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
    
                        fluidRow(
                          column(2,""),
                          column(
                            width=3, 
                            sliderInput(
                              "abst_artes", label=h5("Abstención Votantes Artés:",
                                                      style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 0, min=0, max=100, step = 1,width="90%", post="%")),
                          
                          column(width=1, h5("")),
                          
                          column(
                            width=3, 
                            sliderInput(
                              "div_part_artes", label=h5("De los votantes de Artes que participen, ¿cómo se dividen los votos? (% votos a Boric, el resto va a Kast))",
                                                          style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 100, min=0, max=100, step = 1,width="90%", post="%")),
                          column(3,"")
                        ),
                        
    column(width=1, h5("")),
    
    fluidRow(column(12,
                    h4("MEO:",
                       style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        
                        fluidRow(
                          column(2,""),
                          column(
                            width=3, 
                            sliderInput(
                              "abst_meo", label=h5("Abstención Votantes MEO:",
                                                      style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 10, min=0, max=100, step = 1,width="90%", post="%")),
                          
                          column(width=1, h5("")),
                          
                          column(
                            width=3, 
                            sliderInput(
                              "div_part_meo", label=h5("De los votantes de MEO que participen, ¿cómo se dividen los votos? (% votos a Boric, el resto va a Kast)",
                                                          style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), value = 70, min=0, max=100, step = 1,width="90%", post="%")),
                          column(3,""),
                        ),
    fluidRow(column(12, h5(HTML(paste0("<p>","</p>")))))
                        
               ),
               
               
    tabPanel(h4("Análisis",
                style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 300;"),value="analisis",
             fluidRow(column(12,h3(HTML("<b>Análisis de resultados</b>"),
                                   style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
             fluidRow(column(12,
                             h4(HTML("<p class='big'>En esta sección realizaré diferentes análisis con respecto a los datos. 
                             Esta sección se irá complentando a medida que obtenga nuevos análisis.</p>")
                             ),tags$head(tags$style("h4{
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 200;
                  }")))),
             
             fluidRow(column(12,h3(HTML("<b>Dónde ir a buscar votos</b>"),
                                   style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
             
             fluidRow(column(12,
                             h4(HTML("<p class='big'>", "Una pregunta importante para la 2da vuelta es si nuevos votantes se incorporarán o no, y si lo hacen, dónde le conviene a la campaña de Gabriel Boric ir a buscarlos.
                                     Los siguientes mapas muestran dónde podría convenir (a nivel de región) focalizar los esfuerzos, comparando la diferencia entre votos del plebiscito 
                                     (nuestro benchmark o referencia) y los votos de primera vuelta.","</p>")
                             ),tags$head(tags$style("h4{
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 200;
                  }")))),
             
             fluidRow(
               column(
                 width=4,selectizeInput(
                   "region", label=h5("Seleccione región:",
                                        style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"), choices=c("XV - Arica y Parinacota","I - Tarapaca","II - Antofagasta","III - Atacama","IV - Coquimbo",
                                       "V - Valparaiso","XIII - Metropolitana","VI - Libertador General Bernardo O'Higgins","VII - Maule",
                                       "XVI - Ñuble", "VIII - Biobio", "IX - Araucania","XIV - Los Rios","X - Los Lagos", 
                                       "XI - Aysen", "XII - Magallanes"), 
                   selected="XIII - Metropolitana", width="90%"))
             ),
             fluidRow(column(2,""),column(8,leafletOutput("map_votes",width="100%")), column(2,"")
             ),
    ),
    
               tabPanel(h4("Supuestos",
                           style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 300;"),value="supuestos",
                        fluidRow(column(12,h3(HTML("<b>Supuestos para simulación de resultados</b>"),
                                              style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        fluidRow(column(12,
                                        h4(HTML("<p class='big'>Acá se describen los supuestos para los distintos parámetros:</b>:<br>
                        <ul class='big'><li>Se asume que todos los votantes de los dos candidatos en carrera votaran en 2da vuelta por el mismo candidato</li>
                        <li>Los parámetros de abstención y distribución de votantes son determinados por el usuario</li>
                        <li>Los parámetros iniciales representan un escenario posible, pero deben ser ajustados según nueva información</li></ul><br>
                        </p>")
                                        ),tags$head(tags$style("h4{
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 200;
                  }")))),
                        
                        fluidRow(column(12,h3(HTML("<b>Próximamente</b>"),
                                              style = "font-family: 'Roboto Condensed', sans-serif;
        font-weight: 500;"))),
                        
                        fluidRow(column(12,
                                        h4(HTML("<ul class='big'><li>", "Simulaciones a nivel de región para analizar cómo se comportaron las distintas zonas del pais y ver dónde hay mas espacio para crecer.","</li></ul>")
                                        ),tags$head(tags$style("h4{
                  font-size: 20px;
                  font-family: 'Roboto Condensed', sans-serif;
                  font-weight: 200;
                  }")))),
                        
                        fluidRow(column(12, h5(HTML("<p>", "Esta aplicación fue construida por", 
                                                    "<a href='http://magdalenabennett.com'>","Magdalena Bennett", "</a>", 
                                                    ". Para cualquier comentario, puedes ponerte en contacto usando la información en 
                                                    <a href='http://magdalenabennett.com/contact'>esta página</a>","</p>"),
                                               style = "font-family: 'Roboto Condensed', sans-serif;")))
               )
    ) 
))



