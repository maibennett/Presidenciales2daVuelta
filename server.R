library(dplyr)
library(tidyr)
library(RColorBrewer)
library(firasans)
library("shades")
library("colorspace")
library(htmltools)
library("ggplot2")
library("ggmap")
library(wesanderson)
library(scales)
library(grid)
library(gridExtra)
library(gtable)
library(hrbrthemes)
library(stringr)
library(emo)
library(leaflet)
library(rgdal) 
library(geojson)
library(htmltools)
library(chilemapas)

options(scipen = 99)

hrbrthemes::update_geom_font_defaults(family=font_fsm)

d_mapa <- read.csv("https://raw.githubusercontent.com/maibennett/Presidenciales2daVuelta/main/data/servel_1era_plebiscito_county_clean.csv", encoding = "UTF-8")

d_mapa$nombre_comuna <- str_to_title(d_mapa$comuna_nombre)

d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna," De "," de ")
d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna," Del "," del ")
d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna," La "," la ")
d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna," Las "," las ")

d_mapa$nombre_comuna[d_mapa$nombre_comuna=="Paihuano"] <- "Paiguano"
d_mapa$nombre_comuna[d_mapa$nombre_comuna=="Marchigue"] <- "Marchihue"
d_mapa$nombre_comuna[d_mapa$nombre_comuna=="Trehuaco"] <- "Treguaco"
d_mapa$nombre_comuna[d_mapa$nombre_comuna=="O'higgins"] <- "OHiggins"
d_mapa$nombre_comuna[d_mapa$nombre_comuna=="Aysen"] <- "Aisen"
d_mapa$nombre_comuna[d_mapa$nombre_comuna=="Coyhaique"] <- "Coihaique" 
d_mapa$nombre_comuna[d_mapa$nombre_comuna=="Cabo de Hornos(Ex-Navarino)"] <- "Cabo de Hornos"

d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna,"Ñ","N")
d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna,"ñ","n")
d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna,"á","a")
d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna,"é","e")
d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna,"í","i")
d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna,"ó","o")
d_mapa$nombre_comuna <- str_replace(d_mapa$nombre_comuna,"ú","ú")


mapa_comunas <- mapa_comunas %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  ) %>% 
  left_join(d_mapa)

mapa_comunas <- mapa_comunas %>% filter(nombre_comuna!="Isla de Pascua" & nombre_comuna!="Juan Fernandez")
  
mapa_comunas <- st_sf(mapa_comunas)

# Datos servel y CASEN
servel <- read.csv("https://raw.githubusercontent.com/maibennett/presidenciales_pobreza/main/data/servel_county_clean.csv")
casen <- read.csv("https://raw.githubusercontent.com/maibennett/presidenciales_pobreza/main/data/casen_county_clean.csv")

casen <- casen %>% rename(comuna_nombre = comuna)
servel <- servel %>% mutate(comuna_nombre = str_to_title(comuna_nombre))

d <- left_join(servel, casen, by = "comuna_nombre")
d <- d %>% mutate(Boric = ifelse(candidato=="GABRIEL BORIC FONT", 1, 0),
                  Kast = ifelse(candidato=="JOSE ANTONIO KAST RIST", 1, 0))

# Acortar nombre candidatos
d <- d %>% mutate(candidato_apellido = ifelse(candidato=="GABRIEL BORIC FONT", "Boric",
                                              ifelse(candidato=="JOSE ANTONIO KAST RIST", "Kast",
                                                     ifelse(candidato=="EDUARDO ARTES BRICHETTI", "Artes",
                                                            ifelse(candidato=="FRANCO PARISI FERNANDEZ", "Parisi",
                                                                   ifelse(candidato=="MARCO ENRIQUEZ-OMINAMI GUMUCIO","MEO",
                                                                          ifelse(candidato=="SEBASTIAN SICHEL RAMIREZ", "Sichel",
                                                                                 ifelse(candidato=="YASNA PROVOSTE CAMPILLAY", "Provoste",""))))))))


# Eliminar filas de votos totales, blancos, y nulos
d <- d %>% filter(candidato_apellido!="")

# Pivot wider
d_wide <- pivot_wider(d, id_cols = c(region_nombre, comuna_nombre, comuna_id), names_from = candidato_apellido,
                      values_from = c(total_votos, p_votos, total))


# Datos de la primera vuelta
total_inscritos = 15043531
votos_total = 7115590
p_boric = 1814809/votos_total
p_kast = 1961122/votos_total
p_parisi = 899403/votos_total
p_artes = 103181/votos_total
p_sichel = 898510/votos_total
p_provoste = 815558/votos_total
p_meo = 534485/votos_total


n_boric = 1814809
n_kast = 1961122
n_parisi = 899403
n_artes = 103181
n_sichel = 898510
n_provoste = 815558
n_meo = 534485


server = function(input, output, session) {
 
    updateSliderInput(session, "part_total", value = 5, min = 0, max = 100,
                      step = 0.2)
  
    updateSliderInput(session, "div_part_total", value = 60, min = 0, max = 100,
                    step = 1)
    
    updateSliderInput(session, "abst_parisi", value = 50, min = 0, max = 100,
                      step = 1)
    
    updateSliderInput(session, "div_part_parisi", value = 30, min = 0, max = 100,
                      step = 1)
    
    updateSliderInput(session, "abst_provoste", value = 5, min = 0, max = 100,
                      step = 1)
    
    updateSliderInput(session, "div_part_provoste", value = 90, min = 0, max = 100,
                      step = 1)
    
    updateSliderInput(session, "abst_sichel", value = 5, min = 0, max = 100,
                      step = 1)
    
    updateSliderInput(session, "div_part_sichel", value = 10, min = 0, max = 100,
                      step = 1)
    
    updateSliderInput(session, "abst_artes", value = 0, min = 0, max = 100,
                      step = 1)
    
    updateSliderInput(session, "div_part_artes", value = 100, min = 0, max = 100,
                      step = 1)
    
    updateSliderInput(session, "abst_meo", value = 10, min = 0, max = 100,
                      step = 1)
    
    updateSliderInput(session, "div_part_meo", value = 70, min = 0, max = 100,
                      step = 1)

    
    updateSelectInput(session, "region", choices=c("XV - Arica y Parinacota","I - Tarapaca","II - Antofagasta","III - Atacama","IV - Coquimbo",
                                                   "V - Valparaiso","XIII - Metropolitana","VI - Libertador General Bernardo O'Higgins","VII - Maule",
                                                   "XVI - Ñuble", "VIII - Biobio", "IX - Araucania","XIV - Los Rios","X - Los Lagos", 
                                                   "XI - Aysen", "XII - Magallanes"), 
                      selected="XIII - Metropolitana")
    
    d_new = reactive({
      
      # Numero total de votantes son todos los de Boric y Kast de 1era vuelta +
      # No votantes de 1era vuelta que votaran en 2da +
      # Votantes de cada candidato que no se van a abster
      
      n_total = n_boric + n_kast + input$part_abs/100*total_inscritos + 
        n_parisi - input$abst_parisi*n_parisi/100 + n_sichel - input$abst_sichel*n_sichel/100 + n_provoste - input$abst_provoste*n_provoste/100+ 
        n_artes - input$abst_artes*n_artes/100 + n_meo - input$abst_meo*n_meo/100
      
      part_total = n_total/total_inscritos
        
        boric_parisi = input$div_part_parisi
        boric_sichel = input$div_part_sichel
        boric_provoste = input$div_part_provoste
        boric_artes = input$div_part_artes
        boric_meo = input$div_part_meo
        
        boric_abs = input$div_part_abs
        
        # Boric tiene todos sus votos de 1era vuelta + 
        # los que no participaron en 1era y van por el +
        # los de los ex-candidatos q voten en 2da y vayan por el
        
        perc_boric = (n_boric + 
                        boric_abs/100*input$part_abs/100*total_inscritos +
                        (100-input$abst_parisi)/100*n_parisi*boric_parisi/100 +
                        (100-input$abst_sichel)/100*n_sichel*boric_sichel/100 +
                        (100-input$abst_provoste)/100*n_provoste*boric_provoste/100 +
                        (100-input$abst_artes)/100*n_artes*boric_artes/100 +
                        (100-input$abst_meo)/100*n_meo*boric_meo/100)/n_total
        
        kast_parisi = 100-input$div_part_parisi
        kast_sichel = 100-input$div_part_sichel
        kast_provoste = 100-input$div_part_provoste
        kast_artes = 100-input$div_part_artes
        kast_meo = 100-input$div_part_meo
        
        kast_abs = 100-input$div_part_abs
        
        perc_kast = (n_kast + 
                       kast_abs/100*input$part_abs/100*total_inscritos +
                       (100-input$abst_parisi)/100*n_parisi*kast_parisi/100 +
                       (100-input$abst_sichel)/100*n_sichel*kast_sichel/100 +
                       (100-input$abst_provoste)/100*n_provoste*kast_provoste/100 +
                       (100-input$abst_artes)/100*n_artes*kast_artes/100 +
                       (100-input$abst_meo)/100*n_meo*kast_meo/100)/n_total

        d_new = data.frame(
          "n_total" = rep(n_total,2),
          "p_total" = rep(part_total,2),
          "perc_candidato" = c(perc_boric, perc_kast),
          "n_candidato" = c(perc_boric, perc_kast)*n_total,
          "candidato" = c("Boric", "Kast")
          
        )
        
        d_new
    })
    
    d_mapa2 = reactive({
      
      regiones = data.frame(nombre = c("XV - Arica y Parinacota","I - Tarapaca","II - Antofagasta","III - Atacama","IV - Coquimbo",
                                       "V - Valparaiso","XIII - Metropolitana","VI - Libertador General Bernardo O'Higgins","VII - Maule",
                                       "XVI - Ñuble", "VIII - Biobio", "IX - Araucania","XIV - Los Rios","X - Los Lagos", 
                                       "XI - Aysen", "XII - Magallanes"),
                            codigo = c("15","01","02", "03", "04", "05", "13", "06", "07", "16", "08", "09", "14", "10", "11", "12"))
      
      region = regiones$codigo[regiones$nombre==input$region]
      
      d <- mapa_comunas %>% filter(codigo_region==region)
      
      d
        
    })
    
    renderPieChart_all = function(mainTitle) {
      renderPlotly({
        data = d_new()
        
        data$n = round(data$perc_candidato*100,2)
        
        col_pie = c("#8bb74c","#ecb448")
      
        participation = data$p_total
        
        m1 <- list(
          l = 50,
          r = 50,
          b = 80,
          t = 70,
          pad = 4
        )
        
        plt = data %>% plot_ly(labels = ~candidato, values = ~perc_candidato,
                               type = "pie", textposition = 'inside',
                               textinfo = 'label+percent',
                               insidetextfont = list(color = '#FFFFFF',size=14),
                               text = ~paste0('<b>', data$candidato, '</b>: <br>',
                                              "N Votos: ", formatC(round(data$n_candidato,0), big.mark = ",", format="d"),"<br>",
                                              "% Votos: ",round(data$perc_candidato*100,1)),
                               hoverinfo = 'text',
                               marker = list(colors = col_pie,
                                             line = list(color = '#FFFFFF', width = 3)),
                               #The 'pull' attribute can also be used to create space between the sectors
                               showlegend = FALSE) %>%
          layout(title = mainTitle, margin = m1,
                 xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = FALSE)) %>%
          layout(annotations = 
                   list(x = 0.5, y = 1.15, text = ~paste0("Participación total: ",round(participation*100,1),"%<br>",
                                                          "Diferencia de votos: ",formatC(abs(round(data$n_candidato[1],0) - round(data$n_candidato[2],0)), big.mark =",", format = "d")), 
                        showarrow = F, xref='paper', yref='paper', 
                        xanchor='auto', yanchor='auto', xshift=0, yshift=0,
                        font=list(size=15))
          )
        
        plt
      })
    }
    
    renderMap_votes = function(mainTitle){
      renderLeaflet({
        
        cols1 = c("#3279a2","#5fb7a2","#8bb74c","#ecb448","#e35b35","#c42727")
        
        data <- d_mapa2()
        
        pal <- colorNumeric(
          palette = cols1[length(cols1):1],
          domain = data$dif_1era_pleb
        )
        
        data2 <- as.data.frame(data)
        
        labs <- lapply(seq(nrow(data2)), function(i) {
          paste0( '<p><b>', data2[i, "nombre_comuna"], '</b></p>', 
                  "<p>Part. Plebiscito: ",round(data2[i, "participacion"]*100,1),'%</p><p>', 
                  "Apruebo: ",round(data2[i, "voto_apruebo"]/data2[i, "total_votos_pleb"]*100,1),'%</p><p>', 
                  "<p>Part. 1era Vuelta: ",round(data2[i, "total_votos_1era"]/data2[i, "total_votos_pleb"]*data2[i, "participacion"]*100,1),'%</p><p>', 
                  "Dif. Votos 1era Vuelta - Plebiscito: ",data2[i, "dif_1era_pleb"], '</p>' ) 
        })
        
        map <- leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addPolygons(data = data,
                      fillColor = ~pal(dif_1era_pleb),
                      color = "#b2aeae",
                      fillOpacity = 0.7,
                      smoothFactor = 0.2,
                      weight = 1,
                      popup = lapply(labs, htmltools::HTML),
                      popupOptions = popupOptions(maxWidth = 300, closeOnClick = TRUE)) %>%   
          addLegend(pal = pal, # paleta de colores 
                    values = data$dif_1era_pleb,
                    position = "bottomright",
                    title = "Diferencia votos (N)") %>% addPolylines(data = data, color = "white", opacity = 1, weight = 1) 
        
        map
        
      })
    }
  
    output$results_all = renderPieChart_all("")
    
    output$map_votes = renderMap_votes("")

    output$update <- renderUI({ 
      data = d_new()
      word = "gana"
      
      if(data$perc_candidato[data$candidato=="Boric"]<0.5){
        word = "pierde"  
      }
      
      HTML(paste0("<p align='center'>",paste0("Gabriel Boric ",word," con ",round(data$perc_candidato[data$candidato=="Boric"]*100,2),"% de los votos"),"</p>"))
    })

}
