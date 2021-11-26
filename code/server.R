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
library(htmltools)

hrbrthemes::update_geom_font_defaults(family=font_fsm)

servel <- read.csv("https://raw.githubusercontent.com/maibennett/presidenciales_pobreza/main/data/servel_county_clean.csv")
casen <- read.csv("https://raw.githubusercontent.com/maibennett/presidenciales_pobreza/main/data/casen_county_clean.csv")

casen <- casen %>% rename(comuna_nombre = comuna)
servel <- servel %>% mutate(comuna_nombre = str_to_title(comuna_nombre))

d <- left_join(servel, casen, by = "comuna_nombre")
d <- d %>% mutate(Boric = ifelse(candidato=="GABRIEL BORIC FONT", 1, 0),
                  Kast = ifelse(candidato=="JOSE ANTONIO KAST RIST", 1, 0))

d <- d %>% mutate(candidato_apellido = ifelse(candidato=="GABRIEL BORIC FONT", "Boric",
                                              ifelse(candidato=="JOSE ANTONIO KAST RIST", "Kast",
                                                     ifelse(candidato=="EDUARDO ARTES BRICHETTI", "Artes",
                                                            ifelse(candidato=="FRANCO PARISI FERNANDEZ", "Parisi",
                                                                   ifelse(candidato=="MARCO ENRIQUEZ-OMINAMI GUMUCIO","MEO",
                                                                          ifelse(candidato=="SEBASTIAN SICHEL RAMIREZ", "Sichel",
                                                                                 ifelse(candidato=="YASNA PROVOSTE CAMPILLAY", "Provoste",""))))))))


d <- d %>% filter(candidato_apellido!="")

d_wide <- pivot_wider(d, id_cols = c(region_nombre, comuna_nombre, comuna_id), names_from = candidato_apellido,
                      values_from = c(total_votos, p_votos, total))

#url <- 'https://raw.githubusercontent.com/pachamaltese/chilemapas/master/data_geojson/comunas/r07.geojson'
#comunas_r07 <- rgdal::readOGR(url)

#comunas_d18 = as.data.frame(cbind(c("Cauquenes","Chanco","Colbun","Linares","Longavi","Parral",
#                                    "Pelluhue","Retiro","San Javier","Villa Alegre","Yerbas Buenas"),
#                                  c("07201","07202","07402","07401","07403","07404",
#                                    "07203","07405","07406","07407","07408")))
#names(comunas_d18) = c("COMUNA","codigo_comuna")

#comunas_r07 <- merge(comunas_r07,comunas_d18,by="codigo_comuna") 

#comunas_d18 = comunas_r07[!is.na(comunas_r07$COMUNA),]
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

# Define server logic required to draw a histogram
server = function(input, output, session) {
 
    updateSliderInput(session, "part_total", value = 0, min = 0, max = 100,
                      step = 0.2)
  
    updateSliderInput(session, "div_part_total", value = 0, min = 0, max = 100,
                    step = 1)
    
    updateSliderInput(session, "abst_parisi", value = 30, min = 0, max = 100,
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

    
    d_new = reactive({
      
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
          "n_candidato" = c(perc_boric, perc_kast)*total_inscritos,
          "candidato" = c("Boric", "Kast")
          
        )
        
        d_new
    })
    
    
    renderPieChart_all = function(mainTitle) {
      renderPlotly({
        data = d_new()
        
        data$n = round(data$perc_candidato*100,2)
        
        col_pie = c("#8bb74c","#ecb448")
        
        plt = data %>% plot_ly(labels = ~candidato, values = ~perc_candidato,
                               type = "pie", textposition = 'inside',
                               textinfo = 'label+percent',
                               insidetextfont = list(color = '#FFFFFF',size=14),
                               hoverinfo = 'text',
                               text = ~paste0(n,"%"),
                               marker = list(colors = col_pie,
                                             line = list(color = '#FFFFFF', width = 3)),
                               #The 'pull' attribute can also be used to create space between the sectors
                               showlegend = FALSE) %>%
          layout(title = mainTitle,
                 xaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = FALSE),
                 yaxis = list(showgrid = FALSE, zeroline = TRUE, showticklabels = FALSE))
        
        plt
      })
    }
    
  
    output$results_all = renderPieChart_all("")
    

    output$update <- renderUI({ 
      data = d_new()
      word = "gana"
      
      if(data$perc_candidato[data$candidato=="Boric"]<0.5){
        word = "pierde"  
      }
      
      HTML(paste0("<p align='center'>",paste0("Gabriel Boric ",word," con ",round(data$perc_candidato[data$candidato=="Boric"]*100,2),"% de los votos"),"</p>"))
    })

}
