#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny.i18n)
library(DT)
library(raster)
library(lwgeom)
library(rgdal)
library("sf")
library(ggplot2)
library(plotly)
library(ggthemes)
library(OpenStreetMap)
library(maps)
library(leaflet)
library(leaflet.extras)
library(jsonlite)
library(geojsonio)
library(geojsonR)
library(dplyr)
library(readr)
library(reshape2)
library(tidyverse)
library(RColorBrewer)
library(colorspace)
library(rsconnect)
library(fontawesome)
library(shinythemes)

#library(plyr)

#here::here("UrbanFabric


source("helpers.R")
source("maphelper.R")


i18n <- Translator$new(translation_json_path='translation.json')
i18n$set_translation_language('deu')


ui <- fluidPage(theme = shinytheme("sandstone"),
                
                shiny.i18n::usei18n(i18n),
                tags$div(
                  style='float: right;',
                  selectInput(
                    inputId='selected_language',
                    label=i18n$t('Change language'),
                    choices = i18n$get_languages(),
                    selected = i18n$get_key_translation()
                  )
                ),
                
    titlePanel(i18n$t("Urban Fabric: Urbaner Boden und versiegelte Oberflächen in Berlins Nachbarschaften - Prototyp/Entwurf")),
    #Einleitung und Erklärung-------------

    
    wellPanel(
      fluidRow("", 
             
          
             column(6, offset = .4,               h3(i18n$t("Diese App wurde erstellt um den gegenwärtigen Bodenversiegelungszustand in Berlins Nachbarschaften abzubilden und zu ermitteln, wie viel Boden bzw. Pflanzensubstrat erforderlich sein wird, um mit grüner Infrastruktur klimaangepasstere Nachbarschaften zu gestalten. In der gegenwärtigen Version ist ausschließich der Planungsraum Wrangelkiez in Berlin-Kreuzberg erkundbar. Die Karten und Diagramme basieren auf Open Data Daten aus dem Geoportal Berlin,  dem  Umweltatlas Berlin und vom Amt für Statistik Berlin-Brandenburg.")),
                   
               h3(""),
               tags$div(i18n$t("Genutzte Datensätze:"), tags$br(),
               "Lebensweltlich orientierte Räume (LOR)- Planungsräume (Urheber: Amt für Statistik Berlin-Brandenburg)", tags$br(),
                "Umweltatlas Berlin / [Gründächer - Gebäudeflächen (Umweltatlas)]", tags$br(),
                 "Geoportal Berlin / [Straßenbefahrung Berlin 2014]", tags$br(),
                 "Umweltatlas Berlin / [Bodengesellschaften und Bodenarten 2015]", tags$br(),
                   "Geoportal Berlin / [Baumbestand Berlin - Straßenbäume]", tags$br(),
                   "Geoportal Berlin / [Baumbestand Berlin - Anlagenbäume]", tags$br(),
                  "Geoportal Berlin / [Adressen Berlin]"),
               
               uiOutput("URLtechdetailsStrassenbefahrung")
               ),
       
             column(5, offset = 7, 
         h3(i18n$t("Autorin: Moreen Willaredt")), 
         h3(i18n$t("Kontakt: moreen.willaredt@campus.tu-berlin.de"))
       
            
            
    ))),#fluidrow  
    
     #Kapitel1 Auswahl des Kiezes      
    fluidRow( "",
            
              column(11, offset = .4,
             
                h2(i18n$t("Auswahl Nachbarschaft")) 
                
              )),#column, fluidRow1
    
      fluidRow( 
        column(3, 
               h3(i18n$t("Fahre mit der Maus über die Karte und Wähle durch klicken eine Nachbarschaft aus (in der gegenwärtigen Version ist ausschließlich der Wrangelkiez wählbar)")),
               h1("")
              
              
        ),
               
        column(9, offset= 3,
                leafletOutput("kiezmap"),
                h1("")
                )),#column  ), #fluidRow2
    
      fluidRow( 
                column(11, offset = .4,
                  textOutput("wahlKiez2"),
                   h1("")
                 )#column3
                 ), #fluidRow3
    
   
    #Kapitel2 Aufteilung der Flächen---------
    
    
    
    wellPanel(fluidRow(  
             column(9,
              h1(i18n$t("Flächenverteilung")) ,
              h3(tableOutput("overviewKiez")),
            leafletOutput("erkunden",height="100vh"),
                        h1("")
            ),#column  #fluidRow2
               column(3,
                      h3(i18n$t("Fahre mit der Maus über die Karte, um zu erkunden, aus welchen Flächenarten sich der Kiez zusammen setzt und woraus sie bestehen")),
                      h1("")
                      
                 
               )),
    fluidRow( #icon("bar-chart-o")),
               
                  column(8, offset = .4,
               h3(i18n$t("Anteile der Flächenarten an der Gesamtfläche")),                    
              plotOutput("gesamtFlaechenPlot",height = "500px"), 
              h3(i18n$t("Anteile der Flächenarten am öffentlichen Raum (ohne Gebäude, Hinterhöfe und Gewässer)")),
              plotOutput("oeffRaumFlaechenPlot",height = "500px"),
              h3(i18n$t("Anzahl der Parkplätze im ausgewählten Bereich")),
              plotOutput("parkenPlot",height = "500px" , width = "100%") 
              
                ))), #fluidrow Flächeninvent
  
  #Kapitel3 Versieglung der Flächen------------
    
      fluidRow("", 
             #icon = icon("road"), 
             column(12, offset = .4,
                    
                    h1(i18n$t("Flächenversiegelung")) ,
                    h3(i18n$t("In dieser Karte spiegeln die Farben die Versiegelungsklassen der Flächenbeläge wieder")),
                    uiOutput("URLVersiegelung"),
                    h1("")
             )),
    fluidRow( "", 
      column(10, 
             leafletOutput("versiegelt",height="100vh")
      )),
    
    fluidRow( "",
              column(8, offset=4,
              h1(i18n$t("Material der Versiegelung nach Flächenarten")), 
              plotOutput("versiegelungsPlot2",height = "800px") ,
             h1(i18n$t("Versiegelungsklassen der Flächenarten")),
              plotOutput("versiegelungsPlot",height = "500px",width = "800px") 
              #h1(i18n$t("Flächenanteile der Versiegelungsmaterialien")),
            #  plotOutput("versiegelungsPlot3",height = "500px",width = "700px")
              
             
    )), #FluidRow versiegelung 
    
  #Kapitel4 Straßenbäume----------
  wellPanel(fluidRow( "",
                      #icon = icon("bar-chart-o"),
                      column(11, offset = .4,
                             
                             h1(i18n$t("Straßenbäume")) ,
                             h3(i18n$t("Fahre mit der Maus über die Karte, um zu erkunden, welche Bäume im Kiez wachsen, wie alt sie sind und wieviel Fläche ihnen in ihrer Baumscheibe zur Verfügung steht")),
                             h1("")
                      )),
 fluidRow( "", 
                       column(10, 
                              leafletOutput("baumemap",height="100vh")
                       )),
           
fluidRow( "",
          h1(i18n$t("Baumarten im ausgewählten Bereich")),
          plotOutput("baumartenPlot",height = "600px",width = "1500px"),
          
          column(8,offset = 4,
               h1("Straßenbäume pro Straße"),
               plotOutput("baeumePlot",height = "500px"),
               h3(i18n$t("Achtung: Nicht alle Straßen sind vollständig in der ausgewählten Fläche enthalten")),
               h1(""),
               h1(i18n$t("Baumarten pro Straße")),
           plotOutput("baeumeproStrPlot",height = "500px"),
           h1(i18n$t("Pflanzjahr der Bäume")),
           plotlyOutput("Baeumealter_strPlot",height = "500px")
          ))),        
           
           
     
  #Kapitel5 Urbaner Boden -----------------
            
fluidRow( "",
        #icon = icon("bar-chart-o"),
        column(11, offset = .4,
        h1(i18n$t("Urbane Böden")) ,
        h3(i18n$t("Fahre mit der Maus über die Karte, um zu erkunden, in welchen Flächen unversiegelter urbaner Boden erlebbar ist und wichtige Ökosystemdienstleistungen erbringen kann.")),
         h1("")
                            )),
   
fluidRow( "", 
          column(10, 
                 h3(i18n$t("Status quo:")), textOutput("urbBodenAnteilstatus"),
                 leafletOutput("UrbSoilMap",height="100vh") ,
                 tags$head(tags$style("#urbBodenAnteilstatus{color: black;
                                 font-size: 20px;}"))
          )),
            
fluidRow(
                       
                       #    tabPanel("Flächenentsiegelung", icon = icon("road"),),
                          
                       
                       #Layout, Baumscheiben transform ---------------------------------------------              
                       h3(i18n$t("Stelle an den Balken das Ausmaß der Begrünungsszenarien ein, um zu ermitteln, wieviel Boden zur Umsetzung gebraucht wird")),    
                       column(3,
                              h2(i18n$t("Baumscheiben vergrößern")),
                                h1(""),
                                     
                                            sliderInput("BSfaktor", i18n$t("Baumscheibengröße in % (100% ist Ausgangsgröße):"),
                                                        min =100, max = 300,
                                                        value =100, step = 10),
                                            sliderInput("BodTiefe", i18n$t("Aushubtiefe nach Entsiegelung in m"),
                                                        min =0.1, max = 1,
                                                        value =0.5, step = 0.1),
                             
                                     #splitLayout
                                
                             #   h1(""), #horizontal whitespace
                              #  actionButton(
                              #    inputId = "transformBS",
                              #    label = "transform",
                              #    icon("leaf"),
                               #   class = "btn btn-success",
                               #   width = "33%"
                               # ),
                               
                                 h1("") #horizontal whitespace
                                
                             #   tableOutput("BSextendBodendemand") 
                                
                               
                                
                       ), # column Baumscheiben
                       
                   #Layout Dachbegrünung trans---------------------    
                  
                   column(3,
                          h2 (i18n$t("Dächer begrünen")),
                                  
                                            sliderInput("DachTeil", i18n$t("Anzahl der Gebäude mit Gründach:"),
                                                        min = round(length(Surfaces_subset$Gebaeude$GRUENDACH[Surfaces_subset$Gebaeude$GRUENDACH == "vorhanden"]),digits = 1), max = length(Surfaces_subset$Gebaeude$GRUENDACH),
                                                        value = round(length(Surfaces_subset$Gebaeude$GRUENDACH[Surfaces_subset$Gebaeude$GRUENDACH == "vorhanden"]),digits = 1), step = 5),
                                            sliderInput("Substrathoehe", i18n$t("Substrattiefe in cm:"),
                                                        min = 2, max = 50,
                                                        value = 2, step = 2) ,
                                            
                                       
                                
                                h1(""),
                                 
                                           
                                          
                             
                                
                                h1("")
                            #    tableOutput("GDextendBodendemand")
                                #tableOutput("GD_transFaktorTab") 
                                
                       ), #column Gründach
                   
                   column(3,
                          h2(i18n$t("Parklets aufstellen")),
                            h1(""),
                            
                                        sliderInput("PKfaktor", i18n$t("Anzahl der Parkplätze:"),
                                                    min =0, max = sum(round(Surfaces_subset$Parkplatzflaeche$flaeche/12)),
                                                    value =1, step = 1),
                                        sliderInput("BeetHoehe", i18n$t("Höhe des Beets in m"),
                                                    min =0.1, max = 1,
                                                    value =0.5, step = 0.1),
                                        
                            
                            h1("") #horizontal whitespace
                        
                    
                   ), # column parklet
                   column(3,
                          h2(i18n$t("Fassade begrünen")),
                          h3(i18n$t("in Bearbeitung, noch nicht intergriert")),
                                sliderInput("FassadenZahl", i18n$t("Anzahl Gebäude für Fassadenbegrünung:"),
                                            min = 1, max = length(Surfaces_subset$Gebaeude$gml_id),
                                           value = 1, step = 1),
                          
                          
                               sliderInput("FassadeVolumen", i18n$t("Bodenvolumen pro Fassadenbegrünung in m³:"),
                                         min = 0.8, max = 1.5,
                                       value = 1, step = 0.1)
                          
                   ), # column Fassadenbegrün
              
                   
         fluidRow( "",
                   column(10,offset = 1,
                   h1(""), #horizontal whitespace   
                   h1(""), #horizontal whitespace  
                             actionButton(
                               inputId = "transformKiez",
                               label = "transform",
                               icon("leaf"),
                               class = "btn btn-success",
                               width = "33%"
                             ),        
                             h1(""), #horizontal whitespace 
                             h1(""), #horizontal whitespace
                            h1(""),
                   
                   h1("Resulting soil demand"),
                   
                              tableOutput("gesamtBodendemand"),  
                   tags$head(tags$style("#gesamtBodendemand{color: green;
                                 font-size: 20px;}")),
                   tags$head(tags$style(type='text/css', ".irs-grid-text{ font-size: 20px;}"))
                             )),
              fluidRow(               
                           
                            column(8, offset = 5,
                            h3(i18n$t("Anteile der Flächen mit unversiegeltem Boden an Gesamtfläche")),
                              plotOutput("urbbodenPlot",height = "500px",width = "800px")
                            ),
                            column(4, offset = 1,  
                             h1(""),
                             h1(""),
                             textOutput("urbBodenAnteiltrans1"),
                             h1(""),
                             textOutput("urbBodenAnteiltrans2"),
                             h1(""),
                             textOutput("urbBodenAnteiltrans3"),
                             h1(""),
                             textOutput("urbBodenAnteiltrans4"),
                             h1(""),
                             
                             tags$head(tags$style("#urbBodenAnteiltrans1{color: black;
                                 font-size: 20px;}")),
                             tags$head(tags$style("#urbBodenAnteiltrans2{color: black;
                                 font-size: 20px;}")),
                             tags$head(tags$style("#urbBodenAnteiltrans3{color: black;
                                 font-size: 20px;}")),
                             tags$head(tags$style("#urbBodenAnteiltrans4{color: black;
                                 font-size: 20px;}"))
                   ))
                   
                   #Layout Fassaden trans---------------------    
                 #  h1(""), 
                     #  tabPanel("Flächentransformation Fassaden begrünen",
                          #      h6("in Bearbeitung"),
                          #      sliderInput("FassadenZahl", "Anzahl Gebäude für Fassadenbegrünung:",
                          #                  min = 1, max = length(Surfaces_subset$Gebaeude$gml_id),
                           #                 value = 1, step = 1),
                                
                                
                           #     sliderInput("FassadeVolumen", "Bodenvolumen pro Fassadenbegrünung in m³:",
                            #                min = 0.8, max = 1.5,
                               #             value = 1, step = 0.1),
                                
                                
                                
                              #  tableOutput("Fass_transFaktor")  )#Tabpanel Fassade
                   ) #tabSetPanel transformers
    #
)#fluid page
       
                       
            
         
              
            
          
           
       
          
       
          
         
            
           
           
   
     


# Define server logic ----
server <- function(input, output, session) {
  
##language --------------------------------------
  observeEvent(input$selected_language, {
    update_lang(session, input$selected_language)
 
  
    
  ##inputs -------------------------------------------
  ## processing - Urbane Transformation----------------------------
  toListen <- reactive({
    list(input$transformKiez) })
  
  observeEvent(toListen(), {
    
   
    # slider inputs ----------
    ###Baumscheiben----
     BSsliderFaktor <- reactive({ input$BSfaktor-100})
     BS_Bodentiefe <- reactive({ input$BodTiefe  })  
     ###Parklets--------
     PK_anzahl <- reactive({ input$PKfaktor }) 
     PK_Bodentiefe <- reactive({ input$BeetHoehe }) 
     ####Dachbegrünung-----
     GD_DachTeil <- reactive({
       input$DachTeil - length(Surfaces_subset$Gebaeude$GRUENDACH[Surfaces_subset$Gebaeude$GRUENDACH == "vorhanden"])
     })
     GD_Substrathoehe <- reactive({ input$Substrathoehe/100 })
    
    
     
     
     
           # output$BS_transFaktor <- renderText({ paste("Baumscheiben vergrößern um", BSsliderFaktor()-100, "%" ,sep=" ")
           # })
              
   #Berechnung erweiterte Baumscheibengröße----------
            BS_extend <- st_buffer(BS_trans, sqrt(((input$BSfaktor-100)/100*st_area(BS_trans)+pi*st_area(BS_trans)/pi)/pi)-sqrt(st_area(BS_select)/pi))
            BS_extend_trans <-st_transform(BS_extend, crs= 4326)
            total_extendedBS <- sum(as.numeric(round(st_area(BS_extend_trans),digits=1)))- sum(as.numeric(round(st_area(Surfaces_subset$Baumscheiben),digits=1)))
            
            urbBoden_anteile$area[urbBoden_anteile$Group == "Baumscheiben"] <- urbBoden_anteile$area[urbBoden_anteile$Group == "Baumscheiben"]+ total_extendedBS
 
            
     #Berechnung umgewandelte parkplätze
            set.seed(123)
            df <- Surfaces_subset$Parkplatzflaeche
            my_samp <- sample(1:nrow(df))  # sample the rows of df
           PK_select <- df[my_samp[which(cumsum(df$flaeche[my_samp]) < PK_anzahl()*12)],]  # return the sampled rows with cumulative sum < 100
            total_surfacePK <- sum(PK_select$flaeche)
           
  #Berechnung begrünbare Dächer
            
            begruenbar <- Surfaces_subset$Gebaeude[Surfaces_subset$Gebaeude$GRUENDACH=="nicht vorhanden", ] 
            begruenbar_select <-  begruenbar[sample(nrow(begruenbar),GD_DachTeil() ), ]
             
            
            total_extendedGD <- sum(as.numeric(st_area(begruenbar_select)))
            total_extendedPK <- sum(as.numeric(st_area(PK_select)*0.8))
            
            
                    
           # urbBoden_anteile$area[urbBoden_anteile$Group == "Baumscheiben"] <- urbBoden_anteile$area[urbBoden_anteile$Group == "Baumscheiben"]+ total_extendedBS
            
  
           # output$map <- renderLeaflet({
          #    leaflet() %>%
           #     ... whatever to build base map ...
           #   ... without reactive part ...  
           # })
            
           # # handle the update of the static map with reactive part
           # observe({
              # update map
           #   leafletProxy("map", data=DFA()) %>%
            #    clearMarkers() %>%
            #    addCircleMarkers(...)
           # })
            
            
      #update ouputs: Karte, Diagramme und Bodenbedrafstabellen          
            #output$UrbaneBodenkarte1 <- renderLeaflet({
             
            leafletProxy("UrbSoilMap", data=toListen())   %>% clearShapes() %>%
                addPolygons(
                  data =begruenbar_select,  # LAD polygon data from geojson
                  weight = 1,  # line thickness
                  opacity = 1,  # line transparency
                  color = "green",  # line colour
                  fillOpacity = 0.8,
                  fillColor = treeCol,
                  labelOptions = labelOptions(noHide = F, textsize = "25px"),
                  label =paste("Zur Begrünung gewähltes Gebäude mit potentieller Dachfläche", 0.3*round(st_area(begruenbar_select),digits=1), "m²", sep=" ")
                )%>% 
              addPolygons(
                  data =  subset(Surfaces_subset$Gebaeude, Surfaces_subset$Gebaeude$GRUENDACH=="vorhanden"),  # LAD polygon data from geojson
                  weight = 1,  # line thickness
                  opacity = 1,  # line transparency
                  color = "green",  # line colour
                  fillOpacity = 0.8,
                  fillColor = treeCol,
                  labelOptions = labelOptions(noHide = F, textsize = "25px"),
                  label =paste("Gebäude mit Dachbegrünung", sep=" ")
                )%>%
              addPolygons(
                  data =Surfaces_subset$Gruendach_Geb,  # LAD polygon data from geojson
                  weight = 1,  # line thickness
                  opacity = 1,  # line transparency
                  color = "green",  # line colour
                  fillOpacity = mapOp,
                  fillColor = gruendCol,
                  labelOptions = labelOptions(noHide = F, textsize = "25px"),
                  label = paste("Urbanes Bodensubstrat auf Gründach:", Surfaces_subset$Gruendach_Geb$GRUEN_KAT ,sep = " ")  # LAD name as a hover label
                )%>%  
             addPolygons(
                data =Surfaces_subset$Gruenanlagen,  # LAD polygon data from geojson
                weight = 1,  # line thickness
                opacity = 1,  # line transparency
                color = "green",  # line colour
                fillOpacity = mapOp,
                fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gruenanlagen$material,surfaceMaterial2$material)]),
                labelOptions = labelOptions(noHide = F, textsize = "25px"),
                label = "teilverisegelter Urbaner Boden in Grünanlagen"  # LAD name as a hover label
              )%>%
              addPolygons(
                data =Surfaces_subset$Gruenflaeche,  # LAD polygon data from geojson
                weight = 1,  # line thickness
                opacity = 1,  # line transparency
                color = "green",  # line colour
                fillOpacity = mapOp,
                fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gruenflaeche$material,surfaceMaterial2$material)]),
                labelOptions = labelOptions(noHide = F, textsize = "25px"),
                label = "Urbaner Boden in Grünfläche"  # LAD name as a hover label
              )%>%
              addPolygons(
                data =Surfaces_subset$Spielplaetze,  # LAD polygon data from geojson
                weight = 1,  # line thickness
                opacity = 1,  # line transparency
                color = "green",  # line colour
                fillOpacity = mapOp,
                fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Spielplaetze$material,surfaceMaterial2$material)]),
                labelOptions = labelOptions(noHide = F, textsize = "25px"),
                label = "Teilversiegelter urbaner Boden auf Spielplatz"  # LAD name as a hover label
              )%>%
              addPolygons(
                  data = BS_extend_trans,  # LAD polygon data from geojson
                  weight = 1,  # line thickness
                  opacity = 1,  # line transparency
                  color = "green",  # line colour
                  fillOpacity = 0.8,
                  fillColor = treeCol,
                  labelOptions = labelOptions(noHide = F, textsize = "25px"),
                  label =paste("vergrößert", round(st_area(BS_extend_trans),digits=1), "m²", sep=" ")
                ) %>% 
                addPolygons(
                  data = PK_select,  # LAD polygon data from geojson
                  weight = 1,  # line thickness
                  opacity = 1,  # line transparency
                  color = "green",  # line colour
                  fillOpacity = 0.8,
                  fillColor = treeCol,
                  labelOptions = labelOptions(noHide = F, textsize = "25px"),
                  label =paste("Parkplatz für Parklets, Fläche:", round(st_area(PK_select),digits=1), "m², Material:" ,surfaceMaterial2$Bezeichnung[match(PK_select$material,surfaceMaterial2$material)], sep=" ")
                )%>%
                addPolygons(
                  data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
                  weight = 1,  # line thickness
                  opacity = 1,  # line transparency
                  color = "gray",  # line colour
                  fillOpacity = mapOp,
                  labelOptions = labelOptions(noHide = F, textsize = "25px"),
                  fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Baumscheiben$material,surfaceMaterial2$material)]),
                  label = paste("Baumscheibe", round(st_area(Surfaces_subset$Baumscheiben),digits = 1),"m²" ,sep = " ")  # LAD name as a hover label
                )
            
            
            
            
    ## Bodenbedarf berechnen-----------------------
            output$gesamtBodendemand <- renderTable({ 
              Boden_begruenKiez <- data.frame("Begrünungsmaßnahme" = c("","Baumscheiben vergrößern","Dächer begrünen", "Parklets Aufstellen","Gesamt"),
                                              "Ausmaß" = c("",BSsliderFaktor(),length(begruenbar_select$GRUENDACH),PK_anzahl(),NA),
                                              "Tiefe" = c("m",BS_Bodentiefe(),GD_Substrathoehe() ,PK_Bodentiefe(), NA),
                                              "unversiegelte Gesamtfläche" = c("m²",round(total_extendedBS)+7, round(total_extendedGD*0.3),  round(sum(st_area(PK_select))*0.8),  sum(round(total_extendedBS)+7,round(total_extendedGD*0.3),round(sum(st_area(PK_select))*0.8))),
                                              "Bodenbedarf" = c("m³",BS_Bodentiefe()*(round(total_extendedBS)+7),round(GD_Substrathoehe()*total_extendedGD*0.3),round(PK_Bodentiefe()*sum(st_area(PK_select)*0.8)),round(sum(BS_Bodentiefe()*(round(total_extendedBS)+7),round(GD_Substrathoehe()*total_extendedGD*0.3),PK_Bodentiefe()*sum(st_area(PK_select))*0.8))),
                                              "LKW-Ladungen" = c("#",BD_Boden*BS_Bodentiefe()*round(round(total_extendedBS+7)*0.8/30),round(BD_Boden*GD_Substrathoehe()*total_extendedGD*0.3*0.8/30),round(BD_Boden*PK_Bodentiefe()*sum(st_area(PK_select)*0.8)/30),round(sum(BD_Boden*BS_Bodentiefe()*(round(total_extendedBS)+7)*0.8/30,round(BD_Boden*GD_Substrathoehe()*total_extendedGD*0.3*0.8/30), round(BD_Boden*PK_Bodentiefe()*sum(st_area(PK_select))*0.8/30)))),
                                              "Preis für Substrat"= c("EUR",BS_Bodentiefe()*(round(total_extendedBS)+7)*15,round(GD_Substrathoehe()*total_extendedGD*0.3*15),round(PK_Bodentiefe()*sum(st_area(PK_select))*15),round(sum(BS_Bodentiefe()*(round(total_extendedBS)+7)*15,round(GD_Substrathoehe()*total_extendedGD*0.3)*15,round(PK_Bodentiefe()*sum(st_area(PK_select))*15))))
            
                 ) })#+7 equals out the error resulting vom the circular buffer assumption for non circular tree pits
            
            
              # output$BSextendBodendemand <- renderTable({ 
              #            Boden_extendedBS <- data.frame("prozentuale Vergr&ouml;ßerung" = BSsliderFaktor(),
                   #                    "Tiefe" = BS_Bodentiefe(),
                    #                   "zu entsiegelnde Gesamtfläche in m2" = total_extendedBS,
                    #                   "Bodenbedarf in m3" =  BS_Bodentiefe()*total_extendedBS,
                     #                  "LKW-Ladungen" = BD_Boden*BS_Bodentiefe()*total_extendedBS/30)
                     #       })
               
               
               
             #  output$GDextendBodendemand <- renderTable({ 
              #   Boden_extendedGD <- data.frame("Anzahl begrünter Gebäude" = length(begruenbar_select$GRUENDACH),
                                              #  "Substratdicke" = GD_Substrathoehe(),
                                              #  "begrünbare Dachfläche" = total_extendedGD*0.3,
                                               # "Bodenbedarf in m3" =  GD_Substrathoehe()*total_extendedGD*0.3,
                                               # "LKW-Ladungen" = BD_Boden*GD_Substrathoehe()*total_extendedGD*0.3/30) # 30t pro LKW
              # })
               
               
            
          #  output$PKextendBodendemand <- renderTable({ 
             # Boden_PK <- data.frame("Anzahl Parklets" = PK_anzahl(),
                             #                "Beethöhe" = PK_Bodentiefe(),
                                 #            "Bodenbedarf in m³" =  PK_Bodentiefe()*sum(st_area(PK_select)*0.8),
                                 #            "LKW-Ladungen" = BD_Boden*PK_Bodentiefe()*sum(st_area(PK_select)*0.8)/30)
            #})
            
      #Erweiterung urbaner boden durch Transformation berechnen
            
            urbBoden_anteile$area[urbBoden_anteile$Group == "Gründach"] <- urbBoden_anteile$area[urbBoden_anteile$Group == "Gründach"]+ total_extendedGD*0.3
            urbBoden_anteile$area[urbBoden_anteile$Group == "Parklet"] <- urbBoden_anteile$area[urbBoden_anteile$Group == "Parklet"]+ total_extendedPK
            
            
            #Erweiterung urbaner boden durch Transformation plotten      
        
        output$urbbodenPlot <- renderPlot(res= 150,expr={
          ggplot(data=urbBoden_anteile, aes(reorder(Group,-area),area/1000)) + 
            geom_col(color="black",aes(fill = class),alpha=0.5)+
            geom_text(aes(
              label= paste(round(area/max(area)*100, digits = 1),"%",sep=" ")),
              position = position_dodge(0.9),
              hjust = -0.2)+
            theme_bw()+
            coord_flip()+
            theme(axis.text.x=element_text(hjust=0,colour="black"),axis.text.y=element_text(hjust=0,colour="black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
            labs(x="",y= "Fläche in 1000 m²") +
            scale_y_continuous(limits=c(0,urbBoden_anteile$area[urbBoden_anteile$Group=="öffentlicher Raum + Gebäude"]*1.1/1000),breaks = seq(0, urbBoden_anteile$area[urbBoden_anteile$Group=="öffentlicher Raum + Gebäude"]*1.1/1000, by = 100))+
            scale_fill_manual(name = "",values = c("0"= VG_class_0, "1"= VG_class_1,"2"= VG_class_2,  "3"=VG_class_3, "4"= VG_class_4, "NA"=VG_class_NA))
        })
        
          #output$urbBodenAnteiltrans <- renderText({ paste("nach Transformation: Anteil unversiegelter Fläche an Gesamtfläche:", round( sum(urbBoden_anteile$area[urbBoden_anteile$Group!="Gesamtfläche"])/urbBoden_anteile$area[urbBoden_anteile$Group=="Gesamtfläche"]*100 ,digits=1), "%" ,sep=" ") 
            #      })
   
        
  # update output on map with extended treepit -------------
      
       #})
    
         
         
         
         
         
   
  ## processing Gruendach transform-------------------------------       
    
   #observeEvent(input$transformGD, {   
    

     
   #   output$GD_transFaktorTab <- renderTable({
        
     #   data.frame(
     #     Name = c("Substratdicke",
     #              "Anzahl gewählte Gebäude"),
     #     Value = c(GD_Substrathoehe(),
       #             GD_DachTeil()
     #     ),
      #    stringsAsFactors = FALSE)
        
     #  })
     



      
     
      output$urbBodenAnteiltrans1 <- renderText({ paste(i18n$t("Anteil unversiegelter Fläche am öffentlichen Raum:"), 
                                                       round( sum(urbBoden_anteile$area[urbBoden_anteile$Group!="öffentlicher Raum + Gebäude"])/urbBoden_anteile$area[urbBoden_anteile$Group=="öffentlicher Raum + Gebäude"]*100 ,digits=1),"%,", 
                                                       sep=" ")})
      
      output$urbBodenAnteiltrans2 <- renderText({ paste( i18n$t("Anzahl begrünter Gebäudedächer:"), length(Surfaces_subset$Gebaeude$GRUENDACH[Surfaces_subset$Gebaeude$GRUENDACH=="vorhanden"])+length(begruenbar_select$GRUENDACH),sep=" ")})
      
        
        output$urbBodenAnteiltrans3 <- renderText({ 
          paste(i18n$t("Durchschnittliche Baumscheibengröße:"), 
               round(mean(as.numeric(st_area(BS_extend_trans))),digits = 2), 
                "m²" ,sep = " ")})
          
          output$urbBodenAnteiltrans4 <- renderText({ paste(i18n$t("Aufgestelle Hochbeete auf Parkplätzen:"), PK_anzahl(), sep = " ")})
# Bodenbedarf berechnen----------------
   
          
      
})
  
###Outputs---------------
  output$UrbSoilMap <- renderLeaflet({
    UrbaneBodenKarte
  })
  
  output$urbbodenPlot <- renderPlot(res= 150,expr={
    
    urbbodenPlot
    
  })
    

  
 
  
  Fassadengebaeudezahl <- reactive({
    input$FassadenZahl
    
      })
  
  bodVolumenFassade <- reactive({
    input$FassadeVolumen
    
  })
  
 

  
    #outputs text------------------------------------------
  
  urlStr2014 <- a(i18n$t("PDF Straßenbefahrung"), href= "https://fbinter.stadt-berlin.de/fb_daten/beschreibung/datenformatbeschreibung/Datenformatbeschreibung_Straßenbefahrung_2014.pdf")
    output$URLtechdetailsStrassenbefahrung <- renderUI({tagList(i18n$t("Technische Beschreibung der Daten aus der Straßenbefahrung:"), urlStr2014)})  
     
  urlPaperAnne <- a("Timm et al. 2018", href="https://scholar.google.de/scholar_url?url=https://www.researchgate.net/profile/Gerd-Wessolek/publication/349104622_Timm_Kluge_Wessolekpdf/data/60201fb192851c4ed5577cf3/Timm-Kluge-Wessolek.pdf&hl=de&sa=X&ei=-FVpYdarAcKSy9YPkNKCmAU&scisig=AAGBfm1bjGLLtWwfsrkPhwJx6Y5__HykqQ&oi=scholarr")
    output$URLVersiegelung <- renderUI({tagList(i18n$t("Die Zuordnung der Versiegelungsklassen orientiert sich an:"), urlPaperAnne)})  
    
    
    output$wahlKiez2 <- renderText({ paste(i18n$t("Ausgewählt:") ,kiez_oi$PLANUNGSRAUM, "in", kiez_oi$BEZIRKSNAME, i18n$t("Gesamtfläche:") ,kiez_oi$FLAECHENGROESSE_IN_M2, "m²" ,sep="\n")
    })
    
    output$overviewKiez <- renderTable({
      overViewSurf <-  data.frame("Nachbarschaft" = c("-", kiez_oi$PLANUNGSRAUM),
                                  "Gesamtfläche" = c("m²", round(st_area(kiez_oi))),
                                  "Hinterhöfe, Gebäude, Wasser"= c( "%", round((unknown_space+Flaech_verteil$area[Flaech_verteil$Group=="Gebäude"])/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2)),
                                  "öffentlicher Raum:"= c( "%",  round(public_space/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2)) )
    })

  
   
    output$urbBodenAnteilstatus <- renderText({ paste("ca",round(sum(urbBoden_anteile$area[urbBoden_anteile$Group!="öffentlicher Raum + Gebäude"])/urbBoden_anteile$area[urbBoden_anteile$Group=="öffentlicher Raum + Gebäude"]*100, digits= 1),  i18n$t(" % des öffentlichen Raums in der ausgewählten Nachbarschaft beherbergt unversiegelten Boden und kann bodenspezifische Ökosystemdienstleistungen erbringen"),  sep=" ")
    })
    
  
 
    
   # output$Fass_transFaktor <- renderText({ paste("Fassadenbegrünung an", round(Fassadengebaeudezahl()/length(Surfaces_subset$Gebaeude$gml_id)*100 , digits = 1), "% der Gebäude im Kiez" ,sep=" ")
   # })
    
    
    output$VGklassen <- renderText({ paste(i18n$t("Versiegelungsklassen nach Timm et al. 2018") ,sep=" ")
    })
    
    #outputs karten------------------------------------------  
    
    output$kiezmap <- renderLeaflet({
        
        kiezmap %>% suspendScroll( sleep = TRUE, sleepTime = 1200, wakeTime = 750,
                                  sleepNote = TRUE, hoverToWake = FALSE,
                                  wakeMessage = i18n$t("Klicken zum Aktivieren"), sleepOpacity = 0.7)
    
    })
    
    output$erkunden <- renderLeaflet({
        karte%>% suspendScroll( sleep = TRUE, sleepTime = 1200, wakeTime = 750,
                                sleepNote = TRUE, hoverToWake = FALSE,
                                wakeMessage = i18n$t("Klicken zum Aktivieren"), sleepOpacity = 0.7)
      
        
    })
    
    output$versiegelt <- renderLeaflet({
      Versiegelungskarte %>% suspendScroll( sleep = TRUE, sleepTime = 1200, wakeTime = 750,
                                        sleepNote = TRUE, hoverToWake = FALSE,
                                        wakeMessage = i18n$t("Klicken zum Aktivieren"), sleepOpacity = 0.7)
      
    }) 
    
    output$baumemap <- renderLeaflet({
      Baumkarte%>% suspendScroll( sleep = TRUE, sleepTime = 1200, wakeTime = 750,
                                  sleepNote = TRUE, hoverToWake = FALSE,
                                  wakeMessage = i18n$t("Klicken zum Aktivieren"), sleepOpacity = 0.7)
      
      
    })
  
    
    
  
    #output$BS_transform <- renderLeaflet({
      
  
      #default in current version: Alle baumscheiben

      #total_extendedBS <- sum(round(st_area(BS_extend_trans),digits=1))- st_area(Surfaces_subset$Baumscheiben)

  
      #check how much bigger
      #st_area(BS_extend)/st_area(BS_select)
      
      
      #visualize
      # kiez_BS_transform <- map %>%
      #   leaflet::addPolygons(
      #     data =kiez_oi,  # LAD polygon data from geojson
      #     weight = .7,  # line thickness
      #     opacity = 1,  # line transparency
      #     color = "grey",  # line colour
      #     fillOpacity = 0.2,
      #     fillColor = "gray",
      #     label = paste("Transformationsraum", kiez_oi$PLANUNGSRAUM,  ", Gesamtfläche der Baumscheiben:", sum(round(st_area(BS_extend_trans),digits=1)), "m²","entspricht", round(sum(round(st_area(BS_extend_trans),digits=1))/(68*105) ,digits = 1),"Fussballfeldern" ,sep = " ")
      #   )%>%
      #   leaflet::addPolygons(
      #     data =BS_extend_trans,  # LAD polygon data from geojson
      #     weight = 1,  # line thickness
      #     opacity = 1,  # line transparency
      #     color = "green",  # line colour
      #     fillOpacity = 0.8,
      #     fillColor = treeCol,
      #     label =paste("vergrößert", round(st_area(BS_extend_trans),digits=1), "m²", sep=" ")
      #   )%>%
      #   leaflet::addPolygons(
      #     data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
      #     weight = 1,  # line thickness
      #     opacity = 1,  # line transparency
      #     color = "green",  # line colour
      #     fillOpacity = 0.5,
      #     fillColor = "green",
      #     label = paste("orginal", round(Surfaces_subset$Baumscheiben$flaeche,digits=1), "m²", sep=" ")  # LAD name as a hover label
      #   )
      # 
      # kiez_BS_transform
      
      
     
  #  })
    
    
    
    
    
    
    #LKWanzahl <- reactive({BD_Boden*BS_Bodentiefe()})
    
    # versuch die anzahl der LKW lieferungen als icons darzustellen
    # output$truck2 <-   DT::renderDataTable({
     #                         data.frame("LKW" = c(rep(as.character(icon("child")), 3 ))
     #                                    )
     #                   }, escape=FALSE)
                              
       
      
     
     
    
    #outputs plots------------------------------------------
    output$gesamtFlaechenPlot <- renderPlot(res= 150,expr={ 
        gesamtFlaechenPlot 
        
    })
    
    output$oeffRaumFlaechenPlot <- renderPlot(res= 150,expr={ 
        oeffRaumFlaechenPlot
        
    })
    
    
    output$parkenPlot <- renderPlot(res= 150,expr={ 
        parkenPlot
        
    })
    
    output$baeumePlot <- renderPlot(res= 150,expr={ 
        baeumePlot
        
    })
    
    
    output$baumartenPlot <- renderPlot(res= 150,expr={ 
        baumartenPlot
        
    })
    output$baeumeproStrPlot <- renderPlot(res= 150,expr={ 
        baeumeproStrPlot
        
    })
    
    output$Baeumealter_strPlot <- renderPlotly({ 
      
      ggplotly(baeumealterPlot_str)
  
    })
    
    output$versiegelungsPlot <- renderPlot(res= 150,expr={ 
      versiegelungsPlot
        
    })
    
    output$versiegelungsPlot2 <- renderPlot(res= 150,expr={ 
        versiegelungsPlot2
        
    })
    
    output$versiegelungsPlot3 <- renderPlot(res= 150,expr={ 
      versiegelungsPlot3
      
    }) 

    
  })#language wrap
}

# Run the app ----
shinyApp(ui = ui, server = server)
