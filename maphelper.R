



#Farbgebung--------------------------------




#tiles:----------------------------------------

map <- leaflet::leaflet() %>%
  leaflet::addProviderTiles(providers$OpenStreetMap, 
                            options = providerTileOptions(minZoom = 15, 
                                                          maxZoom = 28,
                                                          maxNativeZoom=19))

mapOV <- leaflet::leaflet() %>%
  leaflet::addProviderTiles(providers$OpenStreetMap, 
                            options = providerTileOptions(minZoom = 8, 
                                                          maxZoom = 28,
                                                          maxNativeZoom=19))


#Flächenarten  map-----------------------------------------

karte <- map %>%
 # leaflet::addLabelOnlyMarkers(13.44403,52.49872, label= paste(kiez_oi$PLANUNGSRAUM,
  #                                                 "Gesamtfläche in m²:",round(st_area(kiez_oi)),
 #                                                  "Hinterhöfe, Gebäude, Wasser in %:", round(Flaech_verteil$area[Flaech_verteil$Group=="priv. Flächen (Hinterhöfe), Wasser"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2),
#                                                   "öffentlicher Raum in %:", round(public_space/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), 
 #                                                  sep = "</br> "),
 #                     labelOptions = labelOptions(textsize = "20px")
#   )%>%
  leaflet::addPolygons(
    data =kiez_oi,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = 0.2,
    fillColor = "green",
     # LAD name as a hover label
    labelOptions = labelOptions(noHide = F, textsize = "25px")
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gebaeude,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "purple",  # line colour
    fillOpacity = mapOp,
    fillColor = hausCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste ("Gebäude",round(Flaech_verteil$area[Flaech_verteil$Group=="Gebäude"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), "%" ,sep=" ")
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baustelle,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = baustCol,
    label = "Baustelle"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Radweg,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "blue",  # line colour
    fillOpacity = 0.8,
    fillColor = radCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste ("Radweg",round(Flaech_verteil$area[Flaech_verteil$Group=="Radweg"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), "%" ,sep=" ")
  )%>%
  leaflet::addPolygons(
    data = Surfaces_subset$Fahrbahn,  # polygon data from geojson
    weight = 0,  # line thickness
    opacity = 1,  # line transparency
    color = "Purple",  # line colour
    fillOpacity = mapOp,
    fillColor = fahrbCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label=   paste ("Fahrbahn",round(Flaech_verteil$area[Flaech_verteil$Group=="Fahrbahn"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), "%" ,sep=" ")
    )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehweg,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 0.8,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gehwCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste ("Gehweg",round(Flaech_verteil$area[Flaech_verteil$Group=="Gehweg"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), "%" ,sep=" ")
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehwegsueberfahrt,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gehwuebCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = "Gehwegsüberfahrt"
    )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Parkplatzflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = parkCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste ("Parkplatz",round(Flaech_verteil$area[Flaech_verteil$Group=="Parkplatz"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), "%" ,sep=" ")  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$oeffentlicherPlatz,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = oeffplCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste ("öffentlicher Platz",round(Flaech_verteil$area[Flaech_verteil$Group=="öffentlicher Platz"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), "%" ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenanlagen,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gruenanlCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste ("Grünanlagen",round(Flaech_verteil$area[Flaech_verteil$Group=="Grünanlagen"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), "%" ,sep=" ")
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gruenflCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste ("Grünfläche",round(Flaech_verteil$area[Flaech_verteil$Group=="Grünfläche"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), "%" ,sep=" ")
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruendach_Geb,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gruendCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = "Gründach"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Spielplaetze,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "pink",  # line colour
    fillOpacity = mapOp,
    fillColor = spielpCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste ("Spielplatz",round(Flaech_verteil$area[Flaech_verteil$Group=="Spielplatz"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), "%" ,sep=" ")  # LAD name as a hover label
  # )%>%
  # leaflet::addCircles(
  #   data =Surfaces_subset$Baeume,  # LAD polygon data from geojson
  #   radius= Surfaces_subset$Baeume$kronedurch/2,
  #   weight = 1,  # line thickness
  #   opacity = 1,  # line transparency
  #   color = "green",  # line colour
  #   fillOpacity = mapOp,
  #   fillColor = treeCol,
  #   labelOptions = labelOptions(noHide = F, textsize = "25px"),
  #   label = paste(Surfaces_subset$Baeume$art_dtsch, ",", "Standalter" ,Surfaces_subset$Baeume$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
  # )%>%
  # leaflet::addCircles(
  #   data =Surfaces_subset$Baeume_anl,  # LAD polygon data from geojson
  #   radius= Surfaces_subset$Baeume_anl$kronedurch/2,
  #   weight = 1,  # line thickness
  #   opacity = 1,  # line transparency
  #   color = "green",  # line colour
  #   fillOpacity = mapOp,
  #   fillColor = treeCol,
  #   labelOptions = labelOptions(noHide = F, textsize = "25px"),
  #   label = paste(Surfaces_subset$Baeume_anl$art_dtsch,",", "Standalter" ,Surfaces_subset$Baeume_anl$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "yellow",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste ("Baumscheiben",round(Flaech_verteil$area[Flaech_verteil$Group=="Baumscheiben"]/Flaech_verteil$area[Flaech_verteil$Group=="Gesamtfläche"]*100,digits=2), "%" ,sep=" ")  # LAD name as a hover label
  )







# karte <- map %>%
#   leaflet::addPolygons(
#     data =kiez_oi,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "green",  # line colour
#     fillOpacity = 0.2,
#     fillColor = "green",
#     label = paste("Wrangelkiez, Gesamtfläche",round(st_area(kiez_oi)), "m²",sep = " "), # LAD name as a hover label
#     labelOptions = labelOptions(noHide = F, textsize = "25px")
#     )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Gebaeude,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "purple",  # line colour
#     fillOpacity = mapOp,
#     fillColor = hausCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste("Gebaeude, Gründach", Surfaces_subset$Gebaeude$GRUENDACH,  sep = " ")  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Baustelle,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "green",  # line colour
#     fillOpacity = mapOp,
#     fillColor = baustCol,
#     label = "Baustelle"  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Radweg,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "blue",  # line colour
#     fillOpacity = 0.8,
#     fillColor = radCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste("Radweg aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Radweg$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data = Surfaces_subset$Fahrbahn,  # polygon data from geojson
#     weight = 0,  # line thickness
#     opacity = 1,  # line transparency
#     color = "Purple",  # line colour
#     fillOpacity = mapOp,
#     fillColor = fahrbCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label= paste("Fahrbahn aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Fahrbahn$material, surfaceMaterial2$material)], Surfaces_subset$Adressen$str_name[st_nearest_feature(Surfaces_subset$Fahrbahn,Surfaces_subset$Adressen)], sep = " ")
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Gehweg,  # LAD polygon data from geojson
#     weight = 0.5,  # line thickness
#     opacity = 0.8,  # line transparency
#     color = "green",  # line colour
#     fillOpacity = mapOp,
#     fillColor = gehwCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste("Gehweg aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehweg$material, surfaceMaterial2$material)],sep=" ")  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Gehwegsueberfahrt,  # LAD polygon data from geojson
#     weight = 0.5,  # line thickness
#     opacity = 1,  # line transparency
#     color = "green",  # line colour
#     fillOpacity = mapOp,
#     fillColor = gehwuebCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste("Gehwegsüberfahrt aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehwegsueberfahrt$material, surfaceMaterial2$material)],sep=" ")  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Parkplatzflaeche,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "gray",  # line colour
#     fillOpacity = mapOp,
#     fillColor = parkCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste("Parkplatz aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Parkplatzflaeche$material, surfaceMaterial2$material)],round(Surfaces_subset$Parkplatzflaeche$flaeche/12), "Stellplätze" , sep = " ")  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$oeffentlicherPlatz,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "green",  # line colour
#     fillOpacity = mapOp,
#     fillColor = oeffplCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste("öffentlicher Platz, Belag aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$oeffentlicherPlatz$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Gruenanlagen,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "green",  # line colour
#     fillOpacity = mapOp,
#     fillColor = gruenanlCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = "Grünanlagen"  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Gruenflaeche,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "green",  # line colour
#     fillOpacity = mapOp,
#     fillColor = gruenflCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = "Grünfläche"  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Gruendach_Geb,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "green",  # line colour
#     fillOpacity = mapOp,
#     fillColor = gruendCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste("Gründach:", Surfaces_subset$Gruendach_Geb$GRUEN_KAT ,sep = " ")  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Spielplaetze,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "pink",  # line colour
#     fillOpacity = mapOp,
#     fillColor = spielpCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = "Spielplatz"  # LAD name as a hover label
#   )%>%
#   leaflet::addCircles(
#     data =Surfaces_subset$Baeume,  # LAD polygon data from geojson
#     radius= Surfaces_subset$Baeume$kronedurch/2,
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "green",  # line colour
#     fillOpacity = mapOp,
#     fillColor = treeCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste(Surfaces_subset$Baeume$art_dtsch, ",", "Standalter" ,Surfaces_subset$Baeume$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
#   )%>%
#   leaflet::addCircles(
#     data =Surfaces_subset$Baeume_anl,  # LAD polygon data from geojson
#     radius= Surfaces_subset$Baeume_anl$kronedurch/2,
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "green",  # line colour
#     fillOpacity = mapOp,
#     fillColor = treeCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste(Surfaces_subset$Baeume_anl$art_dtsch,",", "Standalter" ,Surfaces_subset$Baeume_anl$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
#   )%>%
#   leaflet::addPolygons(
#     data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "yellow",  # line colour
#     fillOpacity = mapOp,
#     fillColor = treeCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste("Baumscheibe", round(st_area(Surfaces_subset$Baumscheiben),digits = 1),"m²" ,sep = " ")  # LAD name as a hover label
#   )

#übersichts und auswahl map
kiezmap <- mapOV %>%
  leaflet::addPolygons(
    data =Kieze,  # LAD polygon data from geojson
    weight = .7,  # line thickness
    opacity = 1,  # line transparency
    color = "grey",  # line colour
    fillOpacity = 0.2,
    fillColor = "red",
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste(Kieze$BEZIRKSNAME, Kieze$PLANUNGSRAUM, sep = " "),
    layerId = ~PLANUNGSRAUM
  )%>%
  leaflet::addPolygons(
    data =kiez_oi,  # LAD polygon data from geojson
    weight = .7,  # line thickness
    opacity = 1,  # line transparency
    color = "grey",  # line colour
    fillOpacity = 0.7,
    fillColor = "yellow",
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("vorausgewählt:", kiez_oi$PLANUNGSRAUM, sep=" "))

#Versiegelungkarte----------------------------------------
#Karte mit Flächen in der Farbe der Versiegelungsarten

Versiegelungskarte <- map %>%
  leaflet::addPolygons(
    data =kiez_oi,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = 0.1,
    fillColor = "gray",
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Wrangelkiez, Gesamtfläche",round(st_area(kiez_oi)), "m²",sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gebaeude,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "purple",  # line colour
    fillOpacity = 0.5,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gebaeude$material,surfaceMaterial2$material)]) ,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Gebaeude, Gründach", Surfaces_subset$Gebaeude$GRUENDACH,  sep = " ")  # LAD name as a hover label
    )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baustelle,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Baustelle$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = "Baustelle"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Radweg,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "blue",  # line colour
    fillOpacity = 0.5,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Radweg$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Radweg aus", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Radweg$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data = Surfaces_subset$Fahrbahn,  # polygon data from geojson
    weight = 0,  # line thickness
    opacity = 1,  # line transparency
    color = "Purple",  # line colour
    fillOpacity = mapOp,
    fillColor =  pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Fahrbahn$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label= paste("Fahrbahn aus", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Fahrbahn$material, surfaceMaterial2$material)] ,Surfaces_subset$Adressen$str_name[st_nearest_feature(Surfaces_subset$Fahrbahn,Surfaces_subset$Adressen)], sep = "\n")
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehweg,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 0.8,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor =  pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gehweg$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Gehweg aus", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehweg$material, surfaceMaterial2$material)] ,sep="\n")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehwegsueberfahrt,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gehwegsueberfahrt$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Gehwegsüberfahrt aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehwegsueberfahrt$material, surfaceMaterial2$material)],sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Parkplatzflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Parkplatz$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Parkplatz aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Parkplatzflaeche$material, surfaceMaterial2$material)],round(Surfaces_subset$Parkplatzflaeche$flaeche/12), "Stellplätze" , sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$oeffentlicherPlatz,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = "gray",
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Versiegelungsklasse", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$oeffentlicherPlatz$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenanlagen,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gruenanlagen$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = "Grünanlagen"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gruenflaeche$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = "Grünfläche"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruendach_Geb,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = gruendCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Gründach:", Surfaces_subset$Gruendach_Geb$GRUEN_KAT ,sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Spielplaetze,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Spielplaetze$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = "Spielplatz"  # LAD name as a hover label
  )%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume,  # LAD polygon data from geojson
    radius= 0.5,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste(Surfaces_subset$Baeume$art_dtsch, "Standalter: " ,Surfaces_subset$Baeume$standalter, " Jahre" ,sep="")  # LAD name as a hover label
  )%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume_anl,  # LAD polygon data from geojson
    radius= .5,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste(Surfaces_subset$Baeume_anl$art_dtsch,",", "Standalter" ,Surfaces_subset$Baeume_anl$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Baumscheiben$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Baumscheibe", round(st_area(Surfaces_subset$Baumscheiben),digits = 1),"m²" ,sep = " ")  # LAD name as a hover label
  )
#Versiegelungskarte


# Urbaner Boden Karte-------------------------------------------

BS_select <-Surfaces_subset$Baumscheiben #select all
BS_trans <-  st_transform(BS_select, crs=5243)#ETRS89 / LCC Germany (E-N)




UrbaneBodenKarte <- map %>%
  leaflet::addPolygons(
    data =Surfaces_subset$Boden,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = 0.2,
    fillColor = "gray",
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Bodentyp in diesem Block:" ,Surfaces_subset$Boden$btyp, "Bodenausgangsmaterial:", Surfaces_subset$Boden$ausgangsm  ,sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =subset(Surfaces_subset$Gebaeude, Surfaces_subset$Gebaeude$GRUENDACH=="vorhanden"),  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = 0.5,
    fillColor = treeCol 
    #label = paste("Gebaeude, Gründach", Surfaces_subset$Gebaeude$GRUENDACH,  sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baustelle,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = mapOp,
    fillColor = "gray",
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = "Baustelle"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Radweg,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = 0.5,
    fillColor = "gray"
    #label = paste("Radweg aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Radweg$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data = Surfaces_subset$Fahrbahn,  # polygon data from geojson
    weight = 0,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = mapOp,
    fillColor =  "gray"
    #label= paste("Fahrbahn aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Fahrbahn$material, surfaceMaterial2$material)], Surfaces_subset$Adressen$str_name[st_nearest_feature(Surfaces_subset$Fahrbahn,Surfaces_subset$Adressen)], sep = " ")
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehweg,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 0.8,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = mapOp,
    fillColor =  "gray"
   # label = paste("Gehweg aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehweg$material, surfaceMaterial2$material)], surfaceMaterial2$farbe[match(Surfaces_subset$Gehweg$material,surfaceMaterial2$material)],sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gehwegsueberfahrt,  # LAD polygon data from geojson
    weight = 0.5,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = mapOp,
    fillColor = "gray"
    # label = paste("Gehwegsüberfahrt aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Gehwegsueberfahrt$material, surfaceMaterial2$material)],sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Parkplatzflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray29",  # line colour
    fillOpacity = mapOp,
    fillColor = "gray"
    # label = paste("Parkplatz aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$Parkplatzflaeche$material, surfaceMaterial2$material)],round(Surfaces_subset$Parkplatzflaeche$flaeche/12), "Stellplätze" , sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$oeffentlicherPlatz,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = "gray",
   # label = paste("öffentlicher Platz, Belag aus:", surfaceMaterial2$Bezeichnung[match(Surfaces_subset$oeffentlicherPlatz$material, surfaceMaterial2$material)] ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenanlagen,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gruenanlagen$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = "teilverisegelter Urbaner Boden in Grünanlagen"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruenflaeche,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Gruenflaeche$material,surfaceMaterial2$material)]),
    label = "Urbaner Boden in Grünfläche"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Gruendach_Geb,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = gruendCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Urbanes Bodensubstrat auf Gründach:", Surfaces_subset$Gruendach_Geb$GRUEN_KAT ,sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Spielplaetze,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Spielplaetze$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = "Teilversiegelter urbaner Boden auf Spielplatz"  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = pal_VG(surfaceMaterial2$order[match(Surfaces_subset$Baumscheiben$material,surfaceMaterial2$material)]),
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Urbaner Boden in Baumscheibe", round(st_area(Surfaces_subset$Baumscheiben),digits = 1),"m²" ,sep = " ")  # LAD name as a hover label
  )%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume,  # LAD polygon data from geojson
    radius= 0.8,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol
   # label = paste(Surfaces_subset$Baeume$art_dtsch, ",", "Standalter" ,Surfaces_subset$Baeume$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume_anl,  # LAD polygon data from geojson
    radius= .8,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "gray",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol
    #label = paste(Surfaces_subset$Baeume_anl$art_dtsch,",", "Standalter" ,Surfaces_subset$Baeume_anl$standalter, "Jahre" ,sep=" ")  # LAD name as a hover label
  )
#UrbaneBodenkarte


#Bäume karte -----------
Baumkarte <- map %>%
leaflet::addCircles(
  data =Surfaces_subset$Baeume,  # LAD polygon data from geojson
  radius= Surfaces_subset$Baeume$kronedurch/2,
  weight = 1,  # line thickness
  opacity = 1,  # line transparency
  color = "green",  # line colour
  fillOpacity = mapOp,
  fillColor = treeCol,
  labelOptions = labelOptions(noHide = F, textsize = "25px"),
  label = paste(Surfaces_subset$Baeume$art_dtsch, "Standalter:" ,Surfaces_subset$Baeume$standalter, "Jahre," ,"Kronendurchmesser:",Surfaces_subset$Baeume_anl$kronedurch, "m" ,sep=" ")  # LAD name as a hover label
)%>%
  leaflet::addCircles(
    data =Surfaces_subset$Baeume_anl,  # LAD polygon data from geojson
    radius= Surfaces_subset$Baeume_anl$kronedurch/2,
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = "green",  # line colour
    fillOpacity = mapOp,
    fillColor = treeCol,
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste(Surfaces_subset$Baeume_anl$art_dtsch, ", Standalter" ,Surfaces_subset$Baeume_anl$standalter, "Jahre,","Kronendurchmesser:",Surfaces_subset$Baeume_anl$kronedurch, "m" ,sep=" ")  # LAD name as a hover label
  )%>%
  leaflet::addPolygons(
    data =Surfaces_subset$Baumscheiben,  # LAD polygon data from geojson
    weight = 1,  # line thickness
    opacity = 1,  # line transparency
    color = gray,  # line colour
    fillOpacity = mapOp,
    fillColor = "yellow",
    labelOptions = labelOptions(noHide = F, textsize = "25px"),
    label = paste("Baumscheibe", round(st_area(Surfaces_subset$Baumscheiben),digits = 1),"m²" ,sep = " ")  # LAD name as a hover label
  )



#-----------------parkplätze Xhain



### 2 % der kartierten Parkplätze in Kreuzberg

#Parkplätze, die im Straßenbefahrungsdatensatz von 2014 als Parkplatz klassifiziert sind.



#map2 %>%
  
#  leaflet::addPolygons(
 #   data =Xhain,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "purple",  # line colour
#     fillOpacity = mapOp,
#     fillColor = hausCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste ("Xhain, Gesamtfläche: ", round(st_area(Xhain),digits = 1), "m²" ,sep=" ")
#   ) %>%
#   leaflet::addPolygons(
#     data =XhainSubset$Parkplatzflaeche,  # LAD polygon data from geojson
#     weight = 1,  # line thickness
#     opacity = 1,  # line transparency
#     color = "blue",  # line colour
#     fillOpacity = 0.8,
#     fillColor = parkCol,
#     labelOptions = labelOptions(noHide = F, textsize = "25px"),
#     label = paste ("Parkplatz aus ", surfaceMaterial2$Bezeichnung[match(XhainSubset$Parkplatzflaeche$material,surfaceMaterial2$material)],sep=" ") 
#   )
# 
# 
# 
# 
# 
# paste("Gesamtfläche der Parkplätze:", round( sum(st_area(XhainSubset$Parkplatzflaeche)),digits = 1)," m²" , "Zu entsiegelnde Fläche auf Parkplätzen (2% pro Jahr)" , round(
#   sum(st_area(XhainSubset$Parkplatzflaeche))*0.02
#   ,digits = 1)," m²" , sep = " ")


