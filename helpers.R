

Kieze <- st_read("data/lor_planungsraeume.geojson")
Surfaces_subset <- readRDS("Surfaces_subset.rds")
surfaceMaterial2 <- read_delim("data/material2.txt", ";", escape_double = FALSE, trim_ws = TRUE)
kiez_oi <- subset(Kieze, PLANUNGSRAUM == "Wrangelkiez" )
Surfaces_subset[["Baustelle"]][["material"]] <- 17
Surfaces_subset[["Gruenanlagen"]][["material"]] <- 16
Surfaces_subset[["Gruenflaeche"]][["material"]] <- 15
Surfaces_subset[["Gehweg"]][["VersFarbe"]] <- surfaceMaterial2$farbe[match(Surfaces_subset$Gehweg$material,surfaceMaterial2$material)]






parkCol <-rgb(33, 94, 143, maxColorValue = 255)
radCol <- rgb(255,0,0, maxColorValue = 255)
treeCol <- rgb(107,171,5, maxColorValue = 255)

hausCol <- rgb(128,0,128, maxColorValue = 255)
baustCol <- rgb(255,0,0 ,maxColorValue = 255)
fahrbCol <- rgb(128,128,128 ,maxColorValue = 255)
gehwCol <- rgb(255,255,0 ,maxColorValue = 255)
kiezCol <-  rgb(0,128,0, maxColorValue = 255)
spielpCol <-  rgb(255,192,203, maxColorValue = 255)
gruenanlCol <- rgb(0,255,0 ,maxColorValue = 255)
gruenflCol <-rgb(0,255,0 ,maxColorValue = 255)
gruendCol <-rgb(0,255,0 ,maxColorValue = 255)
oeffplCol <- rgb(128,128,128 ,maxColorValue = 255)
gehwuebCol <- rgb(255,165,0,maxColorValue = 255)
gehwCol <-rgb(255,255,0,maxColorValue = 255)

flaechTypFarben <- list(parkCol = rgb(33, 94, 143, maxColorValue = 255),
                        radCol = rgb(255,0,0, maxColorValue = 255))

farbliste <- list(FTfarbe = flaechTypFarben  )


mapOp <- 0.5

# VG_class_0 <-rgb(242,240,247, maxColorValue = 255)
# VG_class_1 <-rgb(203,201,226, maxColorValue = 255)
# VG_class_2 <-rgb(158,154,200, maxColorValue = 255)
# VG_class_3 <-rgb(117,107,177, maxColorValue = 255)
# VG_class_4 <-rgb(84,39,143, maxColorValue = 255)

VG_class_0 <-treeCol  #1    
VG_class_1 <-"darkolivegreen1" #2
VG_class_2 <-'yellow'#4
VG_class_3 <-'orange2' #4
VG_class_4 <-'coral3' #5
VG_class_NA <- "gray69"


#VGklassenFarben<- list(VG_class_0 = treeCol , #1    
#                      VG_class_1 ='#80cdc1' #2
#)


labeldata <- surfaceMaterial2[order(surfaceMaterial2$order),] 
#labeldata <- labeldata[order(newdata$V_grad),] 



pal_VG <- colorFactor(
  c("#6BAB05",  "darkolivegreen3",   "darkolivegreen1", "darkolivegreen2"  ,"yellow3",      "yellow2",  
    "yellow"  ,  "lightgoldenrod1",   "orange",  "orange1" ,  "orange2", "orange3" , 
    "coral2" ,    "coral3",     "coral1",   "coral",   "orangered3"  , "gray69"),
  domain = labeldata$order
) 


VGartFarben <- list(
  col_mat_geb = "orangered3",  # 0;  Gebaeude;4
  col_mat_bet = "coral3", # 1;   Beton;4
  col_mat_asph = "coral2", # 2;   Asphalt;4
  col_mat_mos = "yellow3", # 3;   Mosaikpflaster (ca. 50/50mm);2
  col_mat_klStpfl = "yellow2", # 4;   Kleinsteinpflaster (ca. 90/90 mm);2
  col_mat_grStpfl = "lightgoldenrod1" ,# 5;   Grosssteinpflaster (ca. 160/160-220mm bzw. 120/120-180mm);2
  col_mat_betPf = "yellow" ,# 6;   Betonpflaster;2
  col_mat_gehwPlB = "orange", # 7;   Gehwegplatten Beton (350/350mm);3
  col_mat_gewhPlN =  "orange1" ,# 8;   Gehwegplatten Naturstein;3
  col_mat_grGranPl =  "orange2", # 9;   Granitplatte Grossformat (alt/neu);3
  col_mat_grBetPl =  "orange3", # 10;  Betonplatte Grossformat (>350/350mm);3
  col_mat_aspAP = "coral", # 11;  Asphaltueberzug auf Pflaster;4
  col_mat_aspAB = "coral1", # 12;  Asphaltueberzug auf Beton;4
  col_mat_wasgebD = "darkolivegreen2", # 13;  wassergebunden Decke;1
  col_mat_unvers = "darkolivegreen3", # 14;  unversiegelt (Sand etc.);0
  col_mat_gruen = rgb(107,171,5, maxColorValue = 255), # 15;  Gruen;0
  col_mat_befMisch = "darkolivegreen1", # 16;  befestigte Mischflaeche;1
  col_mat_NA = "gray69" # 17;  sonstiges Material;NA
)


























#read out releveant data for further processing---------------------
surface_extensions <- data.frame(Group = character(0),
                                 area = numeric(0),
                                 material = character(0),
                                 class = character(0)
                                 )



baum <-   data.frame(Group = c(rep("Baumscheiben",length(Surfaces_subset$Baumscheiben[["material"]]))),
                     area = as.numeric(st_area(Surfaces_subset$Baumscheiben)),
                     material = Surfaces_subset$Baumscheiben[["material"]],
                     class = surfaceMaterial2$V_grad[match(Surfaces_subset$Baumscheiben[["material"]], surfaceMaterial2$material)]
                     )


gruenfl <-  data.frame(Group = c(rep("Grünfläche",length(Surfaces_subset$Gruenflaeche[["material"]]))),
                       area = as.numeric(st_area(Surfaces_subset$Gruenflaeche)),
                       material = Surfaces_subset$Gruenflaeche[["material"]],
                       class = surfaceMaterial2$V_grad[match(Surfaces_subset$Gruenflaeche[["material"]], surfaceMaterial2$material)]
                      )

gruenanl <-   data.frame(Group = c(rep("Grünanlagen",length(Surfaces_subset$Gruenanlagen[["material"]]))),
                         area = as.numeric(st_area(Surfaces_subset$Gruenanlagen)),
                         material = Surfaces_subset$Gruenanlagen[["material"]],
                         class = surfaceMaterial2$V_grad[match(Surfaces_subset$Gruenanlagen[["material"]], surfaceMaterial2$material)]
                         )


haus <-   data.frame(Group = c(rep("Gebäude",length(Surfaces_subset$Gebaeude[["material"]]))),
                     area = as.numeric(st_area(Surfaces_subset$Gebaeude)),
                     material = Surfaces_subset$Gebaeude[["material"]],
                     class = surfaceMaterial2$V_grad[match(Surfaces_subset$Gebaeude[["material"]], surfaceMaterial2$material)]
                    )


baustel <-   data.frame(Group = c(rep("Baustelle",length(Surfaces_subset$Baustelle[["material"]]))),
                        area = as.numeric(st_area(Surfaces_subset$Baustelle)),
                        material = Surfaces_subset$Baustelle[["material"]],
                        class = surfaceMaterial2$V_grad[match(Surfaces_subset$Baustelle[["material"]], surfaceMaterial2$material)]
                        )


fahrb <-   data.frame(Group = c(rep("Fahrbahn",length(Surfaces_subset$Fahrbahn[["material"]]))),
                      area = as.numeric(st_area(Surfaces_subset$Fahrbahn)),
                      material = Surfaces_subset$Fahrbahn[["material"]],
                      class = surfaceMaterial2$V_grad[match(Surfaces_subset$Fahrbahn[["material"]], surfaceMaterial2$material)]
                      )


gehw <-   data.frame(Group = c(rep("Gehweg",length(Surfaces_subset$Gehweg[["material"]]))),
                     area = as.numeric(st_area(Surfaces_subset$Gehweg)),
                     material = surfaceMaterial2$V_grad[match(Surfaces_subset$Gehweg[["material"]], surfaceMaterial2$material)],
                     class = surfaceMaterial2$V_grad[match(Surfaces_subset$Gehweg[["material"]], surfaceMaterial2$material)]
                     )


gehwueber <-   data.frame(Group = c(rep("Gehweg",length(Surfaces_subset$Gehwegsueberfahrt[["material"]]))),
                          area = as.numeric(st_area(Surfaces_subset$Gehwegsueberfahrt)),
                          material = Surfaces_subset$Gehwegsueberfahrt[["material"]],
                          class = surfaceMaterial2$V_grad[match(Surfaces_subset$Gehwegsueberfahrt[["material"]], surfaceMaterial2$material)]
                          )


radw <-   data.frame(Group = c(rep("Radweg",length(Surfaces_subset$Radweg[["material"]]))),
                     area = as.numeric(st_area(Surfaces_subset$Radweg)),
                     material = Surfaces_subset$Radweg[["material"]],
                     class = surfaceMaterial2$V_grad[match(Surfaces_subset$Radweg[["material"]], surfaceMaterial2$material)]
                     )

oeffpl <-   data.frame(Group = c(rep("öffentlicher Platz",length(Surfaces_subset$oeffentlicherPlatz[["material"]]))),
                       area = as.numeric(st_area(Surfaces_subset$oeffentlicherPlatz)),
                       material = Surfaces_subset$oeffentlicherPlatz[["material"]],
                       class = surfaceMaterial2$V_grad[match(Surfaces_subset$oeffentlicherPlatz[["material"]], surfaceMaterial2$material)]
                       )

parkpl <-   data.frame(Group = c(rep("Parkplatz",length(Surfaces_subset$Parkplatzflaeche[["material"]]))),
                       area = as.numeric(st_area(Surfaces_subset$Parkplatzflaeche)),
                       material = Surfaces_subset$Parkplatzflaeche[["material"]],
                       class = surfaceMaterial2$V_grad[match(Surfaces_subset$Parkplatzflaeche[["material"]], surfaceMaterial2$material)]
                       )

spielpl <-   data.frame(Group = c(rep("Spielplatz",length(Surfaces_subset$Spielplaetze[["material"]]))),
                        area = as.numeric(st_area(Surfaces_subset$Spielplaetze)),
                        material = Surfaces_subset$Spielplaetze[["material"]],
                        class = surfaceMaterial2$V_grad[match(Surfaces_subset$Spielplaetze[["material"]], surfaceMaterial2$material)]
                        )




#unite to one dataframe                            
spaces <- rbind(surface_extensions, baum,gruenfl, gruenanl, haus, baustel, fahrb, gehw, gehwueber, radw, oeffpl, parkpl, spielpl)
#hist(spaces$area)

spaces$Group <- as_factor(spaces$Group)


#calcualte void space (surfaces with no class, Hofflaechen or missing etc)
sum(as.numeric(spaces$area))

total_space <- as.numeric(st_area(kiez_oi))
unknown_space <- as.numeric(st_area(kiez_oi)) -sum(as.numeric(spaces$area))
public_space <- sum(as.numeric(spaces$area))- sum(as.numeric(spaces$area[spaces$Group=="Gebäude"]))
houses_space <- sum(spaces$area[spaces$Group=="Gebäude"])



bilanz <-data.frame(total_space,unknown_space,public_space,houses_space)

melt(bilanz)

#check validity
total_space - unknown_space - public_space - houses_space

spaces$class <- as.numeric(spaces$class)
groupwizeVG <-   aggregate(
    x = spaces[c("class")],
    by = spaces[c("Group")],
    FUN = mean, na.rm = TRUE
  )

names(groupwizeVG) <- c("Group","VG_group")
#unite material names with corresponding IDs
#spaces$material_names <- surfaceMaterial$Bezeichnung[match(spaces$material, surfaceMaterial$material)]
spaces$material_names <- surfaceMaterial2$Bezeichnung[match(spaces$material, surfaceMaterial2$material)]
spaces$material_names <- as_factor(spaces$material_names)
spaces$farbe <- surfaceMaterial2$farbe[match(spaces$material, surfaceMaterial2$material)]
spaces$groupwise_VG  <- groupwizeVG$VG_group[match(spaces$Group, groupwizeVG$Group)]

spaces$order <- surfaceMaterial2$order[match(spaces$material, surfaceMaterial2$material)]
spaces$kiez <- kiez_oi$PLANUNGSRAUM



#plots for webApp------------------------


# Simple aggregation with one function


 Flaech_verteil <-aggregate(
  x = spaces[c("area")],
  by = spaces[c("Group")],
  FUN = sum, na.rm = TRUE
)



Flaech_verteil <-Flaech_verteil %>% add_row(Group = "priv. Flächen (Hinterhöfe), Wasser" , area = unknown_space)
Flaech_verteil <-Flaech_verteil %>% add_row(Group = "Gesamtfläche" , area = total_space)
Flaech_verteil$colors <- c(treeCol ,gruenflCol , gruenanlCol,  hausCol ,baustCol, fahrbCol ,gehwCol ,radCol ,oeffplCol ,parkCol, spielpCol,  kiezCol, "gray69") 


sum(Flaech_verteil$area[1:length(Flaech_verteil$area)-1])
sum(Flaech_verteil$area[1:length(Flaech_verteil$area)-1]/total_space*100)

#gesamtFlaechenPlot-----------------------------
gesamtFlaechenPlot <- ggplot(data=Flaech_verteil, aes(reorder(Group,-area),area/1000)) + 
  geom_col(color="black",fill = Flaech_verteil$colors,alpha=0.5)+
  geom_text(aes(
    label= paste(round(area/total_space*100, digits = 1), "%",sep="")), 
    position = position_dodge(0.9),
    hjust = -.2)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=1,colour="black"),axis.text.y=element_text(hjust=0,colour="black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="",y= "Gesamtfläche in 1000 m²")+
  scale_y_continuous(limits=c(0,600),breaks = seq(0, 600, by = 50))


#oeffRaumFlaechenPlot-----------------------------
Flaechen_verteil_oeffent <- subset(Flaech_verteil, !(Group %in% c("Gesamtfläche", "Gebäude", "priv. Flächen (Hinterhöfe), Wasser")))


oeffRaumFlaechenPlot <- ggplot(data=Flaechen_verteil_oeffent, aes(reorder(Group,-area),area/1000)) + 
  geom_col(color="black",fill = Flaechen_verteil_oeffent$colors,alpha=0.5)+
  geom_text(aes(
    label= paste(round(area/sum(area)*100, digits = 1), "%",sep="")), 
    position = position_dodge(1),
    hjust = -.2)+
  coord_flip()+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=1,colour="black"),
        axis.text.y=element_text(hjust=0,colour="black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"), # bg of the panel
        plot.background = element_rect(fill = "transparent", color = NA))+ # bg of the plot)+
  labs(x="",y= "Gesamtfläche in 1000 m²")+
  scale_y_continuous(limits=c(0,150),breaks = seq(0, 150, by = 50))











# baumartenPlot-----------------------  


               # Applying aggregate
                          
kiez_tree_div <-aggregate(
  data = Surfaces_subset$Baeume,
    baumid ~ art_dtsch ,
  function(baumid) length(unique(baumid)))
names(kiez_tree_div) <- c("art_dtsch", "n")

baumartenPlot <-   ggplot(data = kiez_tree_div, aes(reorder(art_dtsch,-n),n) ) + 
  geom_col(fill=treeCol, color="black",alpha=0.5)+
  geom_text(aes(label= round(n)),nudge_y= 5)+
  theme_bw()+
  theme(axis.text.x=element_text(angle=45, hjust=1,colour="black"),axis.text.y=element_text(colour="black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="",y= "Anzahl")+
  scale_y_continuous(limits=c(0,100),breaks = seq(0, 100, by = 20))


# ggplot() + 
#   geom_bar(data=Surfaces_subset$Baumscheiben, aes(x=round(flaeche ,digits = 1)))+
#   ggtitle("Größen der Baumscheiben")+
#   theme(legend.position = "none")+
#   labs(x="",y= "Anzahl")+
#   scale_y_continuous(limits=c(0,20),breaks = seq(0, 20, by = 2))+
#   scale_x_continuous(limits=c(0,45),breaks = seq(0, 45, by = 10))



#parkenPlot----------------------------
Surfaces_subset$Parkplatzflaeche$str_name <- Surfaces_subset$Adressen$str_name[st_nearest_feature(Surfaces_subset$Parkplatzflaeche,Surfaces_subset$Adressen)]



totals_parking <-aggregate(
  x = Surfaces_subset$Parkplatzflaeche[c("flaeche")],
  by = list(Surfaces_subset$Parkplatzflaeche$str_name),
  FUN = sum, na.rm = TRUE
)
totals_parking$total <- totals_parking$flaeche/12
totals_parking$str_name <- totals_parking$Group.1
  
parkenPlot <-ggplot(data=totals_parking, aes(reorder(str_name,-total),total)) + 
  geom_col(color="black",fill=parkCol,alpha=0.5)+
  coord_flip()+
  
  geom_text(aes(label= round(total)),nudge_y= 15)+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=0,colour="black"),axis.text.y=element_text(hjust=0,colour="black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="",y= "Anzahl") 


#hier weiter sortieren
kiez_tree_div <-aggregate(
  data = Surfaces_subset$Baeume,
  baumid ~ c(art_dtsch),
  function(baumid) length(unique(baumid)))
names(kiez_tree_div) <- c("art_dtsch", "n")

totals_tree_div <-aggregate(
  data = Surfaces_subset$Baeume,
  art_dtsch ~ c(strname),
  function(art_dtsch) length(unique(art_dtsch)))
  names(totals_tree_div) <- c("strname","n")
 

  kiez_tree_alter_str <-aggregate(
    data = Surfaces_subset$Baeume,
    baumid ~ c(pflanzjahr) ,
    function(baumid) length(unique(baumid)))
  names(kiez_tree_alter_str) <- c("pflanzjahr", "n")
 
  kiez_tree_alter_anl <-aggregate(
    data = Surfaces_subset$Baeume_anl,
    baumid ~ pflanzjahr ,
    function(baumid) length(unique(baumid)))
  names(kiez_tree_alter_anl) <- c("pflanzjahr", "n") 
  


totals_tree <-aggregate(
    data = Surfaces_subset$Baeume,
    baumid ~ c(strname),
    function(baumid) length(unique(baumid)))
    
  names(totals_tree) <- c("strname","n")
  




# baeumePlot------------------------------
baeumePlot <-   ggplot(data=totals_tree, aes(reorder(strname, -n), n)) + 
  geom_col(fill=treeCol, color="black",alpha=0.5)+
    geom_text(aes(label= round(n)),nudge_y= 12)+
    coord_flip()+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=0,colour="black"),axis.text.y=element_text(hjust=0,colour="black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="",y= "Anzahl")

baeumeproStrPlot <- ggplot(data=totals_tree_div, aes(reorder(strname, -n), n)) + 
  geom_col(fill=treeCol, color="black", alpha=0.5)+
  geom_text(aes(label= round(n)),nudge_y= 1)+
  theme_bw()+
  coord_flip()+
  theme(axis.text.x=element_text(hjust=0,colour="black"),axis.text.y=element_text(hjust=0,colour="black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="",y= "Anzahl der Arten")+
  scale_y_continuous(limits=c(0,20),breaks = seq(0, 20, by = 5))

baeumealterPlot_str <- ggplot(data = kiez_tree_alter_anl, aes(x=pflanzjahr, y=n))+
  geom_col(fill=treeCol,  alpha=0.5)+
  theme_bw()+
  coord_flip()+
  theme(axis.text.x=element_text(hjust=0,colour="black"),axis.text.y=element_text(hjust=0,colour="black"),legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="",y= "Anzahl")+
  scale_y_continuous(limits=c(0,30),breaks = seq(0, 30, by = 5)) +
scale_x_continuous(limits=c(1820,2025),breaks = seq(1800, 2025, by = 20))

#ggplotly(baeumealterPlot_str)


baeumealterPlot_anl <- ggplot(data = Surfaces_subset$Baeume_anl, aes(x=pflanzjahr))+
  geom_histogram(binwidth=5, fill=treeCol,  alpha=0.5)+
  theme_bw()+
  coord_flip()+
  theme(legend.position = "none",panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x="",y= "Anzahl")+
  scale_y_continuous(limits=c(0,60),breaks = seq(0, 60, by = 10))+ 
scale_x_continuous(limits=c(1820,2025),breaks = seq(1800, 2025, by = 20))


#baeumealterPlot_str <- ggplot(data = Surfaces_subset$Baeume, aes(x=pflanzjahr))+
#  geom_histogram(fill=treeCol, color="black")
  

paste("Artenvielfalt der Straßenbäume im ausgewählten Kiez:", length(levels(as.factor(Surfaces_subset[["Baeume"]][["art_dtsch"]]))), "Arten" ,sep= " ")
levels(as.factor(Surfaces_subset[["Baeume"]][["art_dtsch"]]))

#Flächenverteilung-------------------------------------------------

#dazu gibt es schon eine Auswertung

#versiegelungsPlot----------------------



spaces$class <- factor(spaces$class, labels = c(0,1,2,3,4))

versPlotalpha <- .8




versiegelungsPlot <- ggplot() + 
#  geom_bar(data=spaces, aes(fill=class, y=area, x=kiez),position="fill", stat="identity",alpha= versPlotalpha)+
  geom_bar(data=spaces, aes(fill=class, y=area, x=reorder(Group, groupwise_VG)),position="fill", stat="identity",alpha= versPlotalpha)+
  labs(x="",y= "Flächenanteil m²/m²")+theme(axis.text.x=element_text(angle=45, hjust=0))+ 
  theme_bw()+
  coord_flip()+
  theme(axis.text.x=element_text(hjust=0,colour="black"),axis.text.y=element_text(hjust=0,colour="black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(name = "Versiegelungsklassen",values = c("0"= VG_class_0, "1"= VG_class_1,"2"= VG_class_2,  "3"=VG_class_3, "4"= VG_class_4, "NA"=VG_class_NA))







 #spaces$Groupsorted <- spaces$Group

versiegelungsPlot2 <-  ggplot() + 
    geom_bar(data=spaces , aes(fill=as.factor(order), y=area, x=reorder(Group, groupwise_VG)),position="fill", stat="identity",alpha=versPlotalpha)+
  labs(x="",y= "Flächenanteil m²/m²")+theme(axis.text.x=element_text(angle=45, hjust=1))+ 
  theme_bw()+
  coord_flip()+
   theme(axis.text.x=element_text(hjust=0,colour="black"),axis.text.y=element_text(hjust=,colour="black"),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank())+
   scale_fill_manual(name = "Materialien",values = c("14"= VGartFarben$col_mat_bet, 
                                                     "13"= VGartFarben$col_mat_asph,
                                                     "16"= VGartFarben$col_mat_geb,
                                                    "5"=VGartFarben$col_mat_mos, 
                                                     "6"= VGartFarben$col_mat_klStpfl, 
                                                     "8"= VGartFarben$col_mat_grStpfl, 	
                                                     "7"= VGartFarben$col_mat_betPf, 
                                                     "9"=VGartFarben$col_mat_gehwPlB, 
                                                     "10"= VGartFarben$col_mat_gewhPlN, 
                                                     "11" = VGartFarben$col_mat_grGranPl, 
                                                     "12"= VGartFarben$col_mat_grBetPl, 
                                                     "17"= VGartFarben$col_mat_aspAP, 
                                                     "15"= VGartFarben$col_mat_aspAB, 
                                                     "4"= VGartFarben$col_mat_wasgebD, 
                                                    "2" = VGartFarben$col_mat_unvers,
                                                     "1"= VGartFarben$col_mat_gruen, 
                                                    "3"= VGartFarben$col_mat_befMisch,
                                                     "18"= VGartFarben$col_mat_NA),
                                       breaks=labeldata$order,
                                      labels=labeldata$Bezeichnung)
 

versiegelungsPlot3 <- ggplot() + 
  geom_bar(data=spaces , aes(fill=as.factor(order), y=area/bilanz$total_space*100, x=order), stat="identity",alpha=versPlotalpha)+
  labs(x="",y= "Flächenanteil in %")+ 
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "none",axis.text.x=element_text(hjust=0,colour="black"),axis.text.y=element_text(hjust=0,colour="black"))+
  scale_x_discrete(limits = factor(labeldata$order), labels = labeldata$order)+
  scale_y_continuous(limits=c(0,40),breaks = seq(0, 40, by = 5)) +
  scale_fill_manual(name = "Materialarten",values = c("14"= VGartFarben$col_mat_bet, 
                                                    "13"= VGartFarben$col_mat_asph,
                                                    "16"= VGartFarben$col_mat_geb,
                                                    "5"=VGartFarben$col_mat_mos, 
                                                    "6"= VGartFarben$col_mat_klStpfl, 
                                                    "8"= VGartFarben$col_mat_grStpfl, 	
                                                    "7"= VGartFarben$col_mat_betPf, 
                                                    "9"=VGartFarben$col_mat_gehwPlB, 
                                                    "10"= VGartFarben$col_mat_gewhPlN, 
                                                    "11" = VGartFarben$col_mat_grGranPl, 
                                                    "12"= VGartFarben$col_mat_grBetPl, 
                                                    "17"= VGartFarben$col_mat_aspAP, 
                                                    "15"= VGartFarben$col_mat_aspAB, 
                                                    "4"= VGartFarben$col_mat_wasgebD, 
                                                    "2" = VGartFarben$col_mat_unvers,
                                                    "1"= VGartFarben$col_mat_gruen, 
                                                    "3"= VGartFarben$col_mat_befMisch,
                                                    "18"= VGartFarben$col_mat_NA),
                    breaks=labeldata$order,
                    labels=paste(labeldata$order,labeldata$Bezeichnung, sep=": "))




#urbanerBodenPlot-------------
gruenDa <- data.frame(Group = rep("Gründach", length(Surfaces_subset$Gruendach_Geb$GRUEN_KAT)),
                                 area = st_area(Surfaces_subset$Gruendach_Geb),
                                 material =  rep(15, length(Surfaces_subset$Gruendach_Geb$GRUEN_KAT)),
                                 class = rep(0, length(Surfaces_subset$Gruendach_Geb$GRUEN_KAT)),
                      material_names = rep("Grün", length(Surfaces_subset$Gruendach_Geb$GRUEN_KAT)),
                      farbe= rep("col_mat_gruen", length(Surfaces_subset$Gruendach_Geb$GRUEN_KAT)),
                      groupwise_VG= rep(0, length(Surfaces_subset$Gruendach_Geb$GRUEN_KAT)),
                      order= rep(1, length(Surfaces_subset$Gruendach_Geb$GRUEN_KAT)),
                     kiez= rep(kiez_oi$PLANUNGSRAUM, length(Surfaces_subset$Gruendach_Geb$GRUEN_KAT))
)
parklet <- data.frame(Group = rep("Parklet", length(1)),
                      area = 0,
                      material =  rep(15, length(1)),
                      class = rep(0, length(1)),
                      material_names = rep("Grün", length(1)),
                      farbe= rep("col_mat_gruen", length(1)),
                      groupwise_VG= rep(0, length(1)),
                      order= rep(1, length(Surfaces_subset$Gruendach_Geb$GRUEN_KAT)),
                      kiez= rep(kiez_oi$PLANUNGSRAUM, length(1))
)
parklet$area[1] <- 5*1.5


#urbBoden <- spaces[spaces$Group == c("Baumscheiben","Spielplatz", "Gruenanlagen", "Gruenflaeche"),]
urbBaum <- spaces[spaces$Group == "Baumscheiben",]
urbSpielplatz <-spaces[spaces$Group == "Spielplatz",]
urbGruenanl <- spaces[spaces$Group == "Grünanlagen",]
urbGruenfl <- spaces[spaces$Group == "Grünanfläche",]


urbBoden <- rbind(gruenDa,parklet,urbBaum,urbSpielplatz,urbGruenanl,urbGruenfl)
urbBoden$area <- as.numeric(urbBoden$area) 
 urbBoden_anteile <-aggregate(
   x = urbBoden[c("area")],
   by = urbBoden[c("Group")],
   FUN = sum, na.rm = TRUE
 )
 
 
 urbBoden_anteile$class <- urbBoden$class[match(urbBoden_anteile$Group, urbBoden$Group)]
 
 urbBoden_anteile <- urbBoden_anteile %>% add_row(Group = "öffentlicher Raum + Gebäude" , area = public_space+sum(spaces[spaces$Group == "Gebäude",]$area), class = "NA")
 
 
 
 urbbodenPlot <-  ggplot(data=urbBoden_anteile, aes(reorder(Group,-area),area/1000)) + 
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


 
 # wichtig: diagram, das flächen relativ zur öffentlichen raum gesamtfläche abbildet. wir mach ich das dann mit dachbegrünung??

#,
 
 BD_Boden <- 1.3 # feuchte Komposterde
