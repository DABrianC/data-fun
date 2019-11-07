
library(rgdal)
library(tidyverse)
library(readxl)
library(sf)
library(ggmap)
library(ggspatial)
library(ggridges)
library(raster)
library(maptools)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(extrafont)
library(extrafontdb)
library(viridis)
library(mapview)


setwd("W:/Private/bcalhoon/R training/Niger maps")

#import shape file. Type getData('ISO3') to get a list of 3 letter ISO country codes
Niger<- getData("GADM", country="NER", level=3)

#Just checking to see if my shape is what I want. 
plot(Niger)
# Yes, it goes down to communes (or whatever they're called).

#I converted the shapefile from an sp to an sf object. Now I can manipulate it as data.frame
Niger <- sf::st_as_sf(Niger)

Niger <- Niger %>%
  dplyr:: select(NAME_1, NAME_2, NAME_3, geometry)

#import data to plot on map
dat <- read.csv("1900-01-01-2019-07-02-Niger.csv") %>%
  dplyr:: select(event_date
         , year
         , event_type
         , sub_event_type
         , actor1
         , inter1
         , actor2
         , assoc_actor_2
         , inter2
         , interaction
         , admin1
         , admin2
         , admin3
         , location
         , latitude
         , longitude
         , source
         , source_scale
         , fatalities)

#I want to look at the descriptive statistics to see what there is
summary(dat)


#Filtering datasets by sector so that we can make maps that highlight individual sector differences.

Niger.battles <- dat %>%
  filter(event_type == "Battles")


Niger.explos <- dat %>%
  filter(event_type == "Explosions/Remote violence")

Niger.protests <- dat %>%
  filter(event_type == "Protests")

Niger.riots <- dat %>%
  filter(event_type == "Riots")

Niger.strategic <- dat %>%
  filter(event_type == "Strategic developments")

Niger.violence <- dat %>%
  filter(event_type == "Violence against civilians")
#Check for differences across commune names
#dplyr:: setdiff(dat$admin3, Niger2$NAME_3)
#It found 21 differences: Chetimari, Gueskerou, ZR N Guigmi, Abala, Toumour, Diagourou,
# Baroua Gana, Kabelawa, Bani Bangou, N Guigmi, Tillia, Filingue, Dakoro, Matankari,
#Birni N Konni, "", Bangi, Abalak, Gogone, Bagara, Birni N Lalle


                         

#Niger2 <- Niger2 %>%
 # mutate(NAME_3 = recode (NAME_3,
  #                        "N' Guigmi" = "N Guigmi"
   #                       , "ZR N' Guigmi" = "ZR N Guigmi"
    #                      , "Birni N' Lalle" = "Birni N Lalle"
     #                     , "Birni N' Konni" = "Birni N Konni"
      #                    , "Diagorou" = "Diagourou"))





#Niger3 <- left_join(Niger2
 #                   , dat
  #                  , by = c("NAME_3" = "admin3"))
#summary(Niger3)

#anti.niger <- anti_join(dat
 #                       , Niger2
  #                      , by = c("admin3" = "NAME_3"))

#summary(anti.niger)

#I have a bunch of admin3 names that I need to place into 

#anti.niger <- anti.niger %>%
 # mutate(admin3 = recode(admin3
  #                       , "Chetimari" = "Komadougou"
   #                      , ))

#ggplot(Niger3) +
#geom_sf(aes(geometry=geometry)) +
 # geom_sf_text(aes(label = NAME_3)) +
  #geom_point(data = anti.niger, aes(x = longitude
   #                          , y = latitude)
    #          , color = "red") +
  #geom_text_repel(data = anti.niger, aes(x = longitude, y = latitude
   #                                , label = admin3)
    #        , size = 3.9, col = "blue", fontface = "bold") 

#The st_coordinates function pulls the lon and lat out into X and Y variables for plotting
#This will be useful for labels if we want to label the communes
Commune.centers <- cbind(Niger, st_coordinates(st_centroid(Niger$geometry)))

jitter <- position_jitter()

library(ggthemes)
#This map actually works. It has too many labels, but that's ok for now.
Map1 <- ggplot() +
  geom_sf(data = Niger, aes(geometry = geometry)
              , fill = "#E0E0E0"
              , color = "white")+
   geom_point(data = dat, aes(x = longitude, y = latitude
             , color = event_type
             , size = fatalities)
             , alpha = .6) +
             scale_color_viridis_d(name = "Conflict Event") +
             scale_size_continuous(name = "Number of Fatalities")+
  labs(title = "Niger Conflict Incidents, 1997 - 2019"
       , caption = "By Brian Calhoon, using data from www.acleddata.com")+
  theme(plot.title = element_text(family = "Arial"
                                    , size = 18
                                    , face = "bold")
        , legend.text = element_text(family = "Arial"
                                     , size = 12)
        , legend.title = element_text(family = "Arial"
                                      , size = 14)
        , legend.position = "right"
        , plot.background = element_blank()
        , rect = element_blank()
        , panel.background = element_blank()
        , panel.grid = element_line(color = "white")
        , axis.title = element_blank()
        , axis.line = element_blank()
        , axis.text = element_blank()
        , axis.ticks = element_blank()
        , plot.caption = element_text(family = "Arial"
                                      , size = 10))
  
Map1
 

#-----Interactive map-------------------
#This pal is used in the leaflet map below.
pal <- colorFactor(palette = c("#1B9E77"
                               , "#D95F02"
                               , "#7570B3"
                               , "#E7298A"
                               , "#66A61E"
                               , "#E6AB02"),
                   levels = c("Battles"
                              , "Explosions/Remote violence"
                              , "Protests"
                              , "Riots"
                              , "Strategic developments"
                              , "Violence against civilians"))

#These are the codes used in the color palette
brewer.pal(n = 6, name = "Dark2")

Nigerconflict <- leaflet() %>%
  addProviderTiles(provider = "CartoDB", group = "Carto") %>%
  addProviderTiles(provider = "Esri", group = "Esri") %>%
  addPolygons(data = Niger
              , color = "#444444"
              , weight = 1
              , smoothFactor = .5
              , opacity = 1.0
              , label = Niger$NAME_3
              , fillOpacity = .1) %>%
  addCircleMarkers(lng = Niger.battles$longitude
             , lat = Niger.battles$latitude
             , color = pal(Niger.battles$event_type)
             , group = "Battles"
             , popup = paste0("<b>Location:</b> " , Niger.battles$location
                              , "<br/>"
                              , "<b>Event:</b> " ,Niger.battles$event_type
                              , "<br/>"
                              , "<b>Sub-event:</b> " , Niger.battles$sub_event_type
                              , "<br/>"
                              , "<b>Actors:</b> " , Niger.battles$actor1, "<br/>", Niger.battles$actor2, "<br/>", Niger.battles$assoc_actor_2
                              , "<br/>"
                              , "<b>Fatalities: </b> " ,Niger.battles$fatalities
                              , "<br>"
                              , "<b>Date:</b> " ,Niger.explos$event_date) 
             , radius = 1) %>%
  addCircleMarkers(lng = Niger.explos$longitude
                   , lat = Niger.explos$latitude
                   , color = pal(Niger.explos$event_type)
                   , group = "Explosions/Remote violence"
                   , popup = paste0("<b>Location:</b> " , Niger.explos$location
                                    , "<br/>"
                                    , "<b>Event:</b> " ,Niger.explos$event_type
                                    , "<br/>"
                                    , "<b>Sub-event:</b> " ,Niger.explos$sub_event_type
                                    , "<br/>"
                                    , "<b>Actors:</b> " ,Niger.explos$actor1, "<br/>", Niger.explos$actor2, "<br/>", Niger.explos$assoc_actor_2
                                    , "<br/>"
                                    , "<b>Fatalities: </b> " ,Niger.explos$fatalities
                                    , "<br>"
                                    , "<b>Date:</b> " ,Niger.explos$event_date) 
                   , radius = 1) %>%
  addCircleMarkers(lng = Niger.protests$longitude
                   , lat = Niger.protests$latitude
                   , color = pal(Niger.protests$event_type)
                   , group = "Protests"
                   , popup = paste0("<b>Location:</b> " , Niger.protests$location
                                    , "<br/>"
                                    , "<b>Event:</b> " ,Niger.protests$event_type
                                    , "<br/>"
                                    , "<b>Sub-event:</b> " ,Niger.protests$sub_event_type
                                    , "<br/>"
                                    , "<b>Actors:</b> " ,Niger.protests$actor1, "<br/>", Niger.protests$actor2, "<br/>", Niger.protests$assoc_actor_2
                                    , "<br/>"
                                    , "<b>Fatalities: </b> " ,Niger.protests$fatalities
                                    , "<br>"
                                    , "<b>Date:</b> " ,Niger.protests$event_date) 
                   , radius = 1) %>%
  addCircleMarkers(lng = Niger.riots$longitude
                   , lat = Niger.riots$latitude
                   , color = pal(Niger.riots$event_type)
                   , group = "Riots"
                   , popup = paste0("<b>Location:</b> " , Niger.riots$location
                                    , "<br/>"
                                    , "<b>Event:</b> " ,Niger.riots$event_type
                                    , "<br/>"
                                    , "<b>Sub-event:</b> " ,Niger.riots$sub_event_type
                                    , "<br/>"
                                    , "<b>Actors:</b> " ,Niger.riots$actor1, "<br/>", Niger.riots$actor2, "<br/>", Niger.riots$assoc_actor_2
                                    , "<br/>"
                                    , "<b>Fatalities: </b> " ,Niger.riots$fatalities
                                    , "<br>"
                                    , "<b>Date:</b> " ,Niger.riots$event_date) 
                   , radius = 1) %>%
  addCircleMarkers(lng = Niger.strategic$longitude
                   , lat = Niger.strategic$latitude
                   , color = pal(Niger.strategic$event_type)
                   , group = "Strategic developments"
                   , popup = paste0("<b>Location:</b> " , Niger.strategic$location
                                    , "<br/>"
                                    , "<b>Event:</b> " ,Niger.strategic$event_type
                                    , "<br/>"
                                    , "<b>Sub-event:</b> " ,Niger.strategic$sub_event_type
                                    , "<br/>"
                                    , "<b>Actors:</b> " ,Niger.strategic$actor1, "<br/>", Niger.strategic$actor2, "<br/>", Niger.strategic$assoc_actor_2
                                    , "<br/>"
                                    ,"<b>Fatalities: </b> " ,Niger.strategic$fatalities
                                    , "<br>"
                                    , "<b>Date:</b> " ,Niger.strategic$event_date) 
                   , radius = 1) %>%
  addCircleMarkers(lng = Niger.violence$longitude
                   , lat = Niger.violence$latitude
                   , color = pal(Niger.violence$event_type)
                   , group = "Violence against civilians"
                   , popup = paste0("<b>Location:</b> " , Niger.violence$location
                                    , "<br/>"
                                    , "<b>Event:</b> " ,Niger.violence$event_type
                                    , "<br/>"
                                    , "<b>Sub-event:</b> " ,Niger.violence$sub_event_type
                                    , "<br/>"
                                    , "<b>Actors:</b> " ,Niger.violence$actor1, "<br/>", Niger.violence$actor2, "<br/>", Niger.violence$assoc_actor_2
                                    , "<br/>"
                                    , "<b>Fatalities: </b> " ,Niger.violence$fatalities
                                    , "<br>"
                                    , "<b>Date:</b> " ,Niger.violence$event_date) 
                   , radius = 1) %>%
  addLegend(title = "Event Type"
            , pal = pal
            , values = c("Battles"
                         , "Explosions/Remote violence"
                         , "Protests"
                         , "Riots"
                         , "Strategic developments"
                         , "Violence against civilians")
            , position = "topleft") %>%
  addLayersControl(
    baseGroups = c("Carto"
                   , "Esri")
                   , position = "topleft"
    , overlayGroups = c("Battles"
                      , "Explosions/Remote violence"
                      , "Protests"
                      , "Riots"
                      , "Strategic developments"
                      , "Violence against civilians"))
 
  
  mapshot(Nigerconflict, file = "Nigerconflict.html", path = ".")
  

#-------An animated map over time----------------------
library(gganimate)
library(gifski)
library(extrafont)

Nigertime <- ggplot(Niger) +
  geom_sf(aes(geometry = geometry)
          , fill = "#E0E0E0"
          , color = "white")+
  geom_point(data = dat
             , aes(x = dat$longitude
                , y = dat$latitude
                , color = dat$event_type
                , size = dat$fatalities)
                , alpha = .6) +
  labs(title = "Niger Conflict Incidents by Year: {frame_time}"
       , caption = "By Brian Calhoon, using data from www.acleddata.com") +
  theme(plot.title = element_text(family = "Arial"
                                       , size = 18)
             , legend.text = element_text(family = "Arial"
                                          , size = 12)
             , legend.title = element_text(family = "Arial"
                                           , size = 14)
             , legend.position = "right"
             , plot.background = element_blank()
             , rect = element_blank()
             , panel.background = element_blank()
             , panel.grid = element_line(color = "white")
             , axis.title = element_blank()
             , axis.line = element_blank()
             , axis.text = element_blank()
             , axis.ticks = element_blank()
             , plot.caption = element_text(family = "Arial"
                                           , size = 10))+
  scale_color_viridis_d(name = "Conflict Event")+
  scale_size_continuous(name = "Number of Fatalities")+
  transition_time(dat$year) +
  enter_fade() +
  exit_fade() 
  
animate(Nigertime, width = 600, height = 450
        , fps = 3) 

 anim_save("Niger conflict data over time.gif", animation = last_animation(), path = ".", ) 
