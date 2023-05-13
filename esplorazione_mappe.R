##########
## Esplorazione mappe con R
##########

# Descrizione: questo script permette l'esporazione di mappe in R a partire da shapefiles

# 0. Set-up

library(leaflet)
library(readxl)
library(sf)
library(tidyverse)

# 1. Acquisizione dati

# Scarico lo shapefile delle regioni ISTAT
download.file("https://www.istat.it/storage/cartografia/confini_amministrativi/generalizzati/2023/Limiti01012023_g.zip" , destfile="C:/Users/Nick/Desktop/Nick/progetti/shapefiles/shapefile_italia.zip")
# Lo shapefile contiene le mappe delle regioni, province e comuni italiani al 2023

# Unzip del file scaricato
dir.create("C:/Users/Nick/Desktop/Nick/progetti/shapefiles/shapefile_italia/") # crea il folder in cui salvare lo shapefile
unzip("C:/Users/Nick/Desktop/Nick/progetti/shapefiles/shapefile_italia.zip",
      exdir = "C:/Users/Nick/Desktop/Nick/progetti/shapefiles/shapefile_italia/")  
# unzip dello shapefile

# 2. Caricamento shapefile e cleaning

shape_path <- c("C:/Users/Nick/Desktop/Nick/progetti/shapefiles/shapefile_italia/Limiti01012023_g/Reg01012023_g/")
# definizione del path dove trovo lo shape delle regioni
italia_regioni <- sf::st_read(paste(shape_path, "Reg01012023_g_WGS84.shp", sep = "")) 
# caricamento dello shapefile regionale
# ATTENZIONE: lo step precedente ha richiesto l'unzip manuale della cartella shapefile_italia

# file.exists(paste(shape_path, "Reg01012023_g_WGS84.shp", sep = ""))
# Riga di controllo per verificare che lo shapefile esista

export_colonne <- c("regione", "export_2021", "perc_2021", "export_2022", "perc_2021", "delta_22_21")
ds_export <- readxl::read_xlsx("C:/Users/Nick/Desktop/Nick/progetti/territorio/S_Storiche_IV_trim_2022.xlsx", sheet = "Tabella_2", skip = 5, col_names = export_colonne) %>% as.data.frame()
# caricamento dati export

regioni_remove <- c("Bolzano/Bozen", "Trento", "Sud", "Isole", "Nord-est", "Nord-centro", "Nord-ovest", "Centro", "Sud e Isole", "Province diverse e non specificate", "ITALIA")
ds_export_clean <- ds_export[-which(ds_export$regione %in% regioni_remove),]
# cleaning dati export

ds_export_clean$COD_REG <- recode(ds_export_clean$regione,
                                  "Piemonte" = 1,
                                  "Valle d'Aosta/Vallée d'Aoste" = 2,
                                  "Lombardia" = 3,
                                  "Trentino-Alto Adige/Südtirol" = 4,
                                  "Veneto" = 5,
                                  "Friuli-Venezia Giulia" = 6,
                                  "Liguria" = 7,
                                  "Emilia-Romagna" = 8,
                                  "Toscana" = 9,
                                  "Umbria" = 10,
                                  "Marche" = 11,
                                  "Lazio" = 12,
                                  "Abruzzo" = 13,
                                  "Molise" = 14,
                                  "Campania" = 15,             
                                  "Puglia" = 16,
                                  "Basilicata" = 17,
                                  "Calabria" = 18,             
                                  "Sicilia" = 19,
                                  "Sardegna" = 20)

italia_regioni <- left_join(italia_regioni, ds_export_clean, by = "COD_REG")            
# Metto in join il dataset export pulito con lo shapefile italia
                                                                
# 3. Creazione mappa

# Create a color palette for the map:

mypalette <- colorNumeric( palette="viridis", domain=italia_regioni$export_2022, na.color="transparent")
mypalette(c(45,43))

italia_regioni_wgs84 <- st_transform(italia_regioni, "+init=epsg:4326")
# Traduco le coordinate in formato 4326 (leggibile da leaflet)

# Basic choropleth with leaflet?
m <- leaflet(italia_regioni_wgs84) %>% 
  addTiles()  %>% 
  setView( lat=41.3, lng=12.3 , zoom=4) %>%
  addPolygons( fillColor = ~mypalette(export_2022), stroke=FALSE )

m

# Esplorazione variabile export 2022
italia_regioni_wgs84 %>% 
  ggplot( aes(x=as.numeric(export_2022))) + 
  geom_histogram(bins=20, fill='#69b3a2', color='white') +
  xlab("Export (mln)") + 
  theme_bw()

# Esplorazione di tipologie di colore
m <- leaflet(italia_regioni_wgs84)%>% addTiles()  %>% setView(lat=41.3, lng=12.3 , zoom=4) %>%
  addPolygons( stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5, color = ~colorBin("YlOrRd", export_2022)(export_2022) )
m

# Creazione di una mappa choroplettica personalizzata

# Creiamo una palette di colori con intervalle ad hoc
library(RColorBrewer)
mybins <- c(0,25000,50000,100000,Inf)
mypalette <- colorBin( palette="YlOrBr", domain=italia_regioni_wgs84$export_2022, na.color="transparent", bins=mybins)

# Prepariamo il testo per i tooltip:
mytext <- paste(
  "Regione: ",   italia_regioni_wgs84$DEN_REG,"<br/>", 
  "Export 2022: ", round(italia_regioni_wgs84$export_2022, 1), 
  sep="") %>%
  lapply(htmltools::HTML)

# Mappa finale versione 1
m <- leaflet(italia_regioni_wgs84) %>% 
  addTiles()  %>% 
  setView(lat=41.3, lng=12.3 , zoom=4) %>%
  addPolygons( 
    fillColor = ~mypalette(export_2022), 
    stroke=TRUE, 
    fillOpacity = 0.9, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=mypalette, values=~export_2022, opacity=0.9, title = "Export 2022 (Mln)", position = "bottomleft" )

m  

# Creazione di una mappa versione 2

# Prepariamo il testo per i tooltip:
label_testo <- paste(
  "Regione: ",   italia_regioni_wgs84$DEN_REG,"<br/>", 
  "Esportazioni: ", round(italia_regioni_wgs84$export_2022, 1), 
  sep="") %>%
  lapply(htmltools::HTML)

# Creazione di elementi referenze e titolo per l'HTML
references <- "<h5>Referenze</h5><a target='_blank' href='https://www.istat.it/it/archivio/282056'><h5>Esportazioni IV Trim 22</h5></a><a target='_blank' href='https://www.istat.it/it/archivio/222527'><h5>Confini amm ISTAT</h5></a>"
htmltitle <- "<h5> Esportazioni IV Trim 2022 per regione</h5>"

# Creo dei bins di esportazione per regione (5 bins per semplicità)
library(RColorBrewer)
mybins <- c(0,1/5, 2/5,3/5, 4/5, 1) * max(italia_regioni_wgs84$export_2022)
mypalette <- colorBin( palette="Blues", domain=italia_regioni_wgs84$export_2022, na.color="transparent", bins=mybins)


factpal <- colorFactor(rev(topo.colors(100)), unique(italia_regioni_wgs84$export_2022))
# Other version of the map
leaflet(italia_regioni_wgs84) %>%
  addPolygons(
    fillColor = ~factpal(italia_regioni_wgs84$export_2022), 
    stroke = TRUE, 
    color = 'Grey', 
    weight = 1.5, 
    label = label_testo,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=factpal, 
             values=~italia_regioni_wgs84$export_2022, 
             labels = italia_regioni_wgs84$DEN_REG,
             opacity=0.3, 
             title = "Esportazioni per regione", 
             position = "bottomleft" 
  ) %>%
  addControl(html=htmltitle, position = "topright") %>%
  addControl(html=references, position = "bottomright")

# Ora con la crescita percentuale dell'export nell'ultimo anno
# Creo dei bins di esportazione per regione (5 bins per semplicità)
library(RColorBrewer)
mybins <- c(0,1/5, 2/5,3/5, 4/5, 1) * max(italia_regioni_wgs84$delta_22_21)
mypalette <- colorBin( palette="Blues", domain=italia_regioni_wgs84$delta_22_21, na.color="transparent", bins=mybins)

label_testo <- paste(
  "Regione: ",   italia_regioni_wgs84$DEN_REG,"<br/>", 
  "Esportazioni: ", round(italia_regioni_wgs84$delta_22_21, 1), 
  sep="") %>%
  lapply(htmltools::HTML)

factpal <- colorFactor(rev(topo.colors(100)), unique(italia_regioni_wgs84$delta_22_21))
# Other version of the map
leaflet(italia_regioni_wgs84) %>%
  addPolygons(
    fillColor = ~factpal(italia_regioni_wgs84$delta_22_21), 
    stroke = TRUE, 
    color = 'Grey', 
    weight = 1.5, 
    label = label_testo,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  ) %>%
  addLegend( pal=factpal, 
             values=~italia_regioni_wgs84$delta_22_21, 
             labels = italia_regioni_wgs84$DEN_REG,
             opacity=0.3, 
             title = "Esportazioni per regione", 
             position = "bottomleft" 
  ) %>%
  addControl(html=htmltitle, position = "topright") %>%
  addControl(html=references, position = "bottomright")

