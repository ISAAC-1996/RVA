library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(DT)
library(DBI)
library(odbc)
library(datamodelr)
library(DiagrammeR)
library(writexl)
library(sf)
library(leaflet)
library(shiny)
library(geosphere)
library(shinyjs)
library(openxlsx)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(plotly)
library(leaflet.extras2)

RVA_2023 <- read_excel("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Demandes/output/2024/11_2024/Carte_RVA/Cart_de_france_RVA_2023.xlsx")
RVA_2023 <- as_tibble(RVA_2023)

RVA_2024 <- read_excel("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Demandes/output/2024/11_2024/Carte_RVA/Cart_de_france_RVA_2024.xlsx")
RVA_2024 <- as_tibble(RVA_2024)

Departement_France <- read_excel("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Demandes/output/2024/11_2024/Carte_RVA/Les departements de France.xlsx")
Departement_France <- as_tibble(Departement_France)

RVA <- Departement_France%>%
  left_join(RVA_2023, by=c("code" = "cp"))%>%
  left_join(RVA_2024, by=c("code" = "cp"))%>%
  select(-code)%>%
  filter(!is.na(nbre_phies_2023)) # Pour supprimer les données manquantes.
str(RVA)



# Charger les bibliothèques nécessaires
library(dplyr)
library(leaflet)
library(sf)

# Charger les données géographiques des départements
departements <- st_read("https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/departements.geojson")

# Harmoniser les noms des départements pour la jointure
departements$nom <- trimws(tolower(departements$nom))
RVA$nom <- trimws(tolower(RVA$nom))

# Jointure des données géographiques avec les données de RVA
map_data <- departements %>%
  left_join(RVA, by = c("nom" = "nom")) %>%
  filter(!is.na(volume_rva_2023) & !is.na(volume_rva_2024))

# Calculer les pourcentages pour 2023 et 2024
total_rva_2023 <- sum(map_data$volume_rva_2023, na.rm = TRUE)
total_rva_2024 <- sum(map_data$volume_rva_2024, na.rm = TRUE)

map_data <- map_data %>%
  mutate(
    rva_2023_percent = (volume_rva_2023 / total_rva_2023) * 100,
    rva_2024_percent = (volume_rva_2024 / total_rva_2024) * 100
  )

# Générer la carte interactive
leaflet_map <- leaflet(map_data) %>%
  addTiles() %>%
  
  # Ajouter les polygones pour RVA 2023
  addPolygons(
    group = "RVA 2023",
    fillColor = ~colorNumeric(palette = "Reds", domain = rva_2023_percent)(rva_2023_percent),
    weight = 0.2,
    color = "black",
    fillOpacity = 0.8,
    label = ~paste0(round(rva_2023_percent, 2), " %"),
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "font-size" = "12px"),
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE
    ),
    popup = ~paste0(
      "<b>Département :</b> ", nom, "<br>",
      "<b>RVA 2023 :</b> ", round(rva_2023_percent, 2), "%"
    )
  ) %>%
  
  # Ajouter les polygones pour RVA 2024
  addPolygons(
    group = "RVA 2024",
    fillColor = ~colorNumeric(palette = "Reds", domain = rva_2024_percent)(rva_2024_percent),
    weight = 0.2,
    color = "black",
    fillOpacity = 0.8,
    label = ~paste0(round(rva_2024_percent, 2), " %"),
    labelOptions = labelOptions(
      style = list("font-weight" = "bold", "font-size" = "12px"),
      noHide = TRUE,
      direction = "center",
      textOnly = TRUE
    ),
    popup = ~paste0(
      "<b>Département :</b> ", nom, "<br>",
      "<b>RVA 2024 :</b> ", round(rva_2024_percent, 2), "%"
    )
  ) %>%
  
  # Ajouter un contrôle pour basculer entre RVA 2023 et RVA 2024
  addLayersControl(
    baseGroups = c("RVA 2023", "RVA 2024"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  
  # Ajouter une légende dynamique
  addLegend(
    position = "bottomright",
    pal = colorNumeric(palette = "Reds", domain = map_data$rva_2023_percent),
    values = map_data$rva_2023_percent,
    title = "Pourcentage RVA 2023 (%)",
    group = "RVA 2023",
    labFormat = labelFormat(suffix = " %")
  ) %>%
  addLegend(
    position = "bottomright",
    pal = colorNumeric(palette = "Reds", domain = map_data$rva_2024_percent),
    values = map_data$rva_2024_percent,
    title = "Pourcentage RVA 2024 (%)",
    group = "RVA 2024",
    labFormat = labelFormat(suffix = " %")
  ) %>%
  
  # Ajouter un titre dynamique
  addControl(
    html = "<div id='dynamic-title' style='text-align:center; font-size:18px; font-weight:bold;'>
              Pourcentage de RVA en France Métropolitaine
            </div>",
    position = "topleft"
  )

# Sauvegarder la carte comme fichier HTML
saveWidget(leaflet_map, "C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Demandes/output/2024/11_2024/Carte_RVA/carte_pourcentage_rva_filtre.html", selfcontained = TRUE)

# Ouvrir la carte dans le navigateur
browseURL("C:/Users/erwan/OneDrive/Documents/Rstudio/Document personnel/Demandes/output/2024/11_2024/Carte_RVA/carte_pourcentage_rva_filtre.html")
