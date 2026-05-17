# import library need
library(shiny); library(shiny.i18n); library(bslib); library(thematic) # shiny and template
library(DBI); library(RPostgres); library(rpostgis) # database
library(leaflet); library(leaflet.extras); library(plotly); library(ggplot2) # map
library(sf); library(dplyr); library(raster); library(htmlwidgets) ; library(viridis)

thematic_shiny(font = "auto")

## frontend app UI
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  title = "Monitoring individual rice field",
  # Header
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$link(rel = "shortcut icon", type = "text/css", href = "favicon.ico")
  ),
  sidebar = sidebar(width = 300,
      title = "Filters and Map Control",
      open = T,
      selectInput('inp_anl_data', 'Information displayed', #Information affichée
                  c("Vegetated water"="ev", "Flooding"="flood"), #Eaux végétalisées #Inondation
                  selected = "flood"),
      selectInput('inp_var_saison', 'Primary indicators', #Indicateurs principaux
                  c("Inactive seasons"="inactive", #Inactivité de culture
                   "Proportion of rice fields with at least 2 active seasons"="prop_2_culture",
                   "Households close to rice fields (500m)"="household", #Ménages proches des rizières (500m)
                   "Primary season"="primary", 
                   "Secondary season"="secondary"), 
                  selected = "inactive"),
      uiOutput("slc_ditrib_season")
    
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Rice fields observed",
      value = scales::unit_format(unit = "Obs")(17321),
      showcase = bsicons::bs_icon("database"),
      theme = "primary"
    ),
    value_box(
      title = "Area of rice fields",
      value = scales::unit_format(unit = "ha")(13275),
      showcase = bsicons::bs_icon("graph-up"),
      theme = "primary"
    ),
    value_box(
      title = "Involvement in rice-growing activities",
      value = scales::unit_format(unit = "%", big.mark = ",")(86),
      showcase = bsicons::bs_icon("pie-chart-fill"),
      theme = "primary"
    )
  ),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Visualization"),
      leafletOutput("mapRiz", width="100%", height="100%"),
      uiOutput("uiAbsolutePanel")
    )
  )
  
)
