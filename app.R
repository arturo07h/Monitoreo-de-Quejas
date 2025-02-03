library(collapse)
library(data.table)
library(stringr)
library(tidyr)
library(leaflet)
library(echarts4r)
library(shiny)
library(bslib)

options(encoding = "UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

dataRaw <- openxlsx2::read_xlsx("./data/quejas_clientes.xlsx") |> 
  tibble::as_tibble() |> janitor::clean_names()

# Procesamiento de data ---------------------------------------------------
dataQuejas <- dataRaw

## Vectores de filtros
## Direccipon
dir_vect <- funique(dataQuejas$direccion,sort = T)

### Fechas
max_Date <- format(fmax(dataQuejas$mes),"%B %Y")
vect_fechas <- format(funique(dataQuejas$mes,sort = T),"%B %Y")

## Evolución Nacional
data1_evo <- dataQuejas |> 
  fcount(mes,sort = T) |> 
  fmutate(direccion = "Nacional")

data2_evo <- dataQuejas |> 
  fcount(direccion,mes,sort = T)

### Data a lista para visualizar
data_gen_evo <- rowbind(data1_evo,data2_evo) |> 
  fcount(direccion) 

## Tipo de gravedad
data1_grav <- dataQuejas |> 
  fcount(mes,gravedad,sort = T) |> 
  fgroup_by(mes) |> 
  fmutate(por = N/fsum(N,na.rm = T)) |> 
  fungroup()

## Tipo de queja
data1_tipoQuej <- dataQuejas |> 
  fcount(mes,motivo_queja,sort = T) |> 
  fgroup_by(mes) |> 
  fmutate(por = N/fsum(N,na.rm = T)) |> 
  fmutate(rank = frank(-por,ties.method = "first")) |> 
  fungroup() 

theme <- bs_theme(
  bg = "#000000", fg = "#B8BCC2",
  "input-border-color" = "#a6a6a6"
)

## Estructura de SHiny
ui <- bootstrapPage(
  absolutePanel(
    top = 10, left = 10, style = "z-index:500; text-align: right;",
    tags$h2("Monitoreo de Quejas")
  ),
  theme = theme,
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(
    top = 5,
    height = 5,
    # left = -1,
    right = -300,
    fluidRow(
      column(
        3,
        selectInput("mes","Mes:",choices = vect_fechas,selected = max_Date)
      ),
      column(
        3,
        selectInput("dir","Dirección:",choices = dir_vect)
      )
    )
  )
  
  )

server <- function(input, output){
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
               attribution = paste(
                 "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                 "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>"
               ))  %>%
      setView(lat = 23.6345, lng = -102.5528, zoom = 5)
  })
}

shinyApp(ui,server)
