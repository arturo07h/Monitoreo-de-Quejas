library(collapse)
library(data.table)
library(stringr)
library(tidyr)
library(leaflet)
library(echarts4r)
library(shiny)
library(bslib)
library(echarts4r)

options(encoding = "UTF-8")
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

dataRaw <- openxlsx2::read_xlsx("./data/quejas_clientes.xlsx") |> 
  tibble::as_tibble() |> janitor::clean_names()

shp_mexico <- sf::read_sf("insumos/México_Estados/mexico_drv.shp")

# Procesamiento de data ---------------------------------------------------
dataQuejas <- dataRaw

shp_mexico <- shp_mexico |> 
  fmutate(color = dplyr::case_when(
    str_detect(drv,"Pacifico") ~ "#023e8a",
    str_detect(drv,"Occidente") ~ "#d00000",
    str_detect(drv,"Noreste") ~ "#2b2d42",
    str_detect(drv,"Centro Norte") ~ "#3c6e71",
    str_detect(drv,"Centro") ~ "#582f0e",
    str_detect(drv,"Metro") ~ "#5c677d",
    str_detect(drv,"Peninsula") ~ "#004b23",
    str_detect(drv,"Sur") ~ "#463f3a",
    str_detect(drv,"Sureste") ~ "#563152",
    .default = NA
  )) 

## Vectores de filtros
## Direccipon
dir_vect <- funique(dataQuejas$direccion,sort = T)
dir_vect <- c("Nacional",dir_vect)

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
data_gen_evo <- rowbind(data1_evo,data2_evo) 

## Tipo de gravedad
data1_grav <- dataQuejas |> 
  fcount(mes,direccion,gravedad,sort = T) |> 
  fgroup_by(mes,direccion) |> 
  fmutate(por = N/fsum(N,na.rm = T)) |> 
  fungroup()

data2_grav <- data1_grav |> 
  fgroup_by(mes,gravedad) |> 
  fsummarise(N = fsum(N)) |> 
  fungroup() |> 
  fgroup_by(mes) |> 
  fmutate(por = N/fsum(N,na.rm = T)) |> 
  fungroup() |> 
  fmutate(direccion = "Nacional")

data_gen_grav <- rowbind(data1_grav,data2_grav)

## Tipo de queja
data1_tipoQuej <- dataQuejas |> 
  fcount(direccion,motivo_queja,sort = T) |> 
  fgroup_by(direccion) |> 
  fmutate(por = N/fsum(N,na.rm = T)) |> 
  fungroup() 

data2_tipoQuej <- data1_tipoQuej |> 
  fselect(direccion,motivo_queja,N) |> 
  fgroup_by(motivo_queja) |> 
  fsummarise(N = fsum(N,na.rm = T)) |> 
  fmutate(por = N / fsum(N,na.rm = T),
          direccion = "Nacional") 

data_gen_tipoQuej <- rowbind(data1_tipoQuej,data2_tipoQuej)

# Shiny -------------------------------------------------------------------
theme <- bs_theme(
  bg = "#000000", fg = "#B8BCC2",
  "input-border-color" = "#a6a6a6"
)


ui <- bootstrapPage(
  absolutePanel(
    top = 5,
    height = 5,
    right = 100,
    
    style = "z-index:500; text-align: right;",
    
    tags$h2("Monitoreo de Quejas")
  ),
  theme = theme,
  
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("map", width = "100%", height = "100%"),
  
  absolutePanel(
    top = 10, 
    left = 50, 
    
    fluidRow(
      column(
        4,
        selectInput("mes","Mes:",choices = vect_fechas,selected = max_Date)
      ),
      column(
        4,
        selectInput("dir","Dirección:",choices = dir_vect)
      )
    ),
    fluidRow(
      column(
        4,
        echarts4rOutput("graf_1_evo", height = '350px', width = '650px'),
        echarts4rOutput("graf_2_grav", height = '400px', width = '800px')
      )
    )
  )
)

server <- function(input, output){
  
  ## Data evolución
  data_evo <- reactive({
    
    data_gen_evo |> fsubset(direccion == input$dir)
    
  })
  
  data_tipQueja <- reactive({
    
    data_gen_tipoQuej |> fsubset(direccion == input$dir)
    
  })
  
  ## Renderizado de mapa como fondo
  output$map <- renderLeaflet({
    leaflet() |> 
      addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
               attribution = paste(
                 "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                 "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>"
               )) |> 
      addPolygons(
        data = shp_mexico,
        color = "red",
        fill = unique(shp_mexico$color),
        fillColor = unique(shp_mexico$color),
        fillOpacity = 0.5,
        weight = 2
        ) |> 
      setView(lat = 23.6345, lng = -102.5528, zoom = 5)
  })
  
  ## Gráficos
  
  ### Evolución de quejas
  
  output$graf_1_evo <- renderEcharts4r({
    
    tit <- paste0("Evolución mensual, ",funique(data_evo()$direccion))
    
    data_evo() |> 
      e_chart(x = mes) |> 
      e_area(serie = N) |> 
      e_legend(show = FALSE) |> 
      e_color(color = "#853c3c") |> 
      e_title(text = tit) |> 
      e_tooltip(trigger = "axis") |> 
      e_theme("inspired")
      
  })
  
  output$graf_2_grav <- renderEcharts4r({
    
    tit <- paste0("Principales motivos de queja acumulado, ",funique( data_tipQueja()$direccion))
    
    data_tipQueja() |> 
      dplyr::arrange(N) |> 
      dplyr::top_n(5,N) |> 
      fmutate(motivo_queja = str_wrap(motivo_queja, width = 10)) |>
      e_chart(x = motivo_queja) |> 
      e_bar(serie = N) |> 
      e_flip_coords() |> 
      e_legend(show = FALSE) |> 
      e_color(color = "#853c3c") |> 
      e_title(text = tit) |> 
      e_theme("inspired")
    
  })
}

shinyApp(ui,server)
