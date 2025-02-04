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

# Procesamiento de data ---------------------------------------------------
dataQuejas <- dataRaw

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
        1,
        echarts4rOutput("graf_1_evo", height = '350px', width = '550px'),
        echarts4rOutput("graf_2_grav", height = '350px', width = '550px')
      )
    )
  )
  
  )

server <- function(input, output){
  
  ## Data evolución
  data_evo <- reactive({
    
    data_gen_evo |> fsubset(direccion == input$dir)
    
  })
  
  data_grav <- reactive({
    
    data_gen_tipoQuej |> 
      fsubset(direccion == input$dir) |> 
      fselect(mes,gravedad,por) |> 
      pivot(c("mes"),"por","gravedad",how = "wider")
    
  })
  
  ## Renderizado de mapa como fondo
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
               attribution = paste(
                 "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                 "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>"
               ))  %>%
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
      e_tooltip(trigger = "axis")
      
  })
  
  output$graf_2_grav <- renderEcharts4r({
    
    data_grav() |> 
      fmutate(
        mes = as.character(mes),
        total = Baja + Media + Alta,  # Sumar total de cada mes
        Baja = Baja / total,          # Normalizar valores
        Media = Media / total,
        Alta = Alta / total
      ) |> 
      e_chart(x = mes) |> 
      e_bar(Baja, stack = "grp", name = "Baja") |> 
      e_bar(Media, stack = "grp", name = "Media") |> 
      e_bar(Alta, stack = "grp", name = "Alta") |> 
      e_tooltip(
        trigger = "axis",
        formatter = JS("function(params) {
          let tooltip = '<strong>' + params[0].axisValue + '</strong><br/>';  // Fecha en negritas
          params.forEach(function(item) {
            if (item.value !== null) {  // Asegura que el valor no sea null
              tooltip += item.marker + ' <strong>' + item.seriesName + '</strong>: ' 
                       + (item.value * 100).toFixed(1) + '%<br/>';
            }
          });
          return tooltip;
        }")
      ) |> 
      e_legend(bottom = 0) |> 
      e_y_axis(min = 0, max = 1)
    
  })
}

shinyApp(ui,server)
