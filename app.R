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

## Relacion DRV, SDRV y OS
rel_ubic <- dataQuejas |> fcount(direccion,subdireccion,sucursal,latitud,longitud,sort = T) |> 
  fselect(-N)

shp_mexico <- shp_mexico |> 
  fmutate(drv = drv |> 
            str_replace_all(c("Peninsula" = "Península")) |> 
            str_remove_all("Direccion") |> 
            str_squish())

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

## Info por Sucursal
data3_info_suc <- dataQuejas |> 
  fcount(sucursal,mes,gravedad,sort = T) |> 
  fgroup_by(sucursal,mes) |> 
  fmutate(suma = fsum(N)) |> 
  fungroup() |> 
  fmutate(por = N / suma)


data3_info_suc <- data3_info_suc |> 
  complete(sucursal, mes, gravedad) %>%
  dplyr::mutate(dplyr::across(c(N, suma, por), ~ dplyr::case_when(
    is.na(.) ~ 0, 
    TRUE ~ .)))

data_gen_infoSuc <- join(x = data3_info_suc,y = rel_ubic,on = "sucursal",how = "left") |> 
  fmutate(fecha = format(mes,"%B %Y"))

# Shiny -------------------------------------------------------------------
theme <- bs_theme(
  bg = "#000000", fg = "#B8BCC2",
  "input-border-color" = "#a6a6a6"
)

cards_graf <- list(
  card(
    full_screen = F,
    card_body(
      echarts4rOutput("graf_1_evo",height = '50px', width = '500px')
    )
  ),
  card(
    full_screen = T,
    card_body(
      echarts4rOutput("graf_2_grav",height = '50px', width = '500px')
    )
  ),
  card(
    full_screen = T,
    card_body(
      leafletOutput("map",width = "100%",height = "50%"),
    )
  )
)

## Ui
ui <- page_sidebar(
  
  title = "Monitoreo de quejas",
  
  theme = theme,
  
  sidebar = sidebar(
   
     title = "Control de regiones y fechas",
    
    selectInput("dir", "Dirección:", width = 180, choices = dir_vect,selected = "Nacional"),
    selectInput("fecha", "Mes de consulta:", width = 180, choices = vect_fechas,selected = "enero 2025")
    
  ),
  
  layout_columns(cards_graf[[1]],cards_graf[[2]]),
  layout_columns(cards_graf[[3]])
)

## Server
server <- function(input, output){
  
  # Data evolución
  data_evo <- reactive({
    data_gen_evo |> fsubset(direccion == input$dir)
  })
  ## Data tipo de queja
  data_tipQueja <- reactive({
    data_gen_tipoQuej |> fsubset(direccion == input$dir)
  })
  
  ## Data por sucursal
  data_suc <- reactive({
    data_gen_infoSuc |> fsubset(direccion == input$dir) |> 
      fsubset(fecha == input$fecha)
  })

  ## SHP reactivo
  shp_reactivo <- reactive({
    if(input$dir == "Nacional"){
      shp_mexico
    }else{
      shp_mexico |> fsubset(drv == input$dir)
    }
  })
  
  ## Renderizado de mapa como fondo
  output$map <- renderLeaflet({
    
    if (input$dir == "Nacional") {return}
    
    leaflet() |> 
      addTiles("http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
               attribution = paste(
                 "&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors",
                 "&copy; <a href=\"http://cartodb.com/attributions\">CartoDB</a>")
               ) |> 
      addPolygons(
        data = shp_reactivo(),
        color = "red",
        fillOpacity = 0.5,
        weight = 2
        ) 
  })
  
  observeEvent(input$dir,{
    
    if (input$dir == "Nacional") {return()}

    ## Polígonos
    zona_lat <- sf::st_coordinates(shp_reactivo()) |> as_tibble()
    
    flng1 <- fmin(zona_lat$X)
    flng2 <- fmax(zona_lat$X)
    
    flat1 <- fmin(zona_lat$Y)
    flat2 <- fmax(zona_lat$Y)
  
    # browser()
    data_suc_resumen <- data_suc() %>%
      fgroup_by(sucursal, fecha, longitud, latitud) %>%
      fsummarise(
        suma_total = sum(N, na.rm = TRUE),
        Alta = scales::percent(sum(por[gravedad == "Alta"], na.rm = TRUE), accuracy = 1),
        Media = scales::percent(sum(por[gravedad == "Media"], na.rm = TRUE), accuracy = 1),
        Baja = scales::percent(sum(por[gravedad == "Baja"], na.rm = TRUE), accuracy = 1)) |> 
      fungroup()
    
    leafletProxy("map") |> 
      clearShapes() |> 
      addPolygons(data = shp_reactivo(),
                  color = "red",
                  label = ~ ESTADO,
                  fillOpacity = 0.5,
                  weight = 2,
                  popup = ~ paste("Estado:", ESTADO)) |> 
      addMarkers(
        data = data_suc_resumen,
        lng = ~longitud,
        lat = ~latitud,
        popup = ~paste0(
          "<b>Sucursal: </b>", sucursal, "<br>",
          "<b>Mes de consulta: </b>", fecha, "<br>",
          "<b>Total de quejas mensuales: </b>", suma_total, "<br>",
          "<b>Gravedad</b><br>",
          fifelse(Alta != "0%", paste0("<b>Alta: </b>", Alta, "<br>"), paste0("<b>Alta: </b>","0%", "<br>")),
          fifelse(Media != "0%", paste0("<b>Media: </b>", Media, "<br>"), paste0("<b>Media: </b>","0%", "<br>")),
          fifelse(Baja != "0%", paste0("<b>Baja: </b>", Baja), paste0("<b>Baja: </b>","0%", "<br>"))
        )) |> 
      flyToBounds(
        lng1 = flng1,
        lng2 = flng2,
        lat1 = flat1,
        lat2 = flat2
      )
  })
  
  ## Gráficos
  
  ### Evolución de quejas
  
  output$graf_1_evo <- renderEcharts4r({

    tit <- paste0("Evolución mensual, ",funique(data_evo()$direccion))

    data_evo() |>
      e_chart(x = mes) |>
      e_area(serie = N) |>
      e_legend(show = FALSE) |>
      e_color(color = "#606c38") |>
      e_title(text = tit) |>
      e_tooltip(trigger = "axis") |>
      e_theme("cool")|> 
      e_datazoom(
        type = "slider",
        startValue = "2024-01-01"
      )

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
      e_color(color = "#606c38") |>
      e_title(text = tit) |>
      e_theme("cool")

  })
}
"#ff715e"
"#ffaf51"
shinyApp(ui,server)
